import {
  App,
  Modal,
  Notice,
  Plugin,
  PluginSettingTab,
  Setting,
  TAbstractFile,
  TFile,
  TFolder,
  addIcon,
  moment,
  normalizePath } from 'obsidian';
import { CardBoardView, VIEW_TYPE_CARD_BOARD } from './view';
import { CardBoardPluginSettings, CardBoardPluginSettingsPostV11, TaskItem } from './types';
import type { Flags } from '../src/Worker';
import workerSource from '../src/Worker.elm?worker-source';
import { renameOrderPaths } from './cardOrder';
import { ParserWorker } from './parserWorker';
import { getDateFromFile, IPeriodicNoteSettings } from 'obsidian-daily-notes-interface';

export default class CardBoardPlugin extends Plugin {
  private commandIds: string[] = [];
  private worker: ParserWorker;
  private layoutReady = false;
  private viewRequested = false;
  private indexStarted = false;
  private indexReady = false;
  private disposed = false;
  private indexPromise: Promise<void>;
  private snapshotReady: (() => void) | null = null;
  private dirty = new Set<string>();
  private changeTimer: ReturnType<typeof setTimeout>;
  private flushing: Promise<void> | null = null;
  settings:           CardBoardPluginSettings;

  async onload() {
    console.log('CardBoard: loading plugin');

    await this.loadSettings();

    this.registerView(
      VIEW_TYPE_CARD_BOARD,
      (leaf) => new CardBoardView(this, leaf)
    );

    this.app.workspace.onLayoutReady(this.onLayoutReady.bind(this));
  }

  async onLayoutReady() {
    if (this.disposed || this.layoutReady) return;
    this.layoutReady = true;
    this.registerEvent(this.app.vault.on('create', file => this.handleFileCreated(file)));
    this.registerEvent(this.app.vault.on('delete', file => this.handleFileDeleted(file)));
    this.registerEvent(this.app.vault.on('modify', file => this.handleFileModified(file)));
    this.registerEvent(this.app.vault.on('rename', (file, oldPath) => this.handleFileRenamed(file, oldPath)));
    this.installControls();
    if (this.viewRequested) this.startIndex();
  }

  private startIndex() {
    if (!this.layoutReady || this.indexStarted || this.disposed) return;
    this.indexStarted = true;
    // @ts-ignore
    const dataviewSettings = this.app.plugins.getPlugin("dataview")?.settings

    const workerFlags:Flags = {
      dataviewTaskCompletion:   {
        taskCompletionTracking:           dataviewSettings  === undefined ? true          : dataviewSettings['taskCompletionTracking'],
        taskCompletionUseEmojiShorthand:  dataviewSettings  === undefined ? false         : dataviewSettings['taskCompletionUseEmojiShorthand'],
        taskCompletionText:               dataviewSettings  === undefined ? "completion"  : dataviewSettings['taskCompletionText']
      }
    };

    try {
      this.worker = new ParserWorker(workerSource, workerFlags, error => {
        this.indexReady = false;
        this.snapshotReady?.(); this.snapshotReady = null;
        new Notice('CardBoard task parser: ' + error.message);
      });
    } catch (error) {
      this.indexStarted = false;
      new Notice('Could not start CardBoard background parser: ' + String(error));
      return;
    }

    const that = this;

    this.worker.ports.interopFromElm.subscribe((fromElm) => {
      switch (fromElm.tag) {
        case "allTaskItems":
          that.handleAllTaskItems(fromElm.data);
          break;
        case "allTasksLoaded":
          that.handleAllTasksLoaded();
          break;
        case "tasksAdded":
          that.handleTasksAdded(fromElm.data);
          break;
        case "tasksDeleted":
          that.handleTasksDeleted(fromElm.data);
          break;
        case "tasksDeletedAndAdded":
          that.handleTasksDeletedAndAdded(fromElm.data);
          break;
        case "tasksUpdated":
          that.handleTasksUpdated(fromElm.data);
          break;
      }
    });

    this.indexPromise = this.scanVault().catch(error => {
      if (!this.disposed) { this.worker?.terminate(); this.indexStarted = false; this.indexReady = false; this.snapshotReady?.(); this.snapshotReady = null; new Notice('CardBoard indexing: ' + String(error)); }
    });
  }

  private async scanVault() {
    const firstSnapshot = new Promise<void>(resolve => { this.snapshotReady = resolve; });
    let sliceStart = performance.now();
    let queued = 0;
    const files = this.app.vault.getMarkdownFiles().map(file => ({ path: file.path, file }));
    for (const { path, file } of files) {
      if (this.disposed) return;
      if (this.app.vault.getAbstractFileByPath(path) !== file) continue;
      try {
        const text = await this.app.vault.cachedRead(file);
        if (this.disposed) return;
        if (!this.dirty.has(path) && /(?:^|\n)[-*+] \[[ xX]\] /.test(text)) {
          this.sendFile('fileAdded', file, text); queued++;
        }
      } catch (error) {
        if (this.app.vault.getAbstractFileByPath(path) === file) this.dirty.add(path);
      }
      if (queued >= 32 || performance.now() - sliceStart >= 8) {
        if (queued > 0) await this.worker.flush();
        await new Promise(resolve => setTimeout(resolve, 0));
        sliceStart = performance.now(); queued = 0;
      }
    }
    if (this.disposed) return;
    await this.flushChanges();
    if (this.disposed) return;
    this.worker.ports.interopToElm.send({ tag: 'allMarkdownLoaded', data: {} });
    await this.worker.flush();
    await firstSnapshot;
  }

  private sendFile(tag: 'fileAdded' | 'fileModified', file: TFile, text: string) {
    this.worker.ports.interopToElm.send({ tag, data: { filePath: file.path,
      fileDate: this.formattedFileDate(file), currentYear: new Date().getFullYear(),
      fileContents: /(?:^|\n)[-*+] \[[ xX]\] /.test(text) ? text : '' } });
  }

  private queueFile(path: string) {
    if (!this.indexStarted || this.disposed) return;
    this.dirty.add(path);
    if (!this.indexReady) return;
    clearTimeout(this.changeTimer);
    this.changeTimer = setTimeout(() => { void this.flushChanges().catch(error => {
      if (!this.disposed) new Notice('CardBoard update: ' + String(error));
    }); }, 100);
  }

  private flushChanges(): Promise<void> {
    if (this.flushing) return this.flushing;
    this.flushing = (async () => {
      let count = 0;
      while (this.dirty.size && !this.disposed) {
        const path = this.dirty.values().next().value as string;
        this.dirty.delete(path);
        const file = this.app.vault.getAbstractFileByPath(path);
        if (file instanceof TFile && file.extension === 'md') {
          let text: string;
          try { text = await this.app.vault.read(file); }
          catch (error) {
            if (this.disposed) return;
            if (this.app.vault.getAbstractFileByPath(path) !== file) this.dirty.add(path);
            else new Notice('CardBoard could not read ' + path + ': ' + String(error));
            continue;
          }
          if (this.disposed) return;
          if (this.dirty.has(path) || this.app.vault.getAbstractFileByPath(path) !== file) continue;
          this.sendFile('fileModified', file, text);
        } else {
          this.worker.ports.interopToElm.send({ tag: 'fileDeleted', data: path });
        }
        if (++count % 32 === 0) { await this.worker.flush(); await new Promise(resolve => setTimeout(resolve, 0)); }
      }
      if (count > 0 && !this.disposed) await this.worker.flush();
    })().finally(() => { this.flushing = null; });
    return this.flushing;
  }


  async handleAllTaskItems(taskItems: TaskItem[]) {
    this.snapshotReady?.(); this.snapshotReady = null;
    console.debug("CardBoard: [main] fromWorker -> allTasksItems");

    const leaves = this.app.workspace.getLeavesOfType(VIEW_TYPE_CARD_BOARD);

    for (const leaf of leaves) {
      if (leaf.view instanceof CardBoardView) {
        leaf.view.taskItemsRefreshed(taskItems);
      }
    }
  }

  async handleAllTasksLoaded() {
    this.indexReady = true;
    this.worker.ports.interopToElm.send({ tag: 'viewInitialized' });
    if (this.dirty.size) this.queueFile(this.dirty.values().next().value as string);
  }

  private installControls() {

    addIcon("card-board",
      '<rect x="2" y="2" width="96" height="96" rx="12" ry="12" fill="none" stroke="currentColor" stroke-width="5"></rect>' +
      '<rect x="28" y="28" width="12" height="46" fill="none" stroke="currentColor" stroke-width="5"></rect>' +
      '<rect x="56" y="28" width="12" height="30" fill="none" stroke="currentColor" stroke-width="5"></rect>');

    this.addRibbonIcon('card-board', 'CardBoard', async () => {
      this.activateView(0);
    });

    this.addCommands();
  }

  async viewInitialized() {
    this.viewRequested = true;
    if (this.indexReady) this.worker.ports.interopToElm.send({ tag: 'viewInitialized' });
    else this.startIndex();
  }


  async handleTasksAdded(taskItems : TaskItem[]) {
    const leaves = this.app.workspace.getLeavesOfType(VIEW_TYPE_CARD_BOARD);

    for (const leaf of leaves) {
      if (leaf.view instanceof CardBoardView) {
        leaf.view.taskItemsAdded(taskItems);
      }
    }
  }

  async handleTasksDeleted(taskIds : string[]) {
    console.debug("CardBoard: [main] fromWorker -> tasksDeleted");

    const leaves = this.app.workspace.getLeavesOfType(VIEW_TYPE_CARD_BOARD);

    for (const leaf of leaves) {
      if (leaf.view instanceof CardBoardView) {
        leaf.view.taskItemsDeleted(taskIds);
      }
    }
  }

  async handleTasksDeletedAndAdded(toDeleteAndAdd : [string[], TaskItem[]]) {
    console.debug("CardBoard: [main] fromWorker -> tasksDeletedAndAdded: (" + toDeleteAndAdd[0].length + ", " + toDeleteAndAdd[1].length + ")");

    const leaves = this.app.workspace.getLeavesOfType(VIEW_TYPE_CARD_BOARD);

    for (const leaf of leaves) {
      if (leaf.view instanceof CardBoardView) {
        leaf.view.taskItemsDeletedAndAdded(toDeleteAndAdd);
      }
    }
  }

  async handleTasksUpdated(updateDetails : [string, TaskItem][]) {
    console.debug("CardBoard: [main] fromWorker -> tasksUpdated: " + updateDetails.length);

    const leaves = this.app.workspace.getLeavesOfType(VIEW_TYPE_CARD_BOARD);

    for (const leaf of leaves) {
      if (leaf.view instanceof CardBoardView) {
        leaf.view.taskItemsUpdated(updateDetails);
      }
    }
  }

  onunload() {
    this.disposed = true;
    clearTimeout(this.changeTimer);
    this.worker?.terminate();
    this.snapshotReady?.(); this.snapshotReady = null;
    console.log('unloading CardBoard plugin');
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_CARD_BOARD);
  }


  addCommands() {
    this.settings?.data?.boardConfigs?.forEach((boardConfig, index) => {
      const config : any = boardConfig;
      var boardName : string;

      if (config.hasOwnProperty('data')) {
        boardName = config.data.title;
      } else {
        boardName = config.name;
      }

      const command = this.addCommand({
        id: "open-card-board-plugin-" + index,
        name: "Open " + boardName,
        callback: async () => {
          this.activateView(index);
        },
      });

      this.commandIds.push(command.id);
    });
  }


  removeCommands() {
    for (const commandId of this.commandIds) {
      // @ts-ignore
      this.app.commands.removeCommand(commandId);
    }
    this.commandIds = [];
  }

  async activateView(index: number) {
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_CARD_BOARD);

    await this.app.workspace.getLeaf(true).setViewState({
      type: VIEW_TYPE_CARD_BOARD,
      active: true,
    });

    const leaf = this.app.workspace.getLeavesOfType(VIEW_TYPE_CARD_BOARD)[0];

    if (leaf.view instanceof CardBoardView) {
      leaf.view.currentBoardIndex(index);
    }

    this.app.workspace.revealLeaf(leaf);
  }

  async deactivateView() {
    this.app.workspace.detachLeavesOfType(VIEW_TYPE_CARD_BOARD);
  }

  async loadSettings() {
    this.settings = await this.loadData();
  }

  async saveSettings( newSettings: CardBoardPluginSettingsPostV11) {
    if (JSON.stringify(this.settings) === JSON.stringify(newSettings)) return;
    await this.backupOldVersion(this.settings?.version, newSettings.version);

    const previousBoards = JSON.stringify(this.settings?.data?.boardConfigs?.map(config => (config as any).name || (config as any).data?.title));
    this.settings = newSettings;
    const nextBoards = JSON.stringify(newSettings.data.boardConfigs.map(config => config.name));
    if (previousBoards !== nextBoards) { this.removeCommands(); this.addCommands(); }
    await this.saveData(newSettings);
  }

  async backupOldVersion(oldVersion: string | null, newVersion: string) {
    if (oldVersion && (oldVersion != newVersion)) {
      const pathToSettings = normalizePath(this.app.vault.configDir + "/plugins/card-board/data.json");
      const pathToSavedSettings = normalizePath(this.app.vault.configDir + "/plugins/card-board/data." + oldVersion + ".json");

      if (await this.app.vault.adapter.exists(pathToSavedSettings)) {
        await this.app.vault.adapter.remove(pathToSavedSettings);
      }
      this.app.vault.adapter.copy(pathToSettings, pathToSavedSettings);
    }
  }

  async handleFileCreated(file: TAbstractFile) {
    if (file instanceof TFile && file.extension === 'md') this.queueFile(file.path);
  }

  async handleFileDeleted(file: TAbstractFile) {
    if (file instanceof TFile) this.queueFile(file.path);
  }

  async handleFileModified(file: TAbstractFile) {
    if (file instanceof TFile && file.extension === 'md') this.queueFile(file.path);
  }

  async handleFileRenamed(file: TAbstractFile, oldPath: string) {
    const settings = this.settings as CardBoardPluginSettingsPostV11;
    const orders = settings?.data?.globalSettings?.cardOrder;
    if (orders?.length) {
      const cardOrder = renameOrderPaths(orders, oldPath, file.path);
      if (JSON.stringify(cardOrder) !== JSON.stringify(orders)) {
        const next = { ...settings, data: { ...settings.data, globalSettings: { ...settings.data.globalSettings, cardOrder } } };
        await this.saveSettings(next);
        for (const leaf of this.app.workspace.getLeavesOfType(VIEW_TYPE_CARD_BOARD)) {
          if (leaf.view instanceof CardBoardView) leaf.view.settingsUpdated(next);
        }
      }
    }
    this.queueFile(oldPath);
    if (file instanceof TFile && file.extension === 'md') this.queueFile(file.path);
    if (file instanceof TFolder) {
      for (const note of this.app.vault.getMarkdownFiles()) {
        if (note.path.startsWith(file.path + '/')) {
          this.queueFile(oldPath + note.path.slice(file.path.length)); this.queueFile(note.path);
        }
      }
    }
  }

  // HELPERS

  formattedFileDate(
    file: TFile
  ): string | null {
    return getDateFromFile(file, "day")?.format('YYYY-MM-DD') || null;
  }
}
