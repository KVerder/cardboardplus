import { ParserWorker } from '../../typescript/parserWorker';
import { CardDrag } from '../../typescript/cardDrag';
import { BoardActions } from '../../typescript/boardActions';
import { FileFilter } from '../../typescript/fileFilter';
import { applyTaskReplacements, TaskReplacement } from '../../typescript/cardEditing';
import { completionDefaultHours, isKillianTask, needsCompletionDuration, withCompletionDuration } from '../../typescript/completionDuration';
import { askCompletionDuration } from '../../typescript/completionDurationModal';
import { TFile } from 'obsidian';
import * as obsidian from 'obsidian';
const w = window as any;
w.fixtureNotices = [];
new CardDrag(document.body);
const file = new (TFile as any)('Team.md');
const publish = () => w.testWorker.ports.interopToElm.send({ tag: 'fileModified', data: { filePath: file.path, fileDate: null, currentYear: 2026, fileContents: w.testMarkdown } });
const app: any = { metadataCache: { getFileCache: () => ({}) }, vault: {
  getAbstractFileByPath: (path: string) => path === file.path ? file : null,
  read: async () => w.testMarkdown,
  process: async (_file: any, fn: any) => { w.testMarkdown = fn(w.testMarkdown); publish(); }
} };
const actions = new BoardActions(app, () => w.testSettings, new FileFilter([]), async settings => {
  w.testSettings = settings; w.testApp.ports.interopToElm.send({ tag: 'settingsUpdated', data: settings });
});
w.testApp.ports.interopFromElm.subscribe(async (message: any) => {
  if (message.tag === 'assignCard') actions.assignCard(message.data);
  if (message.tag === 'reorderCard') await actions.reorderCard(message.data);
  if (message.tag === 'updateTasks') {
    try {
      applyTaskReplacements(w.testMarkdown, message.data.tasks);
      const tasks: TaskReplacement[] = [];
      for (const task of message.data.tasks) tasks.push(needsCompletionDuration(task) ? withCompletionDuration(task, await askCompletionDuration(app, task.originalLine, completionDefaultHours(task), isKillianTask(task))) : task);
      w.testMarkdown = applyTaskReplacements(w.testMarkdown, tasks); publish();
    } catch (error) { w.fixtureNotices.push(String(error)); }
  }
});
w.fixturePublish = publish;
w.fixtureReady = true;

w.createBackgroundParser = (source: string, flags: any) => new ParserWorker(source, flags, error => w.fixtureNotices.push(error.message));

w.runProductionSmoke = async (source: string) => {
  const settings: any = { version: '0.13.0', data: { globalSettings: { filters: [] }, boardConfigs: [] } };
  // Keep the host class native even if the plugin compiler target regresses.
  const Plugin = new Function('settings', `return class Plugin {
    async loadData() { return settings; }
    registerView() {} registerEvent() {} addRibbonIcon() {}
    addCommand(command) { return command; }
  }`)(settings);
  const api: any = { ...obsidian, Plugin, ItemView: class {}, MarkdownView: class {}, addIcon() {} };
  const module = { exports: {} as any };
  new Function('module','exports','require',source)(module,module.exports,(name: string) => {
    if(name === 'obsidian')return api;throw new Error('Unexpected external: '+name);
  });
  const plugin = new module.exports.default();
  const files = Array.from({length:32},(_,index) => new (TFile as any)('Production'+index+'.md'));
  let reads = 0;let count = 0;let beats = 0;
  plugin.app = { plugins: { getPlugin: (): undefined => undefined }, vault: { on: () => ({}),
    getMarkdownFiles: () => files, getAbstractFileByPath: (path: string) => files.find(file => file.path === path),
    cachedRead: async () => { reads++; return '- [ ] Work @takes(4h) @assigned(Killian)'; },
    read: async () => '- [ ] Latest'
  }, workspace: { onLayoutReady() {}, getLeavesOfType: (): any[] => [], detachLeavesOfType() {} } };
  plugin.formattedFileDate = (): null => null;
  const handleSnapshot = plugin.handleAllTaskItems.bind(plugin);
  plugin.handleAllTaskItems = (tasks: any[]) => { count = tasks.length; return handleSnapshot(tasks); };
  await plugin.onload(); await plugin.onLayoutReady();
  const startupReads = reads;
  const timer = setInterval(() => beats++, 5);
  await plugin.viewInitialized(); await plugin.indexPromise;
  clearInterval(timer); plugin.onunload();
  return { startupReads, reads, count, beats };
};
