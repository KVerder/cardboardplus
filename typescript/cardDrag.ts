const HOLD_MS = 450;
const MOVE_THRESHOLD = 6;
const BORDER_ZONE = 6;

/** A touch hold or a mouse drag from the card perimeter starts reordering. */
export class CardDrag {
  private active: { pointer: number; card: HTMLElement; source: HTMLElement; x: number; y: number; dragging: boolean; touch: boolean } | null = null;
  private holdTimer: ReturnType<typeof setTimeout> | undefined;
  private suppressClickUntil = 0;
  private target: HTMLElement | null = null;
  private before: HTMLElement | null = null;
  private frame = 0;
  private x = 0;
  private y = 0;

  constructor(private root: HTMLElement) {
    root.addEventListener('pointerdown', this.down);
    root.addEventListener('touchmove', this.touchMove, { passive: false });
    root.addEventListener('contextmenu', this.suppressNative, true);
    root.addEventListener('selectstart', this.suppressNative, true);
    root.addEventListener('click', this.click, true);
    root.addEventListener('pointermove', this.move);
    root.addEventListener('pointerup', this.up);
    root.addEventListener('pointercancel', this.cancel);
    root.addEventListener('lostpointercapture', this.cancel);
  }

  private down = (event: PointerEvent) => {
    if (!event.isPrimary || event.button !== 0 || this.active) return;
    this.suppressClickUntil = 0;
    const element = event.target as Element;
    const card = element.closest<HTMLElement>('[data-task-id]');
    const source = card?.closest<HTMLElement>('[data-card-column-index]');
    if (!card || !source || element.closest('a, button, input, textarea, select, [contenteditable]')) return;

    const touch = event.pointerType === 'touch';
    if (!touch) {
      const rect = card.getBoundingClientRect();
      const edge = Math.min(event.clientX - rect.left, rect.right - event.clientX,
        event.clientY - rect.top, rect.bottom - event.clientY);
      if (edge > BORDER_ZONE) return;
      event.preventDefault();
    }
    this.active = { pointer: event.pointerId, card, source, x: event.clientX, y: event.clientY, dragging: false, touch };
    this.x = event.clientX;
    this.y = event.clientY;
    this.root.setPointerCapture(event.pointerId);
    if (touch) this.holdTimer = setTimeout(() => this.begin(), HOLD_MS);
  };

  private begin() {
    const active = this.active;
    if (!active || !active.card.isConnected) { this.clear(); return; }
    active.dragging = true;
    active.card.classList.add('card-board-pointer-dragging');
    this.locate();
    if (!this.frame) this.frame = requestAnimationFrame(this.scroll);
  }

  private move = (event: PointerEvent) => {
    const active = this.active;
    if (!active || event.pointerId !== active.pointer) return;
    this.x = event.clientX;
    this.y = event.clientY;
    if (active.dragging) {
      event.preventDefault();
      this.locate();
    } else if (Math.hypot(this.x - active.x, this.y - active.y) >= MOVE_THRESHOLD) {
      if (active.touch) this.clear();
      else this.begin();
    }
  };

  // Keep native scrolling until the hold is recognized.
  private touchMove = (event: TouchEvent) => {
    if (this.active?.touch && this.active.dragging) event.preventDefault();
  };

  private suppressNative = (event: Event) => {
    if (this.active?.touch) {
      event.preventDefault();
      event.stopPropagation();
    }
  };

  private click = (event: MouseEvent) => {
    if (event.detail && performance.now() < this.suppressClickUntil) {
      event.preventDefault();
      event.stopPropagation();
      this.suppressClickUntil = 0;
    }
  };

  private locate() {
    this.before?.classList.remove('card-board-drop-before');
    this.target?.classList.remove('card-board-drop-target');
    this.target = null; this.before = null;
    const active = this.active;
    if (!active) return;
    const target = this.root.ownerDocument.elementFromPoint(this.x, this.y)?.closest<HTMLElement>('[data-card-column-index]');
    if (!target || !this.root.contains(target) || (target !== active.source && target.dataset.cardCanMove !== 'true')) return;
    this.target = target;
    const cards = Array.from(target.querySelectorAll<HTMLElement>('[data-task-id]')).filter(card => card !== active.card);
    this.before = cards.find(card => { const rect = card.getBoundingClientRect(); return this.y < rect.top + rect.height / 2; }) || null;
    target.classList.add('card-board-drop-target');
    this.before?.classList.add('card-board-drop-before');
  }

  private scroll = () => {
    this.frame = 0;
    if (!this.active?.dragging) return;
    const column = this.target?.querySelector<HTMLElement>('.card-board-column-list');
    const board = this.active.card.closest<HTMLElement>('.card-board-column-container');
    for (const element of [column, board]) {
      if (!element) continue;
      const rect = element.getBoundingClientRect();
      const edge = (position: number, min: number, max: number) => position < min + 40 ? -12 : position > max - 40 ? 12 : 0;
      element.scrollBy(element === board ? edge(this.x, rect.left, rect.right) : 0, element === column ? edge(this.y, rect.top, rect.bottom) : 0);
    }
    this.locate();
    this.frame = requestAnimationFrame(this.scroll);
  };

  private up = (event: PointerEvent) => {
    const active = this.active;
    if (!active || event.pointerId !== active.pointer) return;
    this.x = event.clientX; this.y = event.clientY;
    if (active.dragging) this.locate();
    const target = this.target;
    const detail = target && active.dragging ? { taskId: active.card.dataset.taskId,
      sourceColumnIndex: Number(active.source.dataset.cardColumnIndex), columnIndex: Number(target.dataset.cardColumnIndex),
      beforeTaskId: this.before?.dataset.taskId || null } : null;
    if (active.dragging) this.suppressClickUntil = performance.now() + 400;
    this.clear();
    if (target && detail) target.dispatchEvent(new CustomEvent('cardboard-drop', { bubbles: true, detail }));
  };

  private cancel = (event: PointerEvent) => {
    if (this.active?.pointer === event.pointerId) this.clear();
  };

  private clear() {
    clearTimeout(this.holdTimer);
    this.holdTimer = undefined;
    const pointer = this.active?.pointer;
    this.active?.card.classList.remove('card-board-pointer-dragging');
    this.before?.classList.remove('card-board-drop-before');
    this.target?.classList.remove('card-board-drop-target');
    this.active = null; this.target = null; this.before = null;
    cancelAnimationFrame(this.frame); this.frame = 0;
    if (pointer !== undefined && this.root.hasPointerCapture(pointer)) this.root.releasePointerCapture(pointer);
  }

  destroy() {
    this.clear();
    this.root.removeEventListener('pointerdown', this.down);
    this.root.removeEventListener('touchmove', this.touchMove);
    this.root.removeEventListener('contextmenu', this.suppressNative, true);
    this.root.removeEventListener('selectstart', this.suppressNative, true);
    this.root.removeEventListener('click', this.click, true);
    this.root.removeEventListener('pointermove', this.move);
    this.root.removeEventListener('pointerup', this.up);
    this.root.removeEventListener('pointercancel', this.cancel);
    this.root.removeEventListener('lostpointercapture', this.cancel);
  }
}
