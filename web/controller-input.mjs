export const BUTTONS = ['up', 'down', 'left', 'right', 'a', 'b', 'start', 'select'];
export const ACTION_BUTTON = {up:'up', down:'down', left:'left', right:'right', btn_a:'a', btn_b:'b', start:'start', select:'select'};

// Standard Gamepad layout: east is GB A, south is GB B.
export function gamepadButtons(pad) {
  const pressed = index => !!pad.buttons?.[index]?.pressed;
  const x = pad.axes?.[0] || 0, y = pad.axes?.[1] || 0;
  return new Set(BUTTONS.filter(button => ({
    up: pressed(12) || y < -0.5, down: pressed(13) || y > 0.5,
    left: pressed(14) || x < -0.5, right: pressed(15) || x > 0.5,
    a: pressed(1), b: pressed(0), start: pressed(9), select: pressed(8),
  })[button]));
}

export class ControllerInputs {
  constructor(send, assignmentsChanged = () => {}) {
    this.send = send;
    this.assignmentsChanged = assignmentsChanged;
    this.sources = new Map();
    this.blocked = new Set();
    this.output = new Map();
    this.pads = new Map();
    this.suspended = false;
  }
  set(source, port, button, pressed) {
    if (!BUTTONS.includes(button) || port < 0 || port > 3) return;
    const key = `${source}:${port}:${button}`;
    if (pressed) {
      this.sources.set(key, {port, button});
      if (this.suspended) this.blocked.add(key);
    } else {
      this.sources.delete(key);
      this.blocked.delete(key);
    }
    this.flush();
  }
  flush() {
    const next = new Map();
    for (const [key, value] of this.sources) {
      if (!this.blocked.has(key)) next.set(`${value.port}:${value.button}`, value);
    }
    for (const [key, value] of this.output) if (!next.has(key)) this.send(value.port, value.button, false);
    for (const [key, value] of next) if (!this.output.has(key)) this.send(value.port, value.button, true);
    this.output = next;
  }
  clear() {
    // Held controls must reach neutral before they can press again.
    for (const key of this.sources.keys()) this.blocked.add(key);
    this.flush();
  }
  suspend(value) {
    this.suspended = value;
    if (value) this.clear();
  }
  poll(gamepads = []) {
    const connected = [...gamepads].filter(pad => pad?.connected && pad.mapping === 'standard');
    const present = new Set(connected.map(pad => pad.index));
    let changed = false;
    for (const [index, assignment] of this.pads) {
      if (present.has(index)) continue;
      for (const button of BUTTONS) this.set(`pad${index}`, assignment.port, button, false);
      this.pads.delete(index);
      changed = true;
    }
    for (const pad of connected.sort((a,b) => a.index - b.index)) {
      if (this.pads.has(pad.index)) continue;
      const port = [0,1,2,3].find(port => ![...this.pads.values()].some(value => value.port === port));
      if (port === undefined) continue;
      this.pads.set(pad.index, {port, id: pad.id});
      changed = true;
    }
    for (const pad of connected) {
      const assignment = this.pads.get(pad.index);
      if (!assignment) continue;
      const held = gamepadButtons(pad);
      for (const button of BUTTONS) this.set(`pad${pad.index}`, assignment.port, button, held.has(button));
    }
    if (changed) this.assignmentsChanged([...this.pads.values()].sort((a,b) => a.port - b.port));
  }
}
