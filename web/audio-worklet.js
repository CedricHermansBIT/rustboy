// Runs on the browser's audio rendering thread, independent of UI/ROM work.
class RustBoyAudioProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.capacity = 32768;
    this.prebuffer = 8192;
    this.left = new Float32Array(this.capacity);
    this.right = new Float32Array(this.capacity);
    this.reset();
    this.port.onmessage = ({data}) => {
      if (data.type === 'reset') { this.reset(); return; }
      if (data.type !== 'samples' || !(data.left instanceof Float32Array) ||
          !(data.right instanceof Float32Array) || data.left.length !== data.right.length) return;
      let dropped = false;
      for (let i = 0; i < data.left.length; i++) {
        this.left[this.write] = Number.isFinite(data.left[i]) ? data.left[i] : 0;
        this.right[this.write] = Number.isFinite(data.right[i]) ? data.right[i] : 0;
        this.write = (this.write + 1) % this.capacity;
        if (this.available === this.capacity) { this.read = (this.read + 1) % this.capacity; dropped = true; }
        else this.available++;
      }
      if (dropped && this.primed) this.fadeIn();
    };
  }

  reset() {
    this.read = this.write = this.available = 0;
    this.primed = false;
    this.lastLeft = this.lastRight = 0;
    this.fade = this.tail = 0;
    this.fromLeft = this.fromRight = 0;
  }

  fadeIn() {
    this.fade = 64;
    this.fromLeft = this.lastLeft;
    this.fromRight = this.lastRight;
  }

  process(_inputs, outputs) {
    const output = outputs[0];
    if (!output?.length) return true;
    const left = output[0], right = output[1] || output[0];
    if (!this.primed && this.available >= this.prebuffer) {
      this.primed = true;
      this.fadeIn();
    }
    // Do not assume render blocks will always contain 128 samples.
    for (let i = 0; i < left.length; i++) {
      let l = 0, r = 0;
      if (this.primed && this.available > 0) {
        l = this.left[this.read]; r = this.right[this.read];
        this.read = (this.read + 1) % this.capacity;
        this.available--;
        if (this.fade > 0) {
          const weight = (65 - this.fade--) / 64;
          l = this.fromLeft + (l - this.fromLeft) * weight;
          r = this.fromRight + (r - this.fromRight) * weight;
        }
        this.lastLeft = l; this.lastRight = r;
      } else {
        if (this.primed) { this.primed = false; this.tail = 64; }
        if (this.tail > 0) {
          const weight = --this.tail / 64;
          l = this.lastLeft * weight; r = this.lastRight * weight;
          if (this.tail === 0) this.lastLeft = this.lastRight = 0;
        }
      }
      left[i] = l; right[i] = r;
    }
    return true;
  }
}

registerProcessor('rustboy-audio', RustBoyAudioProcessor);
