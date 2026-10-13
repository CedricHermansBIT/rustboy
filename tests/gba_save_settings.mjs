import assert from 'node:assert/strict';
import {installGbaSaveSettings} from '../web/gba-save-settings.mjs';
const values=new Map();
globalThis.localStorage={getItem:key=>values.get(key)??null,setItem:(key,value)=>values.set(key,value),removeItem:key=>values.delete(key)};
const select={value:'auto',listeners:{},addEventListener(type,fn){this.listeners[type]=fn;}};
const status={textContent:''}, notices=[];
let key='', kind='', loaded=false;
const rom={key:'rustboy_save_gba_originalhash',detected:'flash64'};
const api={
  get_gba_rom_save_key:rom=>rom.key,
  get_gba_save_type:()=>kind,
  get_save_key:()=>key,
  load_gba_data(){throw new Error('Expected the save-aware loader');},
  load_gba_data_with_save_type(rom,bios,override){
    assert.equal(bios.length,0);
    if(override!=='flash128') throw new Error('Stored battery size does not match detected chip');
    key=rom.key;kind=override;loaded=true;
  },
  set_gba_save_type(type){if(type==='none') throw new Error('Would discard saved bytes');kind=type;},
};
const controller=installGbaSaveSettings(api,{querySelector:id=>id==='#gba-save-type'?select:status},(...args)=>notices.push(args));
assert.throws(()=>controller.load(rom,new Uint8Array()),/size/);
assert.equal(loaded,false);
select.value='flash128';select.listeners.change();
assert.equal(values.get(rom.key+'_type'),'flash128');
controller.load(rom,new Uint8Array());
assert.equal(kind,'flash128');
assert.equal(select.value,'flash128');
select.value='none';select.listeners.change();
assert.equal(select.value,'flash128');
assert.equal(values.get(rom.key+'_type'),'flash128');
assert.match(notices.at(-1)[0],/discard/);
controller.load(rom,new Uint8Array());
assert.equal(kind,'flash128','Remembered override precedes every import');
kind='';controller.refresh();
assert.equal(select.value,'auto','A previous cartridge override does not leak into the next game');
console.log('GBA save settings: failed-load repair, remembered pre-import override and rejected unsafe changes passed');
