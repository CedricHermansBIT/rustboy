ENTRY(multiboot_host)
SECTIONS {
  . = 0x1800;
  .host : { *(.host) }
  ASSERT(. <= 0x2000, "host firmware overlaps client entry")
  . = 0x2000;
  .client : { *(.client) }
  . = ALIGN(4);
  .text : { *(.text*) *(.rodata*) }
  ASSERT(. <= 0x4000, "multiboot firmware exceeds BIOS window")
  /DISCARD/ : { *(.ARM.exidx*) *(.ARM.extab*) *(.comment*) *(.note*) }
}
