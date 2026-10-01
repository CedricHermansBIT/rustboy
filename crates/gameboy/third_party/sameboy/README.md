# Replacement boot ROMs

These are the **unmodified, openly licensed SameBoy replacement boot ROMs**,
not dumps of Nintendo firmware. They run on the emulated CPU and provide a logo
animation/chime before handing off to the game. The DMG animation uses the logo
from the game's header; the CGB replacement features SameBoy branding and is
not the exact Nintendo Game Boy Color animation.

Source: [SameBoy v1.0.3](https://github.com/LIJI32/SameBoy/tree/v1.0.3/BootROMs).
Copyright and redistribution terms are retained in [LICENSE](LICENSE).
The same notice is embedded in the WASM bundle and exposed by
`get_boot_rom_license()`.

Provenance: `dmg_boot.bin` and `cgb_boot.bin` from the official
[v1.0.3 Windows SDL archive](https://github.com/LIJI32/SameBoy/releases/download/v1.0.3/sameboy_winsdl_v1.0.3.zip),
encoded as whitespace-separated hexadecimal for source control. The archive's
SHA-256, also published in GitHub release metadata, is:

```
66fb05acc075abba860f2c5fa31af2198fef9767573d28834c78d0dfc15746b2
```

Decoded binaries:

| File | Bytes | SHA-256 |
| --- | ---: | --- |
| dmg_boot.hex | 256 | 6f64da4cecd7e54e2f928eb3e3ba7810a7a567d0d247cc71737d1771e073a916 |
| cgb_boot.hex | 2304 | f767b8e7e510a255f81328c89dba6e0c996b370e1bc86aebb8584a7da47a5bba |

Builds decode these files at compile time; no network or RGBDS installation is
required. Original-firmware boot/timing tests continue to use locally supplied
boot ROMs and do not silently switch to these replacements.
