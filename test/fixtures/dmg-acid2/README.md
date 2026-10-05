# dmg-acid2 fixtures

Upstream: https://github.com/mattcurrie/dmg-acid2

Version: v1.0, commit `dc2295408f881637ff69f784d8d93f3d2db30181`.
The upstream MIT license is included in `LICENSE`.

- `dmg-acid2.gb`: unmodified v1.0 release ROM, downloaded from
  https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb
- `reference-dmg.png`: unmodified 160x144 DMG reference, downloaded from
  https://raw.githubusercontent.com/mattcurrie/dmg-acid2/dc2295408f881637ff69f784d8d93f3d2db30181/img/reference-dmg.png

SHA-256:

```text
464e14b7d42e7feea0b7ede42be7071dc88913f75b9ffa444299424b63d1dff1  dmg-acid2.gb
ca966d50895c7efef05838590d148c2cbfd7fba57dab986f25b35b4da71abb57  reference-dmg.png
```

## Running the test

```sh
cabal test gb-test --test-options='--match dmg-acid2' --test-show-details=direct
```

`PPUSpec` starts at the cartridge entry point with DMG post-boot registers;
the separate CPU boot test covers boot-ROM execution. CPU and PPU run together,
including the ROM's real LY=LYC interrupts. No test-side register changes are
injected to emulate those interrupts.

The ROM contains an `LD B,B` screenshot marker after ten rendered frames. The
test stops there and compares all 160x144 pixels with the upstream DMG reference,
using grayscale levels 255, 170, 85, and 0. It does not require a window or network.

A cycle limit, CPU-step limit (including zero-cycle execution steps), and
30-second timeout prevent hangs. Rendering mismatches, cycle-limit failures,
and unimplemented CPU instructions save a diagnostic image to
`dist-newstyle/ppu-test/dmg-acid2-actual.png`.

This is a rendering integration test, not a dot-timing accuracy test. A failure
can expose missing CPU or interrupt behavior as well as PPU rendering bugs.
