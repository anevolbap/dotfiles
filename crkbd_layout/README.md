# Corne Keyboard keymap

Load this
[`anevolbap.json`](anevolbap.json)
into the [QMK configurator tool](https://config.qmk.fm/) to explore or
define a new keymap. 

## Compile 

Compilation can be made in two ways:
1. with the UI,
2. by hand with `qmk compile -kb crkbd -km anevolbap`.

Either way, the output will be `crkbd_rev1_anevolbap.hex` required for
the *flash* step.

## Flash
**Note:** remember to flash both sides of the *corne* :sweat_smile:!

```qmk flash -kb crkbd -km anevolbap -bl dfu```
