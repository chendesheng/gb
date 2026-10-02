# Battery indicator assets

Generated with the built-in imagegen tool. The original device skin is unchanged;
the UI overlays these matching transparent sprites at the battery-light position.

## battery-off.png

Reference: `device.png`, battery LED at original coordinates (484, 374).

Prompt: Extract and recreate only the circular battery LED with its thin black
housing. Make the lens glossy neutral gray with subtle white reflections, no red
and no glow. Match the original frontal view and material. Center it in a square
canvas with minimal transparent padding. Everything outside the ring must be
transparent. No bezel, text, device, or printed checkerboard.

## battery-on.png

Edit target: `battery-off.png`.

Prompt: Create the matching power-on variant. Change only the gray lens to bright
illuminated red with subtle white reflections. Preserve the black rim, position,
size, frontal geometry, transparent padding, and square canvas. No external glow
or text. Everything outside the ring remains transparent.
