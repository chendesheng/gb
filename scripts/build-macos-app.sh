#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_dir"

cabal build --enable-optimization=2 gb:gb-exe
gb_executable=$(cabal list-bin --enable-optimization=2 gb:gb-exe)
bundle_dir="$project_dir/dist-newstyle/Game Boy.app"
mkdir -p "$bundle_dir/Contents/MacOS" "$bundle_dir/Contents/Resources"
cp "$gb_executable" "$bundle_dir/Contents/MacOS/gb-exe"
cp app/Info.plist "$bundle_dir/Contents/Info.plist"
cp resources/device.png "$bundle_dir/Contents/Resources/device.png"
cp resources/battery-off.png "$bundle_dir/Contents/Resources/battery-off.png"
cp resources/battery-on.png "$bundle_dir/Contents/Resources/battery-on.png"
cp resources/dmg.bin "$bundle_dir/Contents/Resources/dmg.bin"

iconset_dir="$project_dir/dist-newstyle/GameBoy.iconset"
mkdir -p "$iconset_dir"
for icon_size in 16 32 128 256 512; do
    sips -z "$icon_size" "$icon_size" resources/logo.png \
        --out "$iconset_dir/icon_${icon_size}x${icon_size}.png" >/dev/null
    retina_size=$((icon_size * 2))
    sips -z "$retina_size" "$retina_size" resources/logo.png \
        --out "$iconset_dir/icon_${icon_size}x${icon_size}@2x.png" >/dev/null
done
iconutil -c icns "$iconset_dir" -o "$bundle_dir/Contents/Resources/GameBoy.icns"

printf 'Built %s\n' "$bundle_dir"
