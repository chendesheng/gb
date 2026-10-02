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

printf 'Built %s\n' "$bundle_dir"
