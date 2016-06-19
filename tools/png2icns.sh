#!/bin/bash
set -o nounset
set -o errexit

# png dimensions 1024x1024.
# Based on http://stackoverflow.com/questions/12306223/how-to-manually-create-icns-files-using-iconutil

PNG_FILE_PATH="$1"
PNG_FILE_NAME=$(basename "$PNG_FILE_PATH")
IMAGE_FILE_NAME="${PNG_FILE_NAME%.*}"

mkdir _temIcon.iconset
sips -z 16 16     $PNG_FILE_PATH --out _temIcon.iconset/icon_16x16.png
sips -z 32 32     $PNG_FILE_PATH --out _temIcon.iconset/icon_16x16@2x.png
sips -z 32 32     $PNG_FILE_PATH --out _temIcon.iconset/icon_32x32.png
sips -z 64 64     $PNG_FILE_PATH --out _temIcon.iconset/icon_32x32@2x.png
sips -z 128 128   $PNG_FILE_PATH --out _temIcon.iconset/icon_128x128.png
sips -z 256 256   $PNG_FILE_PATH --out _temIcon.iconset/icon_128x128@2x.png
sips -z 256 256   $PNG_FILE_PATH --out _temIcon.iconset/icon_256x256.png
sips -z 512 512   $PNG_FILE_PATH --out _temIcon.iconset/icon_256x256@2x.png
sips -z 512 512   $PNG_FILE_PATH --out _temIcon.iconset/icon_512x512.png
cp $PNG_FILE_PATH _temIcon.iconset/icon_512x512@2x.png
iconutil -c icns _temIcon.iconset
rm -R _temIcon.iconset
mv _temIcon.icns "$IMAGE_FILE_NAME".icns
