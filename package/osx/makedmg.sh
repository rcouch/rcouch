#! /bin/bash

DMG_NAME=$1
SRC_FOLDER=$2
VOLUME_NAME=$3
BACKGROUND_FILE=$4
VOLUME_ICON_FILE=$5
DMG_TEMP_NAME="rw.${DMG_NAME}"
echo DMG_TEMP_NAME = "${DMG_TEMP_NAME}"

ACTUAL_SIZE=`du -sm "$SRC_FOLDER" | sed -e 's/	.*//g'`
DISK_IMAGE_SIZE=$(expr $ACTUAL_SIZE + 20)
hdiutil create -srcfolder "$SRC_FOLDER" -volname "${VOLUME_NAME}" -fs HFS+ -fsargs "-c c=64,a=16,e=16" -format UDRW -size ${DISK_IMAGE_SIZE}m "${DMG_TEMP_NAME}"

# mount it
echo "Mounting disk image..."
MOUNT_DIR="/Volumes/${VOLUME_NAME}"
echo "Mount directory: $MOUNT_DIR"
echo hdiutil attach -readwrite -noverify -noautoopen "${DMG_TEMP_NAME}"
DEV_NAME=$(hdiutil attach -readwrite -noverify -noautoopen "${DMG_TEMP_NAME}" | egrep '^/dev/' | sed 1q | awk '{print $1}')
echo "Device name:     $DEV_NAME"

echo "Copying background file..."
mkdir "$MOUNT_DIR/.background"
cp "$BACKGROUND_FILE" "$MOUNT_DIR/.background/$BACKGROUND_FILE"

echo "Copying README.txt"
cp README.txt "$MOUNT_DIR/README.txt"

echo "Running Applescript: /usr/bin/osascript setviewoptions.applescript \"${VOLUME_NAME}\""
"/usr/bin/osascript" setviewoptions.applescript "${VOLUME_NAME}"
echo "Done running the applescript..."
sleep 10

# make sure it's not world writeable
echo "Fixing permissions..."
chmod -Rf go-w "${MOUNT_DIR}" &> /dev/null || true
echo "Done fixing permissions."

echo tell the volume that it has a special file attribute

SetFile -a C "$MOUNT_DIR"

# unmount
echo "Unmounting disk image..."
hdiutil detach "${DEV_NAME}"

# compress image
echo "Compressing disk image..."
hdiutil convert "${DMG_TEMP_NAME}" -format UDZO -imagekey zlib-level=9 -o "${DMG_NAME}"
rm -f "${DMG_TEMP_NAME}"
