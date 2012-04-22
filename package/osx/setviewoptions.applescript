on run (volumeName)
        tell application "Finder"
                tell disk (volumeName as string)
                        open
                        set current view of container window to icon view
                        set toolbar visible of container window to false
                        set statusbar visible of container window to false
                        set the bounds of container window to {400, 100, 885, 430}
                        set theViewOptions to the icon view options of container window
                        set arrangement of theViewOptions to not arranged
                        set background picture of theViewOptions to file ".background:dmg-background.png"
                        set icon size of theViewOptions to 72
                        set dsStore to "\"" & "/Volumes/" & volumeName & "/" & ".DS_STORE\""
                        make new alias file at container window to POSIX file "/Applications" with properties {name:"Applications"}
                        set position of item "rcouchx.app" of container window to {100, 200}
                        set position of item "README.txt" of container window to {250, 200}
                        set position of item "Applications" of container window to {375, 200}
                        close
                        open
                        update without registering applications
                        delay 10
                end tell
        end tell
end run

