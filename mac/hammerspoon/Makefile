all: install

install:
	mkdir -p ~/.hammerspoon/
	cp init.lua ~/.hammerspoon/
	mkdir -p ~/.hammerspoon/emojis
	cp -r emojis/* ~/.hammerspoon/emojis
	/Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs -c "hs.reload()"
	mkdir -p ~/.hammerspoon/Spoons
	cp -r Spoons/*  ~/.hammerspoon/Spoons/
