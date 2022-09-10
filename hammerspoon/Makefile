all: install

install:
	mkdir -p ~/.hammerspoon/
	cp init.lua ~/.hammerspoon/
	mkdir -p ~/.hammerspoon/emojis
	cp -r emojis/* ~/.hammerspoon/emojis
	mkdir -p ~/.hammerspoon/Spoons
	cp -r Spoons/*  ~/.hammerspoon/Spoons/
	mkdir -p ~/.hammerspoon/ar
	cp -r ar/*  ~/.hammerspoon/ar/
	/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c "hs.reload()"
