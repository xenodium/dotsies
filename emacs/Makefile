all: init zenburn molokai dark_zenburn

init:
	python ../installer/copy_into.py init.el ~/.emacs.d/init.el "my-emacs" ";;"

dark_zenburn:
	curl https://raw.githubusercontent.com/gorauskas/darkburn-theme/master/darkburn-theme.el -o /tmp/darkburn-theme.el
	mkdir -p ~/.emacs.d/downloads/darkburn
	mv /tmp/darkburn-theme.el ~/.emacs.d/downloads/darkburn/

zenburn:
	curl https://raw.githubusercontent.com/bbatsov/zenburn-emacs/master/zenburn-theme.el -o /tmp/zenburn-theme.el
	mkdir -p ~/.emacs.d/downloads/zenburn
	mv /tmp/zenburn-theme.el ~/.emacs.d/downloads/zenburn/

molokai:
	curl https://raw.githubusercontent.com/alloy-d/color-theme-molokai/master/color-theme-molokai.el -o /tmp/color-theme-molokai.el
	mkdir -p ~/.emacs.d/downloads/molokai
	mv /tmp/color-theme-molokai.el ~/.emacs.d/downloads/molokai/
