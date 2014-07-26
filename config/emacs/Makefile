all: emacs molokai

dot_emacs:
	python ../installer/copy_into.py emacs ~/.emacs "my-emacs" ";;"

molokai:
	curl https://raw.githubusercontent.com/alloy-d/color-theme-molokai/master/color-theme-molokai.el -o /tmp/color-theme-molokai.el
	mkdir -p ~/.emacs.d/downloads/molokai
	mv /tmp/color-theme-molokai.el ~/.emacs.d/downloads/molokai/
