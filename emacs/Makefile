all: init zenburn dark_zenburn emaxcode silver_searcher

silver_searcher:
	brew install ag || apt-get install silversearcher-a

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

emaxcode:
	mkdir -p ~/.emacs.d/lib/snippet
	curl https://raw.githubusercontent.com/ShingoFukuyama/emaXcode/master/emaXcode.el -o /tmp/emaXcode.el
	mkdir -p ~/.emacs.d/downloads/emaXcode
	mv /tmp/emaXcode.el ~/.emacs.d/downloads/emaXcode
