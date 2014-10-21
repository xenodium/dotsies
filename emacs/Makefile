all: init emaxcode silver_searcher

silver_searcher:
	brew install ag || apt-get install silversearcher-a

init:
	mkdir -p ~/.emacs.d
	python ../installer/copy_into.py init.el ~/.emacs.d/init.el "my-emacs" ";;"

emaxcode:
	mkdir -p ~/.emacs.d/lib/snippet
	curl https://raw.githubusercontent.com/ShingoFukuyama/emaXcode/master/emaXcode.el -o /tmp/emaXcode.el
	mkdir -p ~/.emacs.d/downloads/emaXcode
	mv /tmp/emaXcode.el ~/.emacs.d/downloads/emaXcode
