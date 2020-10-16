all: init emaxcode silver_searcher global yasnippet

silver_searcher:
	which ag || brew install ag || apt-get install silversearcher-a

init:
	mkdir -p ~/.emacs.d
	python ../installer/copy_into.py init.el ~/.emacs.d/init.el "my-emacs" ";;"
	mkdir -p ~/.emacs.d/local/
	curl https://raw.githubusercontent.com/xenodium/company-async-files/master/company-async-files.el -o ~/.emacs.d/local/company-async-files.el || echo "No internet connection... skipping company-async-files.el install."
	curl https://raw.githubusercontent.com/xenodium/tmtxt-async-tasks/master/tmtxt-async-tasks.el -o ~/.emacs.d/local/tmtxt-async-tasks.el || echo "No internet connection... skipping tmtxt-async-task install."
	curl https://raw.githubusercontent.com/xenodium/tmtxt-dired-async/master/tmtxt-dired-async.el -o ~/.emacs.d/local/tmtxt-dired-async.el || echo "No internet connection... skipping tmtxt-dired-async install."
	curl https://raw.githubusercontent.com/xenodium/org-starless/master/org-starless.el -o ~/.emacs.d/local/org-starless.el || echo "No internet connection... skipping org-starless install."
	mkdir -p ~/local/bin/
	cp emacssocket ~/local/bin/
	cp open_xcode_file_in_emacs.sh ~/local/bin/
	make -C ar
	make -C features
	make -C vendored

pylint:
	cp -i python/pylintrc ~/.pylintrc

global:
	cp -i global/.globalrc ~/

yasnippet:
	mkdir -p ~/.emacs.d/yasnippets
	cp -rf yasnippets/* ~/.emacs.d/yasnippets

yasxcode:
	mkdir -p ~/.emacs.d/yasnippets
	cd yasnippets/personal && python ../generate-xcode-yasnippets.py

emaxcode:
	curl https://raw.githubusercontent.com/xenodium/emaXcode/master/emaXcode.el -o /tmp/emaXcode.el
	mkdir -p ~/.emacs.d/local/emaXcode
	mv /tmp/emaXcode.el ~/.emacs.d/local/emaXcode

local_path := ~/local
ifeq ($(shell uname), Darwin)
  local_path := ~/homebrew
endif
rtags:
	rm -rf ~/.emacs.d/local/rtags
	git clone https://github.com/xenodium/rtags.git ~/.emacs.d/local/rtags
	cd ~/.emacs.d/local/rtags && git submodule init && git submodule update
	cd ~/.emacs.d/local/rtags && cmake . && make

rtags_setup:
	ln -s ~/.emacs.d/local/rtags/bin/gcc-rtags-wrapper.sh $(local_path)/bin/gcc
	ln -s ~/.emacs.d/local/rtags/bin/gcc-rtags-wrapper.sh $(local_path)/bin/c++
	ln -s ~/.emacs.d/local/rtags/bin/gcc-rtags-wrapper.sh $(local_path)/bin/cc
	ln -s ~/.emacs.d/local/rtags/bin/gcc-rtags-wrapper.sh $(local_path)/bin/g++

rtags_reset:
	rm -f $(local_path)/bin/gcc
	rm -f $(local_path)/bin/c++
	rm -f $(local_path)/bin/cc
	rm -f $(local_path)/bin/g++

ycmd_install:
	rm -rf ~/.emacs.d/local/ycmd
	git clone https://github.com/xenodium/ycmd.git ~/.emacs.d/local/ycmd
	cd ~/.emacs.d/local/ycmd && git submodule update --init --recursive
	cd ~/.emacs.d/local/ycmd && ./build.sh --clang-completer
