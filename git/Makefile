all: git_complete git_prompt

git_complete:
	rm -f ~/.git-completion.sh
	curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash -o ~/.git-completion.sh

git_prompt:
	rm -f ~/.git-prompt.sh
	curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh -o ~/.git-prompt.sh

git_config:
	python ../installer/copy_into.py gitconfig ~/.gitconfig "gitconfig" "#"
