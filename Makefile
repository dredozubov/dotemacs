default: link_osx_bin

create_emacs.d:
	mkdir -p ~/.emacs.d

link: create_emacs.d
	ln -sfn `pwd`/emacs.el ~/.emacs
	ln -sfn `pwd`/lib/ ~/.emacs.d/lib

link_osx_bin: link
	ln -sfn `pwd`/bin/emacs /usr/local/bin/gemacs
	chmod +x /usr/local/bin/gemacs

# sane exuberant ctags
install-ctags:
	brew install ctags-exuberant
