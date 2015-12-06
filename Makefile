create_emacs.d:
	mkdir ~/.emacs.d

link:
	ln -sfn `pwd`/emacs.el ~/.emacs
	ln -sfn `pwd`/lib ~/.emacs.d/

link_osx_bin:
	ln -sfn `pwd`/bin/emacs /usr/local/bin/gemacs
	chmod +x /usr/local/bin/gemacs

# sane exuberant ctags
install-ctags:
	brew install ctags-exuberant
