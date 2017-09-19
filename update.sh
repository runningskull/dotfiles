# zsh
cp ~/.zshrc .
cp ~/.zgen/zgen.zsh .zgen

# vim
cp ~/.vimrc .
cp -R ~/.vim/colors .vim
cp ~/.vim/autoload/plug.vim .vim/autoload

#emacs
cp -R ~/.emacs.d/config .emacs.d 
cp ~/.emacs.d/init.el .emacs.d

#general
cp ~/.inputrc .

./remind.sh
