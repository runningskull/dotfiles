# zsh
cp ~/.zshrc .
rm -rf .zgen; git clone https://github.com/tarjoilija/zgen.git .zgen; rm -rf .zgen.git

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
