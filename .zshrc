## Use zgen
DISABLE_AUTO_UPDATE="true"
. "${HOME}/.zsh/zgen.zsh"


##------------------------------------------------------------------------------
## zsh defaults

unsetopt share_history



##------------------------------------------------------------------------------
## Path

export PATH=$HOME/bin:/usr/local/bin:$PATH



##------------------------------------------------------------------------------
## Editors 

export EDITOR=nvim

# NeoVim!
alias vim="nvim"
alias v="nvim"

# Emacs!
alias Emacs="open /Applications/Emacs.app"
alias E="open /Applications/Emacs.app"
alias e="emacs"



##------------------------------------------------------------------------------
## Utils

# std unix
alias ls='ls -G'  # colors
alias ll='ls -lG'
alias c='clear'
alias rm='rm -i' # delete to trash

# dev stuff
alias g='git'

# directory aliases
hash -d p=~/projects/
hash -d r=~/projects/_rafflecopter
hash -d f=~/projects/_forked
hash -d c=~/projects/_cloned



##------------------------------------------------------------------------------
## Dev Environment

alias lein='rlwrap /usr/local/bin/lein'

# necessary ugliness
function usegpg () { killall gpg-agent ssh-agent; eval $(gpg-agent --daemon --enable-ssh-support) }

# node.js version manager
export NVM_DIR="/Users/runningskull/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm



##------------------------------------------------------------------------------
## Silliness
function spam() { python -c "print ':$@: ' * 500" | pbcopy }

