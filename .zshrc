## Use zgen
DISABLE_AUTO_UPDATE="true"
source "${HOME}/.zgen/zgen.zsh"


if ! zgen saved; then
  zgen oh-my-zsh

  zgen load mafredi/async
  zgen load runningskull/pure
  zgen load peterhurford/get-it-on

  zgen save
fi


## virtualenv - todo:remove
# export WORKON_HOME=$HOME/.virtualenvs
# source /usr/local/bin/virtualenvwrapper.sh



##------------------------------------------------------------------------------
## History Tweaks

setopt inc_append_history
setopt SHARE_HISTORY
setopt HIST_IGNORE_SPACE
SAVEHIST=20000
HISTFILE=~/.zsh_history


# << not working

up-line-or-local-history() {
    zle set-local-history 1
    zle up-line-or-history
    zle set-local-history 0
}
zle -N up-line-or-local-history
down-line-or-local-history() {
    zle set-local-history 1
    zle down-line-or-history
    zle set-local-history 0
}
zle -N down-line-or-local-history

## local
bindkey "${terminfo[kcuu1]}" up-line-or-local-history
bindkey "${terminfo[kcud1]}" down-line-or-local-history


# global
bindkey "^[[1;5A" up-line-or-history    # [CTRL] + Cursor up
bindkey "^[[1;5B" down-line-or-history  # [CTRL] + Cursor down

# >> /not working





##------------------------------------------------------------------------------
## Theme

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=240"



##------------------------------------------------------------------------------
## Path

export PATH=$HOME/bin:/usr/local/bin:$PATH



##------------------------------------------------------------------------------
## Editors 

export EDITOR=vim

# NeoVim!
# alias vim="nvim"
# alias v="nvim"
alias v="vim"

# Emacs!
alias Emacs="/usr/local/bin/emacs"
alias E="/usr/local/bin/emacs"
alias emacs="emacs --no-window-system"
alias e="emacs"
alias spacemacs="env HOME=$HOME/spacemacs emacs"
# alias e2="env HOME=$HOME/emacs2 emacs --no-window-system"
# alias E2="env HOME=$HOME/emacs2 Emacs"

# Python!
alias py="python3"



##------------------------------------------------------------------------------
## Utils

# std unix
alias ls=' gls -G --group-directories-first --color=auto'
alias ll=' gls -lG --group-directories-first --color=auto'
alias cls=' clear;echo;ls'
alias rm='rm -i' # prompt for deleting
alias rmm='rm -f'

# speed-dial
alias a='ls'
alias c='clear'

# dev stuff
alias g='git'
alias gti='git'
alias gs='git status'
alias pull='git pull'
alias gpom='git pull origin master'
alias st='st --no-cache'
function greb() { git rebase $@ 2>/dev/null | grep CONFLICT }

# directory aliases
hash -d p=~/projects/
hash -d f=~/projects/_forked
hash -d c=~/projects/_cloned
hash -d e=~/.emacs.d
hash -d ec=~/.emacs.d/lisp/config

function kj() { $@ && terminal-notifier -title "iTerm" -message "done" || terminal-notifier -title "FAILED" -message "process returned nonzero"}
function jk() { $@ && terminal-notifier -title "iTerm" -message "done" || terminal-notifier -title "FAILED" -message "process returned nonzero"}

source ~/.zsh/completion.zsh



##------------------------------------------------------------------------------
## Dev Environment

alias car='carthage'

alias lein='rlwrap /usr/local/bin/lein'
alias cljrepl='rlwrap java -cp ~/.m2/repository/org/clojure/clojure/1.8.0/clojure-1.8.0.jar clojure.main'

# necessary ugliness
function usegpg () { killall gpg-agent ssh-agent; eval $(gpg-agent --daemon --enable-ssh-support) }

# node.js version manager
export NVM_DIR="/Users/runningskull/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm


# Racket

export PATH="/Applications/Racket v6.8/bin:$PATH"



#### TEMP: postgres

alias postgres.server="pg_ctl -D /usr/local/var/postgres start"



##------------------------------------------------------------------------------
## Silliness
function spam() { python -c "print ':$@: ' * 500" | pbcopy }







##------------------------------------------------------------------------------
## Testing

preexec() {
  printf "\033[38;5;236m\n"
  printf '%*s\n' "${COLUMNS:-$(tput cols)}" '' | tr ' ' ┉
  printf "\e[0m"
  clear;echo "";
}

