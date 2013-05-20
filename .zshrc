################################################################################
# Zshuery (https://github.com/myfreeweb/zshuery)

source ~/projects/_cloned/zshuery/zshuery.sh
load_defaults
load_aliases
load_completion ~/projects/_cloned/zshuery/completion
load_correction

export EDITOR=vim

chpwd() {
    update_terminal_cwd
}


zstyle ':completion:*' menu select
zstyle ':git-info:' action    ' %F{white}(%s)%f'
zstyle ':git-info:' ahead     ' %F{red}ahead%f'
zstyle ':git-info:' behind    ' %F{red}behind%f%'
zstyle ':git-info:' branch    '%F{green}%b%f'
zstyle ':git-info:' dirty     ' %F{red}dirty%f'
zstyle ':git-info:' stashed   ' %F{yellow}stashed%f'
zstyle ':git-info:' unmerged  ' %F{red}unmerged%f'
zstyle ':git-info:' prompt    ' %F{black}➜%f %b%s%S%D%U%A%B'


export DEFAULT_USER='runningskull'
source ~/.zsh/git.sh
source ~/.zsh/prompt.sh



################################################################################
# My Customizations

export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

# Use MacVim's vim binary
alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'

# New virtualenv using python 3
alias mkve3='mkvirtualenv --no-site-packages --python=/usr/local/bin/python3'

# Utils
alias whichports='lsof -i | grep LISTEN'
function b64d() { echo "$@" | base64 -D ;}
alias ls='ls -G'

# iTerm Helpers
function tabtitle() { echo -ne "\033]1;$@\007" ;}

alias gg='git log --oneline --abbrev-commit --all --graph --decorate --color'

#source ~/nvm/nvm.sh

export PATH=$HOME/bin:/usr/local/bin:$PATH



### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
