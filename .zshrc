zmodload zsh/zprof

################################################################################
## Core Config

  HISTSIZE=500
  HISTFILESIZE=5000


################################################################################
## Aliases

  alias ..="cd .."

  # Colorize directory listing
  alias a='ls -G'
  alias ls='ls -G'
  alias la='ls -Ga'
  alias ll='ls -Gal'

  # Jump to git root directory
  alias cdg="cd \`(git rev-parse --show-toplevel)\`"

  # Shortcuts
  alias g='git'
  alias v='vim'
  alias get='curl -O'

  # Total directory size
  alias sizeof='du -sh'



################################################################################
# Helpers

  alias rgb="printf \#%02X%02X%02X $@"
  alias rgba="printf \#%02X%02X%02X%02X $@"

  function hex2rgba() { printf "%d %d %d %d\n" 0x${1:0:2} 0x${1:2:2} 0x${1:4:2} 0x${1:6:2}; }
  function hex2rgb() { printf "%d %d %d\n" 0x${1:0:2} 0x${1:2:2} 0x${1:4:2}; }

  # Easy open url on phone
  function url() { qrencode -t ansi "$1"; }

  # Like 'pwd' but for a given file
  function fwd() { echo "$(pwd)/$1"; }

  # Open GitHub repo
  function github() { 
    open "$(git config --get remote.origin.url | sed -E 's#git@github.com:(.+)\.git#https://github.com/\1/#')";
  }



################################################################################
# Path Management

  PATH="/usr/local/opt/qt/bin:$PATH"
  PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"

  # local utils are highest priority
  PATH="~/bin:$PATH"



################################################################################
# Prompt

  setopt PROMPT_SUBST

  CRLF=$'\n'

  function abbrev_pwd() { pwd | sed -E 's#(([^/]+/){4}).*((/[^/]+){2})#\1...\3#g'; }
  function show_jobs() { if [ -n "$(jobs -p)" ]; then echo "✦"; fi; }

  PS1="${CRLF}%F{8}\$(abbrev_pwd)${CRLF}%F{2}\$(show_jobs)%F{10}❯ %f"



################################################################################
# Completion

  autoload -Uz compinit
  (){ if [[ $# -gt 0 ]]; then compinit; else compinit -C; fi } ~/.zcompdump(N.mh+24)

  zstyle ':completion:*' matcher-list '' \
    'm:{a-zA-Z-_}={A-Za-z_-}' \
    'r:|[._-]=* r:|=*' \
    'l:|=* r:|=*'



################################################################################
# vim: set foldmethod=indent sw=2 et fdi= :
