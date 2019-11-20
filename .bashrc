################################################################################
## Aliases

  alias ..="cd .."

  # Colorize directory listing
  alias a='ls -G'
  alias ls='ls -G'
  alias la='ls -Gal'

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
  PATH="~/.bin:$PATH"



################################################################################
# Prompt

  _CLEAR_=$'\e[0m'
  _GREEN_=$'\e[0;32m'
  _GREY_=$'\e[0;37m'
  _LGREEN_=$'\e[1;32m'

  function abbrev_pwd() { pwd | sed -E 's#(([^/]+/){4}).*((/[^/]+){2})#\1...\3#g'; }
  function show_jobs() { if [ -n "$(jobs -p)" ]; then echo "✦"; fi; }

  LOC='`abbrev_pwd`'
  JOB='`show_jobs`'

  PS1="\n${_GREY_}${LOC}\n${_GREEN_}${JOB}${_LGREEN_}❯ ${_CLEAR_}"





# vim: set foldmethod=indent sw=2 et fdi= :
