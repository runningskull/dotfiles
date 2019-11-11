################################################################################
## Aliases

  alias ..="cd .."

  # Colorize directory listing
  alias ls="ls -G"

  # Jump to git root directory
  alias cdg="cd \`(git rev-parse --show-toplevel)\`"

  # Shortcuts
  alias g='git'
  alias v='vim'
  alias get='curl -O'



################################################################################
# Helpers

  alias rgb="printf \#%02X%02X%02X $@"
  alias rgba="printf \#%02X%02X%02X%02X $@"

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

  LOCATION='`pwd | sed "s#\(/[^/]\{1,\}/[^/]\{1,\}/[^/]\{1,\}/\).*\(/[^/]\{1,\}/[^/]\{1,\}\)/\{0,1\}#\1...\2#g"`'
  JOBS='`if [ -n "$(jobs -p)" ]; then echo "✦"; fi`'

  PS1="\n${_GREY_}${LOCATION}\n${_GREEN_}${JOBS}${_LGREEN_}❯ ${_CLEAR_}"





# vim: set foldmethod=indent sw=2 et fdi= :
