# ZSH Theme emulating the Fish shell's default prompt.

source ~/.zsh/git-prompt/zshrc.sh

_fishy_collapsed_wd() {
  echo $(pwd | perl -pe "
   BEGIN {
      binmode STDIN,  ':encoding(UTF-8)';
      binmode STDOUT, ':encoding(UTF-8)';
   }; s|^$HOME|~|g; s|/([^/])[^/]*(?=/)|/\$1|g
")
} 

_suspended_jobs() {
    [[ $(jobs -l | wc -l) -gt 0 ]] && echo "%{%F{yellow}%}⟐ "
}

#_suspended_jobs() {
#    local symbols
#    symbols=()
#    [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}⚙
#}

local user_color='green'; [ $UID -eq 0 ] && user_color='red'
PROMPT='$(_suspended_jobs)%b$(git_super_status)%b%{$fg[$user_color]%}$(_fishy_collapsed_wd)%{$reset_color%}%(!.#.▸) '
PROMPT2='%{$fg[red]%}\ %{$reset_color%}'

#local return_status="%{$fg_bold[red]%}%(?..%?)%{$reset_color%}"
#RPROMPT='${return_status}$(git_super_status)%{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=") "
ZSH_THEME_GIT_PROMPT_DIRTY=""
#ZSH_THEME_GIT_PROMPT_CLEAN=""

#ZSH_THEME_GIT_PROMPT_ADDED="%{$fg_bold[green]%}+"
#ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg_bold[blue]%}!"
#ZSH_THEME_GIT_PROMPT_DELETED="%{$fg_bold[red]%}-"
#ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg_bold[magenta]%}>"
#ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg_bold[yellow]%}#"
#ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg_bold[cyan]%}?"
