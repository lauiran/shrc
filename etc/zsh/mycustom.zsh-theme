
if [[ $UID -eq 0 ]]; then
    local user_symbol='#'
    local color1=red
    local color2=violet
else
    local user_symbol='$'
    local color1=green
    local color2=yellow
fi

local user_host='%{$terminfo[bold]$fg[${color1}]%}%n%{$reset_color%}@%m'
local current_dir='%{$terminfo[bold]$fg[cyan]%}%~%{$reset_color%}'
local ret_status="%{$reset_color%}%(?:%{$fg_bold[${color1}]%}${user_symbol}:%{$fg_bold[${color2}]%}${user_symbol})%{$reset_color%}"

local rvm_ruby=''
if which rvm-prompt &> /dev/null; then
  rvm_ruby='%{$fg[red]%}‹$(rvm-prompt i v g)›%{$reset_color%}'
else
  if which rbenv &> /dev/null; then
    rvm_ruby='%{$fg[red]%}‹$(rbenv version | sed -e "s/ (set.*$//")›%{$reset_color%}'
  fi
fi
local git_branch='$(git_prompt_info)%{$reset_color%}'

PROMPT="${user_host} ${current_dir} ${rvm_ruby} ${git_branch}
${ret_status} "

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[blue]%}‹%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%}*%{$fg[blue]%}›"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%}›"

