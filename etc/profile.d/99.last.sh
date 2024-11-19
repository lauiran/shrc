#!/bin/bash

_curgb () {
    local v=$(_ggcurh)
    if [ ! -z "$v" ]; then
        #echo "$_curd/" | grep -q "/.git/"
        #[ $? -ne 0 ] && eval "$cmds" & disown

        local _cb=`readlink -f $(_ggcurb)`
        local _cf=$_cb/GCSTATUS
        local _wd="/tmp/shgitcache/$(id -u)"
        local _wds="$_wd/watchdirs"
        [ ! -e $_wd ] && mkdir -p $_wd
        grep -qF "$_cf" $_wds || echo "$_cf" >> $_wds

        local CCOLO=${RED}
        local _c=0
        [ -f "$_cf" ] && _c=$(cat $_cf)
        [ $_c -eq 2 ] && CCOLO=${GREEN}+
        [ $_c -eq 1 ] && CCOLO=${YELLOW}*
        echo -e " ${RED}<${CCOLO}$v${RED}>${NC}"
    fi
}

_set_prompt () {
    local v=$?
    PS1="${CYAN}\w$(_curgb)${NC}\n"

    if [[ "$(id -u)" == "0" ]]; then
        PS1=$PS1'\[\e[01;31m\]#\[\e[00m\] '
    else
        if [[ "$v" == "0" ]]; then
            PS1=$PS1'\[\e[01;32m\]\$\[\e[00m\] '
        else
            PS1=$PS1'\[\e[01;33m\]\$\[\e[00m\] '
        fi
    fi
}

PROMPT_COMMAND='_set_prompt'$'\n'"$PROMPT_COMMAND"

