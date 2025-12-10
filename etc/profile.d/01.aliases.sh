#!/bin/bash

alias l="ls -h"
alias lll="ls -alh"
alias ll="ls -alh"
alias c="clear"
alias vi="vim"
alias kittyq="kitten quick_access_terminal -o edge=bottom -o lines=20 -o background_opacity=0.8 --detach"

if [ "$TERM" == "xterm-kitty" ]; then
    alias ssh="kitty +kitten ssh "
fi

function issh () {
    kitten @ set-tab-title $1
    /bin/ssh $*
}

function kssh () {
    arg_t=""
    arg_o=()
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -t)
                if [[ $# -lt 2 ]]; then
                    echo 'ssh-tmux hostname [-t tmux-session]'
                    return 1
                fi
                arg_t="$2"
                shift 2
                ;;
            *)
                arg_o+=("$1")
                shift
                ;;
        esac
    done

    kitten @ set-tab-title ${arg_o[0]}

    if [[ -z "$arg_t" ]]; then
        kitty +kitten ssh "${arg_o[@]}"
    else
        kitty +kitten ssh "${arg_o[@]}" -t "tmux new -A -s \"$arg_t\" "
    fi
    kitten @ set-tab-title ${arg_o[0]}
}

