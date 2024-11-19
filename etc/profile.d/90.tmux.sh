#!/bin/bash

_default_tmux () {
    if [ "$PS1" != "" ]; then
        if [ "$TERM" != "screen" ] && [ "$TERM" != "tmux" ] && [ -z "$TMUX" ]; then
            tmux attach -t 1 || tmux new -s 1
        fi
    fi
}

_my_tmux_cfg () {
    if [ ! -e '~/.tmux.conf' ]; then
        ln -sfT ~/.dotusr/etc/tmux/tmux.conf ~/.tmux.conf
    fi
}

#if command -v tmux &> /dev/null; then
#    _my_tmux_cfg
#    _default_tmux
#fi

