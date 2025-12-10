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
        ln -sfT ~/.shrc/etc/tmux/tmux.conf ~/.tmux.conf
    fi
}

if [ -n "$SSH_TTY" ]; then
echo "$SSH_TTY"
#if command -v tmux &> /dev/null; then
#    _my_tmux_cfg
#    _default_tmux
#fi
fi
