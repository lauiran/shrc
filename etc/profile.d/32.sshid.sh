#!/bin/bash

sshid () {
    sshcfg="$HOME/.ssh/config"
    if [[ -z "$1" ]]; then
        cat $sshcfg
        return
    fi
    grep -A8 -B3 $1 $sshcfg
}

