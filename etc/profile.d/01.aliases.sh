#!/bin/bash

alias l="ls -h"
alias lll="ls -alh"
alias ll="ls -alh"
alias c="clear"
alias vi="vim"

if [ "$TERM" == "xterm-kitty" ]; then

alias ssh="kitty +kitten ssh "

fi