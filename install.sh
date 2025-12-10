#!/usr/bin/env bash

export CURDIR=$(dirname $(realpath $0))
export CURDIR=$(dirname "${BASH_SOURCE[0]}")
export CURDIR=`readlink -f $CURDIR`
export BASEDIR=~/.shrc

if [[ -e "$BASEDIR" ]]; then
    echo "$BASEDIR" already exists!
else
    echo 'ln -s -T' "$CURDIR" "$BASEDIR"
    ln -sT $CURDIR $BASEDIR
fi

bash $CURDIR/etc/vim/install.sh
#bash $CURDIR/etc/zsh/install.sh

_addprofile.sh () {
    if [[ ! -f "$1" ]]; then
        return
    fi
    grep -q "profile.sh" $1
    if [[ $? -eq 0 ]]; then
        return
    fi
    echo ". $BASEDIR/etc/profile.sh" >> $1
    echo "PS1='\[\033[01;36m\]\w\[\033[00m\]\[\033[01;31m\]\$(_ggcurhead v)\[\033[00m\]\\n\[\033[01;32m\]\$\[\033[00m\] '" >> $1
}

_addprofile.sh ~/.bashrc
#_addprofile.sh ~/.bash_profile

echo "ALL DONE! ;)"
