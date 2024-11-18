#!/bin/bash

OMZSHDIR=$BASEDIR/etc/oh-my-zsh

export NODE_TLS_REJECT_UNAUTHORIZED="0"
export NODE_TLS_MIN_PROTOCOL_VERSION="TLSv1.2"

_ggclone() {
    url=$1
    #ddir=${url##*/}
    ddir=$2
    if [[ ! -d "$ddir" ]]; then
        git clone $url $OMZSHDIR
    fi
}

_ggclone https://github.com/robbyrussell/oh-my-zsh \
    $OMZSHDIR
_ggclone https://github.com/zsh-users/zsh-syntax-highlighting \
    $OMZSHDIR/custom/plugins/zsh-syntax-highlighting
_ggclone https://github.com/zsh-users/zsh-autosuggestions \
    $OMZSHDIR/custom/plugins/zsh-autosuggestions

cp -f $BASEDIR/etc/zsh/mycustom.zsh-theme \
    $OMZSHDIR/custom/themes
mkdir -p $OMZSHDIR/custom/plugins/mycustom
cp -f $BASEDIR/etc/zsh/mycustom.plugin.zsh \
    $OMZSHDIR/custom/plugins/mycustom

cp -f $BASEDIR/etc/zsh/dotzshrc ~/.zshrc

echo 'ZSHRC install done :)'

