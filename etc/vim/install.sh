#!/bin/bash

DOTVIM=~/.vimrc

if [[ -f "$DOTVIM" ]]; then
	mv $DOTVIM ${DOTVIM}.bk
fi

cat > $DOTVIM <<-EOF

let \$MYVIMPATH='$BASEDIR/etc/vim'
set runtimepath+=\$MYVIMPATH

set ambw=double
"set pythonthreedll=python310.dll

source \$MYVIMPATH/vimrc_basic.vim
source \$MYVIMPATH/vimrc_filetype.vim
source \$MYVIMPATH/vimrc_plugin.vim
source \$MYVIMPATH/vimrc_extended.vim

EOF

echo 'VIMRC install done :)'

