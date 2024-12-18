#!/bin/bash

INITEL=~/.emacs.d/init.el

cat > $INITEL <<-EOF

(add-to-list 'load-path "~/.dotusr/etc/emacs/")
(require 'init-packages)

(require 'init-ui)
(require 'init-edit)
(require 'init-keybindings)
(require 'init-syntax)
(require 'init-modeline)
(require 'init-autoinsert)
(require 'init-orgmode)


EOF

echo 'INITEL install done :)'

