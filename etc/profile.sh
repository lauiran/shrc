
#!/bin/bash

export PATH=~/.dotusr/bin:~/.local/bin:/usr/local/bin:$PATH

if [ -d ~/.dotusr/etc/profile.d ]; then
  for i in ~/.dotusr/etc/profile.d/*.sh; do
    if [ -r $i ]; then
      . $i
    fi
  done
  unset i
fi

