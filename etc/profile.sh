
#!/bin/bash

export PATH=~/.shrc/bin:~/.local/bin:/usr/local/bin:$PATH

if [ -d ~/.shrc/etc/profile.d ]; then
  for i in ~/.shrc/etc/profile.d/*.sh; do
    if [ -r $i ]; then
      . $i
    fi
  done
  unset i
fi

