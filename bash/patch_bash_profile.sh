#!/bin/bash

add_source_entry() {
  echo 'adding .bash_overrides to .bash_profile'
  echo '[ -f "$HOME/.bash_overrides" ] && source "$HOME/.bash_overrides"' >> ~/.bash_profile
}

# .bash_profile doesn't exist. Create.'
if [[ ! -e ~/.bash_profile  ]]; then
  add_source_entry
  exit 0
fi

# .bash_profile exists. Patch if needed.
if ! grep -q 'bash_overrides' ~/.bash_profile; then
  add_source_entry
fi
