#!/bin/bash

echo 'adding .bash_overrides to .bash_profile'
if [[ ! -e ~/.bash_profile  ]]; then
  echo 'source ~/.bash_overrides' >> ~/.bash_profile
  exit 0
fi

if ! grep -q 'bash_overrides' ~/.bash_profile; then
  echo 'source ~/.bash_overrides' >> ~/.bash_profile
fi
