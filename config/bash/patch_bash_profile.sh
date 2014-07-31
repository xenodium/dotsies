#!/bin/bash

if ! grep -q 'bash_overrides' ~/.bash_profile; then
  echo 'adding .bash_overrides to .bash_profile'
  echo 'source ~/.bash_overrides' >> ~/.bash_profile
fi
