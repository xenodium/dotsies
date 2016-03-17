#!/bin/bash

cp -i bash_overrides ~/.bash_overrides

if ! grep -q 'bash_overrides' ~/.bash_profile; then
 echo 'adding .bash_overrides to .bash_profile'
 echo 'source ~/.bash_overrides' >> ~/.bash_profile
fi

if ! grep -q 'bash_overrides' ~/.bashrc; then
 echo 'adding .bash_overrides to .bashrc'
 echo 'source ~/.bash_overrides' >> ~/.bashrc
fi

source ~/.bash_profile
