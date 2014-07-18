#!/bin/bash
set -o nounset
set -o errexit

cp -i bashrc_overrides ~/.bashrc_overrides

if ! grep -q 'bashrc_overrides' ~/.bashrc; then
 echo 'adding .bashrc_overrides to .bashrc'
 echo 'source ~/.bashrc_overrides' >> ~/.bashrc
fi
source ~/.bashrc
