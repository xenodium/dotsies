#!/bin/bash

if ! grep -q 'bashrc_overrides' ~/.bashrc; then
 echo 'adding .bashrc_overrides to .bashrc'
 echo 'source ~/.bashrc_overrides' >> ~/.bashrc
fi

