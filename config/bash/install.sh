#!/bin/bash
set -o nounset
set -o errexit

# TODO(alvaro): Copy based on OS.
cp -i mac_bashrc ~/.mac_bashrc
cp -i portable_bashrc ~/.portable_bashrc
cp -i alvaro_bashrc ~/.alvaro_bashrc

cd ~/
source .alvaro_bashrc

echo "Don't forget to 'source .alvaro_bashrc'"