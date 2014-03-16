#!/bin/bash

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ] ; do SOURCE="$(readlink "$SOURCE")"; done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

echo "Installing dotfiles from $DIR..."

function remove() {
  [ -L "$1" ] && unlink "$1" || rm -rf "$1"
}

pushd ~

remove .bashrc
remove .bash_profile
remove .emacs
remove .emacs.d

ln -s $DIR/bashrc .bashrc
ln -s $DIR/bash_profile .bash_profile
ln -s $DIR/emacs .emacs
ln -s $DIR/emacs.d .emacs.d

echo "Done."

popd
