#!/bin/bash

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ] ; do SOURCE="$(readlink "$SOURCE")"; done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

echo "Installing dotfiles from $DIR..."

function remove() {
  [ -L "$1" ] && unlink "$1" || rm -rf "$1"
}

pushd ~

remove .emacs
remove .emacs.d
remove .zshrc
remove .oh-my-zsh

ln -s $DIR/emacs .emacs
ln -s $DIR/emacs.d .emacs.d
ln -s $DIR/zshrc .zshrc
ln -s $DIR/oh-my-zsh .oh-my-zsh

echo "Done."

popd