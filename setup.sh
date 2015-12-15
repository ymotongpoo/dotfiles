#!/bin/bash
# This directory is supporsed to be in $HOME as .dotfiles
DOTFILES="$HOME"/.dotfiles
cd "$HOME"

# set zshell configuration files
ln -s "$DOTFILES"/.zshrc .zshrc
platform=`uname`
if [ platform = "Linux" ]; then
  ln -s "$DOTFILES"/.zshenv.linux .zshenv
elif [ platform = "Darwin" ]; then
  ln -s "$DOTFILES"/.zshenv.macosx .zshenv
fi

# set editor configuration files
ln -s "$DOTFILES"/.emacs.d .emacs.d
ln -s "$DOTFILES"/.vim .vim

# set terminal multiplexer configuration files
ln -s "$DOTFILES"/.tmux.conf .tmux.conf
ln -s "$DOTFILES"/.screenrc .screenrc

# create private bin directory
mkdir "$HOME"/bin

cd "$HOME"/bin

# setup OPAM
# wget http://www.ocamlpro.com/pub/opam_installer.sh
# sh ./opam_installer.sh .

