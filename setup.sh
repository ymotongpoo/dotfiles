#!/bin/bash
# This directory is supporsed to be in $HOME as .dotfiles
DOTFILES="$HOME"/.dotfiles
cd "$HOME"

##### set zshell configuration files
ln -s "$DOTFILES"/.zshrc .zshrc
platform=`uname`
if [ platform = "Linux" ]; then
  ln -s "$DOTFILES"/.zshenv.linux .zshenv
elif [ platform = "Darwin" ]; then
  ln -s "$DOTFILES"/.zshenv.macos .zshenv
  ln -s "$DOTFILES"/.zprofile.macos .zprofile
fi

##### set editor configuration files
ln -s "$DOTFILES"/.emacs.d "$HOME"/.emacs.d
ln -s "$DOTFILES"/.vim "$HOME"/.vim

##### install cask for Emacs
cd "$HOME"
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

##### set terminal multiplexer configuration files
ln -s "$DOTFILES"/.screenrc "$HOME"/.screenrc
if [ platform = "Linux" ]; then
  ln -s "$DOTFILES"/.tmux.conf.linux "$HOME"/.tmux.conf
elif [ platform = "Darwin" ]; then
  ln -s "$DOTFILES"/.tmux.conf.macos "$HOME"/.tmux.conf
fi

##### create private bin directory
mkdir "$HOME"/bin

##### Download and install useful tools
tmpdir="$HOME/bin/tmp"
mkdir "$tmpdir"
cd "$tmpdir"

if [ $platform = "Linux" ]; then
  wget "https://github.com/monochromegane/the_platinum_searcher/releases/download/v2.0.2/pt_linux_amd64.tar.gz"
  wget "https://github.com/peco/peco/releases/download/v0.3.5/peco_linux_amd64.tar.gz"
  wget "https://drive.google.com/uc?id=0B3X9GlR6Embnb095MGxEYmJhY2c"
  wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.30.2/install.sh | bash
elif [ $platform = "Darwin" ]; then
  curl -O "https://github.com/monochromegane/the_platinum_searcher/releases/download/v2.0.2/pt_linux_amd64.tar.gz"
  curl -O "https://github.com/peco/peco/releases/download/v0.3.5/peco_darwin_amd64.zip"
  curl -O -L "https://drive.google.com/uc?id=0B3X9GlR6EmbnVjIzMDRqck1aekE" --output drive
  curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.30.2/install.sh | bash
fi

##### set Go workspace
goworkspace="$HOME/src/go/workspace"
mkdir -p "$goworkspace"
cd "$goworkspace"
export GOPATH="$goworkspace"

# go get -u golang.org/x/tools/cmd/goimports
# go get -u golang.org/x/tools/cmd/oracle
# go get -u github.com/rogpeppe/godef
# go get -u github.com/nsf/gocode


# setup OPAM
# wget http://www.ocamlpro.com/pub/opam_installer.sh
# sh ./opam_installer.sh .

