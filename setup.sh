#!/bin/bash

####### get_os_info #######
# Copyright (c) 2016 Kohei Arao
# https://github.com/koara-local/dotfiles/edit/master/bin/get_os_info
# Released under the Unlicense
# http://unlicense.org/

# Get OS bit
# 32bit : i686
# 64bit : x86_64
get_os_bit() {
    echo $(uname -m);
}
# Get Linux distribution name
get_os_distribution() {
    if   [ -e /etc/debian_version ] ||
             [ -e /etc/debian_release ]; then
        # Check Ubuntu or Debian
        if [ -e /etc/lsb-release ]; then
            # Ubuntu
            distri_name="ubuntu"
        else
            # Debian
            distri_name="debian"
        fi
    elif [ -e /etc/fedora-release ]; then
        # Fedra
        distri_name="fedora"
    elif [ -e /etc/redhat-release ]; then
        if [ -e /etc/oracle-release ]; then
            # Oracle Linux
            distri_name="oracle"
        else
            # Red Hat Enterprise Linux
            distri_name="redhat"
        fi
    elif [ -e /etc/arch-release ]; then
        # Arch Linux
        distri_name="arch"
    elif [ -e /etc/turbolinux-release ]; then
        # Turbolinux
        distri_name="turbol"
    elif [ -e /etc/SuSE-release ]; then
        # SuSE Linux
        distri_name="suse"
    elif [ -e /etc/mandriva-release ]; then
        # Mandriva Linux
        distri_name="mandriva"
    elif [ -e /etc/vine-release ]; then
        # Vine Linux
        distri_name="vine"
    elif [ -e /etc/gentoo-release ]; then
        # Gentoo Linux
        distri_name="gentoo"
    else
        # Other
        echo "unkown distribution"
        distri_name="unkown"
    fi
    echo ${distri_name}
}

# Get distribution and bit
get_os_info() {
   echo $(get_os_distribution) $(get_os_bit)
}
####### get_os_info end #######

declare -a info=($(get_os_info))

# This directory is supporsed to be in $HOME as .dotfiles
DOTFILES="$HOME"/.dotfiles
cd "$HOME"

##### set zshell configuration files
ln -s "$DOTFILES"/.zshrc .zshrc
platform=`uname`
if [ platform = "Linux" ]; then
  ln -s "$DOTFILES"/.zshenv.linux "$HOME"/.zshenv
  ln -s "$DOTFILES"/.Xresources "$HOME"/.Xresources
elif [ platform = "Darwin" ]; then
  ln -s "$DOTFILES"/.zshenv.macos "$HOME"/.zshenv
  ln -s "$DOTFILES"/.zprofile.macos "$HOME"/.zprofile
fi

##### set editor configuration files
ln -s "$DOTFILES"/.emacs.d "$HOME"/.emacs.d
ln -s "$DOTFILES"/.vim "$HOME"/.vim

##### set terminal multiplexer configuration files
ln -s "$DOTFILES"/.screenrc "$HOME"/.screenrc
if [ platform = "Linux" ]; then
    ln -s "$DOTFILES"/.tmux.conf.linux "$HOME"/.tmux.conf
elif [ platform = "Darwin" ]; then
    ln -s "$DOTFILES"/.tmux.conf.macos "$HOME"/.tmux.conf
fi

##### set i3 setting
if [ platform = "Linux" ]; then
    ln -s "$DOTFILES"/i3config "$HOME"/.config/i3/config
    ln -s "$DOTFILES"/i3status.conf "$HOME"/.config/i3status/config
fi

##### create private bin directory
mkdir "$HOME"/bin

##### Download and install useful tools
tmpdir="$HOME/bin/tmp"
mkdir "$tmpdir"
cd "$tmpdir"

if [ $platform = "Linux" ]; then
  wget "https://github.com/monochromegane/the_platinum_searcher/releases/download/v2.1.5/pt_linux_amd64.tar.gz"
  wget "https://github.com/peco/peco/releases/download/v0.5.3/peco_linux_amd64.tar.gz"
  wget "https://drive.google.com/uc?export=download&id=0B3X9GlR6Embnb095MGxEYmJhY2c" -o drive
  wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash
elif [ $platform = "Darwin" ]; then
  curl -O "https://github.com/monochromegane/the_platinum_searcher/releases/download/v2.1.5/pt_linux_amd64.tar.gz"
  curl -O "https://github.com/peco/peco/releases/download/v0.5.3/peco_darwin_amd64.zip"
  curl -O -L "https://drive.google.com/uc?id=0B3X9GlR6EmbnVjIzMDRqck1aekE" --output drive
  curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash
fi

##### set up Go and Go workspace
GO_VER=1.10.1
if [ $platform = "Linux" ]; then
  sudo mkdir -p /opt/go
  sudo chmod $USER /opt/go
  cd /opt/go
  GO_LINUX_BINARY="go${GO_VER}.linux-amd64.tar.gz"
  wget "https://dl.google.com/go/${GO_LINUX_BINARY}"
  tar xz "${GO_LINUX_BINARY}" "go${GO_VER}"
  rm "${GO_LINUX_BINARY}"
elif [ $platform = "Darwin" ]; then
  sudo mkdir -p /opt/go
  sudo chmod $USER /opt/go
  cd /opt/go
  GO_MAC_BINARY="go${GO_VER}.darwn-amd64.tar.gz"
  wget "https://dl.google.com/go/${GO_MAC_BINARY}"
  tar xz "${GO_MAC_BINARY}" "go${GO_VER}"
  rm "${GO_MAC_BINARY}"
fi

goworkspace="$HOME/src/go/workspace"
mkdir -p "$goworkspace"
cd "$goworkspace"
export GOPATH="$goworkspace"

declare -a repos=(
    "github.com/nsf/gocode"
    "github.com/rogpeppe/godef"
    "golang.org/x/tools/cmd/godoc"
    "golang.org/x/tools/cmd/goimports"
    "golang.org/x/tools/cmd/guru"
    "github.com/Songmu/goxz/cmd/goxz"
)

for repo in repos; do
    go get -u -v "$repo"
    go install "$repo"
done

# setup OPAM
# wget http://www.ocamlpro.com/pub/opam_installer.sh
# sh ./opam_installer.sh .

case ${info[0]} in
"arch")
    echo "arch linux additional setup"
    sudo pacman -S i3 i3lock \            # window manager
                   feh \                  # viewer
                   pulseaudio-control \   # audio control
                   xbacklight \           # LCD brightness control
                   playerctl \            # video/music player control
                   networkmanager \       # network manager
                   rxvt-unicode \         # terminal
                   pcmanfm \              # file manager
                   emacs vim \            # editors
                   yaourt                 # package manager
    yaourt -S ttf-ricty otf-source-han-code-jp google-chrome
    ;;
"ubuntu" | "debian")
    echo "ubuntu or degian additional setup"
    sudo apt-get install i3 i3lock trash-cli fonts-noto-cjk fonts-noto-cjk-extra google-chrome-stable
    ;;
esac
