#!/bin/bash
# setup OPAM
# wget http://www.ocamlpro.com/pub/opam_installer.sh
# sh ./opam_installer.sh .

if [ "$DESKTOP" == 1 ]; then
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
                   yaourt \               # package manager
                   tmux \                 # terminal multiplexer
		   xclip                  # copy CLI output to X clipboard
    yaourt -S ttf-ricty otf-source-han-code-jp google-chrome
    ;;
esac
fi
