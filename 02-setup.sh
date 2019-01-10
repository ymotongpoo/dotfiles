#!/bin/bash
#####################################
# Run this script after 01-setup.sh #
#####################################

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

##### Install go packages
declare -a repos=(
    "golang.org/x/tools/cmd/golsp"
    "github.com/rogpeppe/godef"
    "golang.org/x/tools/cmd/godoc"
    "golang.org/x/tools/cmd/goimports"
    "golang.org/x/tools/cmd/guru"
    "github.com/Songmu/goxz/cmd/goxz"
    "github.com/uudashr/gopkgs/cmd/gopkgs"
    "github.com/ramya-rao-a/go-outline"
    "github.com/acroca/go-symbols"
    "golang.org/x/tools/cmd/gorename"
    "sourcegraph.com/sqs/goreturns"
    "github.com/derekparker/delve/cmd/dlv"
    "github.com/kisielk/errcheck"
    "github.com/davidrjenni/reftools/cmd/fillstruct"
    "github.com/jstemmer/gotags"
    "github.com/cweill/gotests/..."
    "github.com/fatih/gomodifytags"
    "github.com/huydx/hget"
    "mvdan.cc/sh/cmd/shfmt"
    "github.com/FiloSottile/mkcert"
)

for repo in $repos; do
    go get -u "$repo"
    go install "$repo"
done


##### Install Visual Studio Code extensions
if [ platform = "Linux" ]; then
    cat "./vscode/extensions.txt" | while read line
    do
        code --install-extension $line
    done
fi
