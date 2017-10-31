# zshrc >= 4.3.10
# 2013.04.02 4.3.11
# 2015.12.15 5.0.5
#
# If you want MacPorts' zsh as login shell, you should run:
# % sudo sh -c "echo '/opt/local/bin/zsh' >> /etc/shells"
# % chsh -s /opt/local/bin/zsh
#
########################################
##################### General settings
########################################
if [ -f "$HOME/.zprofile" ]; then
  source "$HOME/.zprofile"
fi

# LANG
export LANG=ja_JP.UTF-8
export LC_CTYPE=ja_JP.UTF-8

# Emacs like key bind
bindkey -e

# completion
autoload -Uz compinit && compinit

setopt auto_menu
setopt auto_list
setopt list_packed
setopt list_types
setopt magic_equal_subst
setopt print_eight_bit
setopt mark_dirs
setopt numeric_glob_sort

# correct typo
#setopt correct

# auto change directory (i.e. cd foo -> foo)
#setopt auto_cd

# auto directory pushd that you can get dirs list by cd -[tab]
setopt auto_pushd

# no beep sound when complete list displayed
setopt nolistbeep

# history setting
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
#setopt hist_ignore_dups  # ignore duplication command history list
setopt share_history     # share command history data
setopt hist_ignore_space # ignore command if it starts from space 
setopt hist_no_store     # do not store 'history' command in hitory file
setopt extended_history  # record command hit time


########################################
##################### PROMPT settings
########################################

setopt prompt_subst
autoload -Uz add-zsh-hook

# PROMPT
# Tomorrow Night Eighties
# Actual color -----------> Xterm 256
# #2d2d2d Background        235 Grey15          #262626
# #393939 Current Line      237 Grey23          #3a3a3a
# #515151 Selection         240 Grey35          #585858
# #cccccc Foreground        251 Grey78          #c6c6c6
# #999999 Comment           247 Grey62          #9e9e9e
# #f2777a Red               210 LightCoral      #ff8787
# #f99157 Orange            209 Salmon1         #ff875f
# #ffcc66 Yellow            221 LightGoldenrod2 #ffd75f
# #99cc99 Green             154 GreenYellow     #afff00
# #66cccc Aqua              080 MediumTurquoise #5fd7d7
# #6699cc Blue              068 SteelBlue3      #5f87d7
# #cc99cc Purple            135 MediumPurple2   #af5fff
case ${UID} in
0)
    PROMPT="%F{255}[%f%F{161}%DT%* %f%F{99}%n%f%F{255}@%m %f%F{191}%~%f%F{255}]%f
 %# "
    ;;
*)
    PROMPT="%F{251}[%f%F{154}%DT%* %f%F{135}%n%f%F{251}@%m %f%F{221}%~%f%F{251}]%f
 %# "
    ;;
esac

# VCS version and branch info in RPROMPT
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git hg bzr
zstyle ':vcs_info:*' formats '(%s:%b)'
zstyle ':vcs_info:*' actionformats '(%s:%b|%a)'
zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
zstyle ':vcs_info:bzr:*' use-simple true

autoload -Uz is-at-least
if is-at-least 4.3.10; then
  zstyle ':vcs_info:git:*' check-for-changes true
  zstyle ':vcs_info:git:*' stagedstr "+"    
  zstyle ':vcs_info:git:*' unstagedstr "-"  
  zstyle ':vcs_info:git:*' formats '(%s:%b) %c%u'
  zstyle ':vcs_info:git:*' actionformats '(%s:%b|%a) %c%u'
fi

local _pre=''
preexec() {
    _pre="$1"
}

function _update_vcs_info_msg() {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
add-zsh-hook precmd _update_vcs_info_msg
RPROMPT="%1(v|%F{green}%1v%f|)"

# use color less if possible
srchilite="/opt/local/bin/src-hilite-lesspipe.sh"
if [ -f $srchilite ]; then
  export LESS='-R'
  export LESSOPEN='| /opt/local/bin/src-hilite-lesspipe.sh %s'
fi


########################################
##################### Programming Env
########################################
### for development
export C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH
export LD_LIBRARY_PATH

### for Python
export VIRTUALENV_HOME
export VIRTUALENVWRAPPER
export PYTHONPATH
export MANPATH

export EXTRA_PATH
export PATH=.:~/bin:/usr/local/bin:$PATH
export WORKON_HOME=$HOME/.virtualenvs

if [ -n "$GOENVWRAPPER" -a -f "$GOENVWRAPPER" ]; then
  source "$GOENVWRAPPER"
fi

########################################
##################### Utilities 
########################################
# complete process ID
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

# put colors on completion candidates
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS} menu select=2
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z} r:|[._-]=*'
zstyle ':completion:*' completer \
       _complete _match _history _ignored _prefix
zstyle ':completion:*:sudo:*' \
       command-path /sbin /usr/sbin /usr/local/sbin /opt/local/sbin \
       /bin /usr/bin /usr/local/bin /opt/local/sbin

# complete from the cursor position
setopt complete_in_word
# do not expand glob and find candidates from the list
setopt glob_complete
setopt extended_glob

alias l=ls
case "$OSTYPE" in
  darwin*)
    alias ls='ls -G'
    ;;
  linux*)
    alias ls='ls --color'
    ;;
esac

# suffix alias
alias -s go='go run'
alias -s py='python'
alias -s js='node'

### Google Cloud Platform
GCPTOOLS="$HOME/google-cloud-sdk"
if [ -d "$GCPTOOLS" ]; then
  export PATH="$HOME/$GCPTOOLS:$PATH"
  source $HOME/google-cloud-sdk/completion.zsh.inc
fi

export PATH="/usr/local/git/current/bin/":"$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.cask/bin/:$PATH"

export NVM_DIR="/home/ymotongpoo/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
