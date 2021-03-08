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
#export LANG=ja_JP.UTF-8
#export LC_CTYPE=ja_JP.UTF-8

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
setopt hist_ignore_dups  # ignore duplication command history list
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

ZSHRC_EXTRA_PATH="$HOME/bin"

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

### Google Cloud Platform
GOOGLE_CLOUD_SDK="$HOME/google-cloud-sdk"
if [ -d "$GOOGLE_CLOUD_SDK" ]; then
  if [ -z "$ZSHRC_EXTRA_PATH" ]; then
    ZSHRC_EXTRA_PATH="$GOOGLE_CLOUD_SDK/bin"
  else
    ZSHRC_EXTRA_PATH="$GOOGLE_CLOUD_SDK/bin:$ZSHRC_EXTRA_PATH"
  fi
  source $GOOGLE_CLOUD_SDK/completion.zsh.inc
fi

if [ -z "$ZSHRC_EXTRA_PATH" ]; then
  ZSHRC_EXTRA_PATH="/usr/local/git/current/bin"
else
  ZSHRC_EXTRA_PATH="/usr/local/git/current/bin:$ZSHRC_EXTRA_PATH"
fi

PATH="$ZSHRC_EXTRA_PATH:$ZSHENV_EXTRA_PATH:$PATH"

alias start-emacs="emacs --daemon"
alias kill-emacs="emacsclient -e '(kill-emacs)'"
alias ec="emacsclient -nc"

rbenv_exists=$(which rbenv)
if [ ! -z "$rbenv_exists" ]; then
  eval "$(rbenv init -)"
fi

nodenv_exists=$(which nodenv)
if [ ! -z "$nodenv_exists" ]; then
  eval "$(rbenv init -)"
fi

starship_exists=$(which starship)
if [ -f "$starship_exists" ]; then
  eval "$(starship init zsh)"
fi

if [ -f "$HOME/.corp" ]; then
  source $HOME/.corp
fi
