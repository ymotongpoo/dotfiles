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

ZSHRC_EXTRA_PATH="$HOME/bin"

alias start-emacs="emacs --daemon"
alias kill-emacs="emacsclient -e '(kill-emacs)'"
alias ec="emacsclient -nc"

if type "pyenv" > /dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

if type "rbenv" > /dev/null 2>&1; then
  eval "$(rbenv init -)"
fi

if type "nodenv" > /dev/null 2>&1; then
  eval "$(nodenv init -)"
fi

if type "starship" > /dev/null 2>&1; then
  eval "$(starship init zsh)"
fi

if [ -f "$HOME/.corp" ]; then
  source $HOME/.corp
fi

PATH="$ZSHRC_EXTRA_PATH:$ZSHENV_EXTRA_PATH:$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

# configurations for OSC 133
# https://gitlab.freedesktop.org/Per_Bothner/specifications/blob/master/proposals/semantic-prompts.md
_prompt_executing=""
function __prompt_precmd() {
    local ret="$?"
    if test "$_prompt_executing" != "0"
    then
      _PROMPT_SAVE_PS1="$PS1"
      _PROMPT_SAVE_PS2="$PS2"
      PS1=$'%{\e]133;P;k=i\a%}'$PS1$'%{\e]133;B\a\e]122;> \a%}'
      PS2=$'%{\e]133;P;k=s\a%}'$PS2$'%{\e]133;B\a%}'
    fi
    if test "$_prompt_executing" != ""
    then
       printf "\033]133;D;%s;aid=%s\007" "$ret" "$$"
    fi
    printf "\033]133;A;cl=m;aid=%s\007" "$$"
    _prompt_executing=0
}
function __prompt_preexec() {
    PS1="$_PROMPT_SAVE_PS1"
    PS2="$_PROMPT_SAVE_PS2"
    printf "\033]133;C;\007"
    _prompt_executing=1
}
preexec_functions+=(__prompt_preexec)
precmd_functions+=(__prompt_precmd)

