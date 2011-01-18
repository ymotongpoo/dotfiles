# bash completion
export BASH_COMPLETION_DIR=/opt/local/etc/bash_completion.d

# prompt command
hg_branch() {
    hg branch 2> /dev/null | awk '{print "(hg:" $1 ")"}'
}

git_branch() {
    __git_ps1 '(git:%s)'
}

# setting for prompt
if [ -f $BASH_COMPLETION_DIR/git ]; then
    source $BASH_COMPLETION_DIR/git
    echo "git-completion enabled..."
    PS1="\[\033[0;37m\][\[\033[0;32m\]\t \[\033[1;36m\]\u\[\033[0;37m\]@\h \$(git_branch)\$(hg_branch) \[\033[0;32m\]\w\[\033[0;37m\]]\n\$ "
else
    PS1="\[\033[0;37m\][\[\033[0;32m\]\t \[\033[1;36m\]\u\[\033[0;37m\]@\h \[033[0;32m\]\w\[\033[0;37m\]]\n\$ "
fi

export PS1

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias gvim="/Applications/MacVim.app/Contents/MacOS/MacVim"

alias docs="cd ~/docs"
alias src="cd ~/src"

export MACPORTS_PREFIX=/opt/local
export LANG=ja_jp.UTF-8
export DISPLAY=localhost:0.0
export INFOPATH=/usr/local/info:$INFOPATH
export EDITOR=/usr/bin/vim
export SVN_EDITOR=emacs

export HGENCODING=UTF-8

### for Python
#export PYTHONHOME=$MACPORTS_PREFIX/Library/Frameworks/Python.framework/Versions/2.5/
export VIRTUALENV_BIN=$MACPORTS_PREFIX/Library/Frameworks/Python.framework/Versions/2.6
export PYTHONPATH=$MACPORTS_PREFIX/lib/python2.5/:$PYTHONPATH.
export MANPATH=$MACPORTS_PREFIX/share/man:/usr/local/man:$MANPATH

### for OCaml
export OCAML_VER=3.11.2
export OCAML_HOME=/opt/ocaml/$OCAML_VER

### for OMake
export OMAKE_HOME=/opt/ocaml/omake

export PATH=$OCAML_HOME/bin:$OMAKE_HOME/bin:$MACPORTS_PREFIX/bin:$VIRTUALENV_BIN/bin:$PATH:~/bin:/usr/local/bin:~/bin:.

export WORKON_HOME=$HOME/.virtualenvs
. /opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin/virtualenvwrapper.sh

### for Scala
export SCALA_DOC_HOME=/opt/local/share/scala/doc/scala-devel-docs/api/

### for coherence
export COHERENCE_HOME=/opt/coherence
export CLASSPATH=$COHERENCE_HOME/lib:$CLASSPATH

export JAVA_HOME=/Library/Java/Home
export CATALINA_HOME=/opt/apache-tomcat-6.0.20
