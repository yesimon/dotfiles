# the basics
: ${HOME=~}
: ${LOGNAME=$(id -un)}
: ${UNAME=$(uname)}

# complete hostnames from this file
: ${HOSTFILE=~/.ssh/known_hosts}

# readline config
: ${INPUTRC=~/.inputrc}

# shell opts. see bash(1) for details
shopt -s cdspell >/dev/null 2>&1
shopt -s extglob >/dev/null 2>&1
shopt -s histappend >/dev/null 2>&1
shopt -s hostcomplete >/dev/null 2>&1
shopt -s no_empty_cmd_completion >/dev/null 2>&1

# ----------------------------------------------------------------------
# PATH
# ----------------------------------------------------------------------

# we want the various sbins on the path along with /usr/local/bin
PATH="$PATH:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/lib/python"
PATH="/usr/local/bin:$PATH"

test -d "$HOME/.cabal/bin" &&
PATH="$HOME/.cabal/bin:$PATH"

# RVM if available
test -r "$HOME/.rvm/scripts/rvm" &&
. "$HOME/.rvm/scripts/rvm"

# put ~/bin on PATH if you have it
test -d "$HOME/bin" &&
PATH="$HOME/bin:$PATH"
test -d "$HOME/bin/x86_64" &&
PATH="$HOME/bin/x86_64:$PATH"

test -d "/usr/local/lib/node_modules" &&
NODE_PATH="/user/local/lib/node_modules:$NODE_PATH"

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    # virtualenvwrapper default location
    test -r "/usr/local/bin/virtualenvwrapper.sh" &&
    source /usr/local/bin/virtualenvwrapper.sh
elif [[ "$unamestr" == 'Darwin' ]]; then
    if type brew > /dev/null 2>&1; then
        source `brew --prefix`/etc/profile.d/z.sh
    fi

    test -d "/usr/local/share/npm/bin" &&
    PATH="/usr/local/share/npm/bin:$PATH"

    # virtualenvwrapper os x homebrew location
    test -r "/usr/local/share/python/virtualenvwrapper.sh" &&
    source /usr/local/share/python/virtualenvwrapper.sh
fi


# Enable en_US locale w/ utf-8 encodings if not already configured
: ${LANG:="en_US.UTF-8"}
: ${LANGUAGE:="en"}
: ${LC_CTYPE:="en_US.UTF-8"}
: ${LC_ALL:="en_US.UTF-8"}
export LANG LANGUAGE LC_CTYPE LC_ALL

# Always use PASSIVE mode ftp
: ${FTP_PASSIVE:=1}
export FTP_PASSIVE

# Ignore backups, CVS directories, python bytecode, vim swap files
FIGNORE="~:CVS:#:.pyc:.swp:.swa:apache-solr-*"

# Save more history, no dups
export HISTCONTROL=erasedups
export HISTSIZE=10000

# Don't exit shell on accidental Ctrl-D
export IGNOREEOF=

# See what we have to work with ...
HAVE_VIM=$(command -v vim)

# EDITOR
test -n "$HAVE_VIM" &&
EDITOR=vim ||
EDITOR=vi
export EDITOR

# Make nice screen titles.
if [ "$TERM" = "screen" ]; then
  screen_set_window_title () {
    local HPWD="$PWD"
    case $HPWD in
      $HOME) HPWD="~";;
      $HOME/*) HPWD="~${HPWD#$HOME}";;
    esac
    printf '\ek%s\e\\' "$HPWD"
  }
  PROMPT_COMMAND="screen_set_window_title; $PROMPT_COMMAND"
fi

# Add recurisve cd ...
function cd()
{
  local -ri n=${#*};
  if [[ n -eq 0 ]]; then
      builtin cd;
  else
      local e="s:\.\.\.:../..:g";
      builtin cd "${@:1:$n-1}" $(sed -e$e -e$e -e$e <<< "${!n}");
  fi
}

# Handy Extract Program
extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1        ;;
            *.tar.gz)    tar xvzf $1     ;;
            *.bz2)       bunzip2 $1       ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1     ;;
            *.tar)       tar xvf $1        ;;
            *.tbz2)      tar xvjf $1      ;;
            *.tgz)       tar xvzf $1       ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1    ;;
            *)           echo "'$1' cannot be extracted via >extract<" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# Usage: puniq [<path>]
# Remove duplicate entries from a PATH style value while retaining
# the original order. Use PATH if no <path> is given.
#
# Example:
#   $ puniq /usr/bin:/usr/local/bin:/usr/bin
#   /usr/bin:/usr/local/bin
puniq () {
    echo "$1" |tr : '\n' |nl |sort -u -k 2,2 |sort -n |
    cut -f 2- |tr '\n' : |sed -e 's/:$//' -e 's/^://'
}

# Fuzzy filename search
function ff () {
    find . -type f -iname '*'"$@"'*' ;
}

# ----------------------------------------------------------------------
# PROMPT
# ----------------------------------------------------------------------

# Show nonzero return codes
PS1='`_ret=$?; if test $_ret -ne 0; then echo "$_ret:"; set ?=$_ret; unset _ret; fi`\u@\h:\W\$ '

# source ~/.shenv now if it exists
test -r ~/.shenv &&
source ~/.shenv

PATH=$(puniq $PATH)
export PATH

export CLICOLOR=1
#export LSCOLORS=ExFxCxDxBxegedabagacad
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

# ----------------------------------------------------------------------
# ALIASES / FUNCTIONS
# ----------------------------------------------------------------------

# disk usage with human sizes and minimal depth
alias fn='find . -name'
alias hi='history | tail -20'
alias ls='ls -Bph'
alias ll='ls -la'
alias pgrep='pgrep -f -l'
alias pkill='pkill -f'
alias shell_name="ps -p $$ | tail -1 | awk '{print $NF}'"
