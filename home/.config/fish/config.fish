if test -d "$HOME/.pyenv"
 set -x PATH "$HOME/.pyenv/bin" $PATH
 status --is-interactive; and . (pyenv init -|psub)
 status --is-interactive; and . (pyenv virtualenv-init -|psub)
end

if test -d "$HOME/go"
   set -x GOPATH "$HOME/go"
   set -x fish_user_paths "/usr/local/go/bin" "$HOME/go/bin" $fish_user_paths
end

set -x fish_user_paths "$HOME/bin" $fish_user_paths
source "$HOME/.homesick/repos/homeshick/homeshick.fish"

source "$HOME/bin/virtual.fish"

set -x PIP_USE_WHEEL "true"
set -x PIP_WHEEL_DIR "$HOME/.pip/wheels"
set -x PIP_FIND_LINKS "$HOME/.pip/wheels"
set -x PIP_DOWNLOAD_CACHE "$HOME/.pip/cache"

if test -e "$HOME/.config/fish/local.fish"
  source "$HOME/.config/fish/local.fish"
end

function tmux
  env TERM=xterm-256color tmux
end

function emacs
  env TERM=xterm-256color emacs $argv
end

function ack
  ack-grep $argv
end

function e
  emacsclient -t $argv
end

# Clean up $PATH.

function inc --description 'Increase the value of variable' --no-scope-shadowing
  set $argv[1] (expr $$argv[1] + 1)
end

function varclear --description 'Remove duplicates from environment variable'
    if test (count $argv) = 1
        set -l newvar
        set -l count 0
        for v in $$argv
            if contains -- $v $newvar
                inc count
            else
                set newvar $newvar $v
            end
        end
        set $argv $newvar
    else
        for a in $argv
            varclear $a
        end
    end
end

# Add this back when it works again.
# set -x fish_user_paths $fish_user_paths /usr/local/bin
set -g -x PATH /usr/local/bin /usr/local/sbin $PATH
varclear PATH