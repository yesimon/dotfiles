set -g -x PATH /usr/local/bin /usr/local/sbin $PATH
switch (uname)
case Linux
  set -x SHELL /usr/bin/fish
  set -x LESSOPEN "|/usr/bin/lesspipe %s"
case Darwin
  set -x SHELL /usr/local/bin/fish
  set -x LESSOPEN "|/usr/local/bin/lesspipe.sh %s"
end

# Python setup
if test -d "$HOME/.pyenv"
  set -x PATH "$HOME/.pyenv/bin" $PATH
  status --is-interactive; and . (pyenv init -|psub)
  # status --is-interactive; and . (pyenv virtualenv-init -|psub)
end

if test -d "$HOME/.local/bin"
  set -x fish_user_paths "$HOME/.local/bin" $fish_user_paths
end

if test -e "$HOME/bin/virtual.fish"
  source "$HOME/bin/virtual.fish"
end

set -x PIP_USE_WHEEL "true"
set -x PIP_WHEEL_DIR "$HOME/.pip/wheels"
set -x PIP_FIND_LINKS "$HOME/.pip/wheels"

# Go lang setup
if test -d "$HOME/go"
  set -x GOPATH "$HOME/go"
  set -x fish_user_paths "$HOME/go/bin" $fish_user_paths
  if test -d "/usr/local/opt/go"
    set -x fish_user_paths "/usr/local/opt/go/libexec/bin" $fish_user_paths
  else if test -d "/usr/local/go/bin"
    set -x fish_user_paths "/usr/local/go/bin" $fish_user_paths
  end
end

# Perl setup
# Set up perl's local::lib
if test -d "$HOME/perl5"
  eval (perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)
end

set -x fish_user_paths "$HOME/bin" $fish_user_paths
source "$HOME/.homesick/repos/homeshick/homeshick.fish"

if test -d "$HOME/local/bin"
  set -x fish_user_paths "$HOME/local/bin" $fish_user_paths
end

if test -e "$HOME/.config/fish/local.fish"
  source "$HOME/.config/fish/local.fish"
end

function tmux
  env TERM=xterm-256color tmux $argv
end

function emacs
  env TERM=xterm-256color emacs $argv
end

if [ -f /etc/debian_version ]
  function ack
    ack-grep $argv
  end
end

function e
  emacsclient -t $argv
end

function cd-python
  cd (python -c "import os.path as _, $argv[1]; \
    print(_.dirname(_.realpath($argv[1].__file__[:-1])))")
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

function fish_load_colors --description 'Restore colors'
  set fish_color_autosuggestion 777
  set fish_color_command blue
  set fish_color_comment red
  set fish_color_cwd green
  set fish_color_cwd_root red
  set fish_color_end green
  set fish_color_error red --bold
  set fish_color_escape cyan
  set fish_color_history_current cyan
  set fish_color_host -o cyan
  set fish_color_match cyan
  set fish_color_normal normal
  set fish_color_operator cyan
  set fish_color_param yellow
  set fish_color_quote yellow
  set fish_color_redirection normal
  set fish_color_search_match --background=111111
  set fish_color_selection --background=purple
  set fish_color_status red
  set fish_color_user -o green

  set fish_color_valid_path --underline
  set fish_pager_color_completion white
  set fish_pager_color_description white
  set fish_pager_color_prefix blue
  set fish_pager_color_progress cyan
end

fish_load_colors
# Add this back when it works again.
# set -x fish_user_paths $fish_user_paths /usr/local/bin
varclear PATH
