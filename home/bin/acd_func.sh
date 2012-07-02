# do ". acd_func.sh"
# acd_func 1.0.5, 10-nov-2004
# petar marinov, http:/geocities.com/h2428, this is public domain

_acd_func()
{
  local x2 the_new_dir adir index
  local -i cnt

  if [[ $1 ==  "--" ]]; then
    dirs -v
    return 0
  fi

  the_new_dir=$1
  [[ -z $1 ]] && the_new_dir=$HOME

  if [[ ${the_new_dir:0:1} == '-' ]]; then
    #
    # Extract dir N from dirs
    index=${the_new_dir:1}
    [[ -z $index ]] && index=1
    adir=$(dirs +$index)
    [[ -z $adir ]] && return 1
    the_new_dir=$adir
  fi

  #
  # '~' has to be substituted by ${HOME}
  [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}"

  #
  # Now change to the new dir and add to the top of the stack
  builtin pushd "${the_new_dir}" &>/dev/null
  [[ $? -ne 0 ]] && return 1
  the_new_dir=$(pwd)

  #
  # Trim down everything beyond 11th entry
  builtin popd -n +11 &>/dev/null

  #
  # Remove any other occurence of this dir, skipping the top of the stack
  for ((cnt=1; cnt <= 10; cnt++)); do
    x2=$(dirs +${cnt} 2>/dev/null)
    [[ $? -ne 0 ]] && return 0
    [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}"
    if [[ "${x2}" == "${the_new_dir}" ]]; then
      builtin popd -n +$cnt &>/dev/null
      cnt=cnt-1
    fi
  done

  return 0
}

function cd()
{
  local -ri n=${#*};
  if [ $n -eq 0 -o -d "${!n}" -o "${!n}" == "-"  -o "${!n}" == "--" ]; then
    _acd_func "$@"
  else
    local e="s:\.\.\.:../..:g";
    builtin cd "${@:1:$n-1}" $(sed -e$e -e$e -e$e <<< "${!n}");
  fi
}

function d() {
  if [[ $# < 1 ]]; then
    _acd_func --
  else
    _acd_func "-${1}"
  fi
}

alias 1='_acd_func -1'
alias 2='_acd_func -2'
alias 3='_acd_func -3'
alias 4='_acd_func -4'
alias 5='_acd_func -5'
alias 6='_acd_func -6'
alias 7='_acd_func -7'
alias 8='_acd_func -8'
alias 9='_acd_func -9'
alias 10='_acd_func -10'
alias 11='_acd_func -11'


if [[ $BASH_VERSION > "2.05a" ]]; then
  # ctrl+w shows the menu
  bind -x "\"\C-w\":_acd_func -- ;"
fi