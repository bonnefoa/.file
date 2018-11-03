export EDITOR=vim
export TERM=rxvt-unicode-256color
export FZF_CTRL_T_COMMAND="fd --hidden --follow --exclude ".git" . "$1" "
export FZF_ALT_C_COMMAND="fd --type d --hidden --follow --exclude ".git" . "$1" "

export LESS=" -S -R"
export GOPATH="$HOME/git-repos/golang/"
export FZF_BASE="$GOPATH/src/github.com/junegunn/fzf"

export PYTHONDONTWRITEBYTECODE="1"
export LANG="en_US.utf8"
export LC_ALL="en_US.UTF-8"
export FZF_COMPLETION_OPTS='+c -x'
export AWS_SESSION_TTL=24h
export AWS_ASSUME_ROLE_TTL=1h
