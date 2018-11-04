alias ls='ls --color=tty'
alias la='ls -alhtr'
alias grep='grep --color '
alias ncmpcl='ncmpc -h localhost'
alias vgdb='valgrind --leak-check=full --db-attach=yes --db-command="cgdb -- -nw %f %p" --track-origins=yes'
alias ssh='TERM=xterm ssh'

if [[ -d $ZPLUG_HOME/repos/seebi/dircolors-solarized/ ]]; then
    eval $(dircolors $ZPLUG_HOME/repos/seebi/dircolors-solarized/dircolors.256dark)
fi
