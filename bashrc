# If not running interactively, don't fo anything
[ -z "$PS1" ] && return

# colorize the CLI                                                                                       
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

bind '"\e[A"':history-search-backward
bind '"\e[B"':history-search-forward

# append history to the file,                                                                            
shopt -s histappend
export PROMPT_COMMAND='history -a'
export HISTIGNORE="&:ls:[bf]g:exit:cd"
export HISTCONTROL="ignoredups:erasedups"
export FIGNORE="~"

export EDITOR=aquamacs
export TERM=xterm-256color

# don't duplicate lines in the history
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# keep history length reasonable
HISTSIZE=100000
HISTFILESIZE=200000

# check the window size after each command and, if necessary, update
# the values of LINES and COLUMNS
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set the prompt
PS1="\`if [ \$? = 0 ]; then echo \[\e[33m\]^_^ \[\e[0m\]; else echo \[\e[31m\]o_O \[\e[0m\]; fi\`[\u@\h:\w]\\$ "

# colors
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Add an "alert" alias for long running commands.  Use like so:                                 
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

alias emacs='aquamacs'

# Load local configuration
if [ -f ~/.bash_host ]; then
   echo "Running ~/.bash_host"
   source ~/.bash_host
fi
