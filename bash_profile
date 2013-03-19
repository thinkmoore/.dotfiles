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

export EDITOR=emacs
export TERM=xterm-256color

# Load standard configuration information
if [ -f ~/.bashrc ]; then
   echo "Running ~/.bash_profile"
   source ~/.bashrc
fi
