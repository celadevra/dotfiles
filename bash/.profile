# settings
set -o vi

# change window title in X Terminals
case ${TERM} in
	xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix|konsole*)
		PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
		;;
	screen*)
		PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\033\\"'
		;;
esac

alias ls='ls -G'
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'
unset use_color safe_term match_lhs sh

# MasterPassword settings
export MP_FULLNAME="Haoyang Xu"


# completion

xhost +local:root > /dev/null 2>&1

complete -cf sudo

# set editors and other default apps
export GIT_EDITOR="vim"
export EDITOR="emacsclient -a '' -t"
export BROWSER=/usr/bin/firefox
export LC_CTYPE=zh_CN.UTF-8
export PATH=$(ruby -rubygems -e "puts Gem.user_dir")/bin:$PATH
# Aliases
alias 'cp'='cp -i' # confirm before overwriting something
alias 'df'='df -h' # human readable sizes
alias free='free -m'                      # show sizes in MB
alias more=less
alias 'ls'='ls -G'
alias 'll'='ls -l'
alias 'la'='ls -a'
alias con='nano $HOME/.i3/config'
alias comp='nano $HOME/.config/compton.conf'
alias fixit='sudo rm -f /var/lib/pacman/db.lck'
alias inst='sudo pacman -S'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias mirrors='sudo pacman-mirrors -g'
alias printer='system-config-printer'
alias update='yaourt -Syua'
alias v='nvim'
alias vim='nvim'
alias sshpi='ssh alarm@alarmpi'
alias conf='stow -t ~'


alias 'gst'='git status'
alias 'ga'='git add'
alias 'gaa'='git add -A'
alias 'gc'='git commit'
alias 'gb'='git branch'
alias 'gcb'='git checkout -b'
alias 'gp'='git push'

alias 'e'='emacsclient -t -a ""'
alias 'emacs'='emacs -nw'

alias 'be'='bundle exec'
alias 'venv'='source env/bin/activate' # enter python virtualenv

alias 'psg'='ps aux | grep'
shopt -s expand_aliases

# Enable history appending instead of overwriting.  #139609
shopt -s histappend

# better yaourt colors
export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"

# import other scripts
#source ~/.bash_prompt
source ~/.bash_functions
source /usr/local/opt/chruby/share/chruby/chruby.sh
source /usr/local/opt/chruby/share/chruby/auto.sh
