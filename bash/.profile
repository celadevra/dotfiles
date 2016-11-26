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

use_color=true

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.  Use internal bash
# globbing instead of external grep binary.
safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
match_lhs=""
[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
	&& type -P dircolors >/dev/null \
	&& match_lhs=$(dircolors --print-database)
[[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

if ${use_color}; then
	# Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
	if type -P dircolors >/dev/null ; then
		if [[ -f ~/.dir_colors ]] ; then
			eval $(dircolors -b ~/.dir_colors)
		elif [[ -f /etc/DIR_COLORS ]] ; then
			eval $(dircolors -b /etc/DIR_COLORS)
		fi
	fi

	if [[ ${EUID} == 0 ]] ; then
		PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
	else
		PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\] $(git_branch)]\$\[\033[00m\] '
	fi

	alias ls='ls --color=auto'
	alias grep='grep --colour=auto'
	alias egrep='egrep --colour=auto'
	alias fgrep='fgrep --colour=auto'
else
	if [[ ${EUID} == 0 ]] ; then
		# show root@ when we don't have colors
		PS1='\u@\h \W \$ '
	else
		PS1='\u@\h \w \$ '
	fi
fi

unset use_color safe_term match_lhs sh

# MasterPassword settings
export MP_FULLNAME="Haoyang Xu"


# completion

xhost +local:root > /dev/null 2>&1

complete -cf sudo

# set editors and other default apps
export GIT_EDITOR="vim"
export EDITOR="emacsclient -a -t"
export BROWSER=/usr/bin/firefox
export LC_CTYPE=zh_CN.UTF-8
export PATH=$(ruby -rubygems -e "puts Gem.user_dir")/bin:$PATH
# Aliases
alias 'cp'='cp -i' # confirm before overwriting something
alias 'df'='df -h' # human readable sizes
alias free='free -m'                      # show sizes in MB
alias more=less
alias 'ls'='ls --color'
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
alias sshpi='ssh alarm@alarmpi'
alias conf='stow -t ~'


alias 'gst'='git status'
alias 'ga'='git add'
alias 'gaa'='git add -A'
alias 'gc'='git commit'
alias 'gp'='git push'

alias 'e'='emacsclient -t'
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
source /usr/share/chruby/chruby.sh
source /usr/share/chruby/auto.sh

