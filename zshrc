# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="ys"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git brew gnu-utils osx ruby rvm github debian gem tmux bundler)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
set -o vi
PATH=$HOME/.cabal/bin:$PATH
PATH=/usr/brlcad/rel-7.12.2/bin:$PATH
PATH=/usr/local/bin:$PATH
PATH=/usr/local/sbin:$PATH
PATH=/usr/texbin:$PATH
PATH=/usr/local/Cellar/smlnj/110.75/libexec/bin:$PATH
PATH=/Applications/Racket\ v5.3.3/bin:$PATH
PATH=$HOME/.rbenv/bin:$PATH
export PATH

alias im="/usr/local/sbin/bitlbee"
alias lx="$HOME/src/xunlei-lixian/lixian_cli.py"
export EDITOR="vim"
export ALTERNATE_EDITOR=""
alias e="/usr/local/bin/emacsclient -t"
alias psg="ps aux | grep"

eval "$(rbenv init -)"
