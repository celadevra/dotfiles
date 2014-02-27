#!/bin/bash

# deploy files to the right paths

cp -p zshrc $HOME/.zshrc

cp -p goobookrc $HOME/.goobookrc

cp -p muttrc $HOME/.muttrc

cp -p tmux.conf $HOME/.tmux.conf

cp -p prelude-personal/*.el $HOME/.emacs.d/personal/

cp -p vimrc.local $HOME/.vimrc.local

cp -pr prelude-snippets/* $HOME/.emacs.d/snippets/

cp -pr irssi $HOME/.irssi
