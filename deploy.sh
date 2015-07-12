#!/bin/bash

# deploy files to the right paths

cp -p zshrc $HOME/.zshrc

cp -p goobookrc $HOME/.goobookrc

cp -p muttrc $HOME/.muttrc

cp -p tmux.conf $HOME/.tmux.conf

cp -p prelude-personal/*.el $HOME/.emacs.d/personal/

cp -p vimrc $HOME/.vimrc

cp -pR prelude-snippets/* $HOME/.emacs.d/snippets/

cp -pR irssi $HOME/.irssi

cp -R vim/ $HOME/.vim
