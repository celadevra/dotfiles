#!/bin/bash

# deploy files to the right paths

cp -p zshrc $HOME/.zshrc

cp -p tmux.conf $HOME/.tmux.conf

cp -p prelude-personal/*.el $HOME/.emacs.d/personal/
