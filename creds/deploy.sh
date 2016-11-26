#!/bin/bash
gpg -d creds.tar.xz.gpg > creds.tar.xz
tar xJvf creds.tar.xz
stow -t ~ .
