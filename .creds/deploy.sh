#!/bin/bash
gpg -d creds.tar.xz.gpg > creds.tar.xz
tar xJvf creds.tar.xz -C ../creds
stow -t ~ ../creds
