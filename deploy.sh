#!/bin/bash
echo "For .creds/, you will have to run the script in that dir first"
for i in `ls -d */`; do
	stow -t ~ $i
done
