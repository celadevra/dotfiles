#!/bin/bash
for i in `ls -d */`; do
	stow -t ~ -D $i
done
