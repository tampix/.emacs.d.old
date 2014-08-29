#!/usr/bin/env zsh

set -e

get_absolute_path() {
	pushd `dirname $0` > /dev/null
	pwd -P
	popd > /dev/null
}

command_exists() {
    command -v $1 > /dev/null 2>&1
}

if [ ! -d ~/.cask/ ]; then
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python
fi
if ! command_exists cask; then
	echo "path+=(~/.cask/bin)" >> ~/.zshenv
	typeset -gxU path
	path+=(~/.cask/bin)
fi

emacsdir=$(get_absolute_path)

cd $emacsdir
git pull
cask install
