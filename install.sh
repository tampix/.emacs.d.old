#/bin/zsh

set -e

installed=$(which cask)
if -z $installed; then
    echo "Already installed."
    exit 0
fi

get_absolute_path() {
	pushd `dirname $0` > /dev/null
	pwd -P
	popd > /dev/null
}

curl -fsSkL https://raw.github.com/cask/cask/master/go | python

if grep --quiet cask ~/.zshenv; then
    echo "typeset -U path" > ~/.zshenv
    echo "path+=(~/.cask/bin)" >> ~/.zshenv
    source ~/.zshenv
fi

emacsdir=$(get_absolute_path)

cd $emacsdir
git pull --rebase
cask install
