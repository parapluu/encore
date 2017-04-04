#!/usr/bin/env sh

PONY_REPOSITORY="git@github.com:ponylang/ponyc.git"
PONY_BRANCH="release"

set -e # Stop when something goes wrong

while getopts xh name # Check command line arguments
do
	case $name in
		x)destructive=1;;
		h)echo "update.sh merges the pony runtime by the newest version."
			exit 0;;
		*)echo "Invalid arguments";
			exit 1;;
	esac
done

echo "Setting up remote repository 'pony'"
remotes=`git remote`
if [[ $remotes == *'pony'* ]]; then
	git remote set-url pony $PONY_REPOSITORY
else
	git remote add pony $PONY_REPOSITORY
fi

echo "Fetching newest Pony changes"
git fetch pony

echo "Creating temporary branch"
current_branch=`git rev-parse --abbrev-ref HEAD`
tmp_branch="tmp_pony_update"
git checkout -b $tmp_branch 2> /dev/null

echo "Removing old Pony runtime"
path=`git rev-parse --show-toplevel`
git rm -r ${path}/src/runtime/pony/libponyrt/ > /dev/null
git rm -r ${path}/src/runtime/common/ > /dev/null
git commit -m "Temporarily removes Pony Runtime" > /dev/null

echo "Updating the Pony runtime"
git read-tree --prefix=src/runtime/pony/libponyrt/ -u pony/${PONY_BRANCH}:src/libponyrt
git read-tree --prefix=src/runtime/common/ -u pony/${PONY_BRANCH}:src/common
git commit -m "Updates Pony runtime to the ${PONY_BRANCH} from ${PONY_REPOSITORY}" > /dev/null

echo "Merging temporary branch back into the current branch"
git checkout $current_branch 2> /dev/null
git merge --squash $tmp_branch > /dev/null
echo ""
if [ "$(git status --short -uno)" ]; then
	git commit -m "Updates Pony runtime to the ${PONY_BRANCH} from ${PONY_REPOSITORY}" > /dev/null
	echo "The new pony runtime is now in place. Make sure that:
	1. The pony runtime hasn't undergone any changes that should be reflected in the
	Encore runtime
	2. Encore still compiles without any warnings or errors
	3. All Encore tests still complete without any failures"
else
	echo "Pony runtime already up to date with '${PONY_BRANCH}' from ${PONY_REPOSITORY}"
fi
git branch -D $tmp_branch > /dev/null

