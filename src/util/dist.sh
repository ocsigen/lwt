#!/bin/bash
#
# dist
# ----
# Copyright : (c) 2012, Jérémie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# Script to build the release

set -e

# Extract project parameters from _oasis
NAME=`oasis query Name 2> /dev/null`
VERSION=`oasis query Version 2> /dev/null`
PREFIX=$NAME-$VERSION

# Clean setup.data and other generated files.
make clean
make distclean

# Create a branch for the release
git checkout -b release-$VERSION

# Generate files
oasis setup

# Set release mode in the Makefile
sed 's/^SETUP := setup-dev.exe.*/SETUP := setup.exe/' Makefile > Makefile.new
mv Makefile.new Makefile

# Adjust opam files
for FILE in `ls *.opam`
do
    sed "s/^version: \"dev\"/version: \"$VERSION\"/" $FILE > $FILE.1
    grep -vi oasis $FILE.1 > $FILE
    rm $FILE.1
done

# Remove dev-files
rm -f .jenkins.sh appveyor.yml .travis.yml *.exe configure _oasis

# Commit
git add --all --force
git rm .gitignore
git commit -m "Release $VERSION"
git tag -a $VERSION

echo "Tag $VERSION created. Run 'git push origin $VERSION' to publish."
