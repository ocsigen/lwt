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

# Adjust opam file
sed "s/^version: \"dev\"/version: \"$VERSION\"/" opam > opam.1
grep -vi oasis opam.1 > opam.2
mv opam.2 opam
rm opam.1

# Remove dev-files
rm -f .jenkins.sh appveyor.yml .travis.yml *.exe configure _oasis

# Commit
git add --all --force
git commit -m "Release $VERSION"
git tag $VERSION

echo "Tag $VERSION created. Run 'git push origin $VERSION' to publish."
