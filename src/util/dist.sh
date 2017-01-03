#!/bin/bash
#
# dist
# ----
# Copyright : (c) 2012, Jérémie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# Script to build the release

set -e
set -x

# Extract project version from _oasis.
VERSION=`oasis query Version 2> /dev/null`

# Clean setup.data and other generated files.
make clean
make distclean

# Create a branch for the release.
git checkout -b release-$VERSION

# Generate files.
oasis setup

# Set release mode in the Makefile
sed 's/^SETUP := setup-dev.exe.*/SETUP := setup.exe/' Makefile > Makefile.new
mv Makefile.new Makefile

# Set the version in lwt.opam to the version in _oasis, and remove the oasis
# dependency.
sed "s/^version: \"dev\"/version: \"$VERSION\"/" lwt.opam > lwt.opam.1
grep -vi oasis lwt.opam.1 > lwt.opam
rm lwt.opam.1

# Set the version in additional packages based on their META files, and remove
# the '| "dev"' constraint on lwt.
for FILE in `ls lwt_*.opam`
do
    PACKAGE=${FILE#lwt_}
    PACKAGE=${PACKAGE%.opam}
    PACKAGE_VERSION=`cat src/$PACKAGE/META | grep version`
    PACKAGE_VERSION=`echo $PACKAGE_VERSION | egrep -o '[0-9]+\.[0-9]+\.[0-9]+'`
    sed "s/^version: \"dev\"/version: \"$PACKAGE_VERSION\"/" $FILE > $FILE.1
    sed "s/\"lwt\" {(/\"lwt\" {/" $FILE.1 > $FILE
    sed "s/) | \"dev\"//" $FILE > $FILE.1
    mv $FILE.1 $FILE
done

# Remove development files.
rm -f \
    .jenkins.sh appveyor.yml .travis.yml *.exe configure _oasis .merlin \
    src/util/appveyor-build.sh src/util/appveyor-install.sh src/util/dist.sh \
    src/util/fetch-dependees.py src/util/travis.sh

# Commit
git add --all --force
git rm .gitignore
git commit -m "Release $VERSION"
git tag -a $VERSION

echo "Tag $VERSION created. Run 'git push origin $VERSION' to publish."
