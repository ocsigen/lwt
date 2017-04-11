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
sed "s/^version: \"dev\"/version: \"$VERSION\"/" opam/opam > opam/opam.1
grep -vi oasis opam/opam.1 > opam/opam
rm opam/opam.1

# Remove development files.
rm -f \
    .jenkins.sh appveyor.yml .travis.yml *.exe configure _oasis .merlin \
    src/util/appveyor-build.sh src/util/appveyor-install.sh src/util/dist.sh \
    src/util/fetch-dependees.py src/util/travis.sh

# Generate extra package build systems, and remove development files.
for PACKAGE in react ssl glib
do
    pushd src/$PACKAGE
    oasis setup -setup-update none
    rm -f _oasis configure
    popd
done

# Print instructions for finishing release packaging manually.
set +x

echo "1. Move out packages lwt_react, lwt_ssl, lwt_glib with"
echo "     mv src/lwt_react SOME/TMP/DIR/lwt_react-1.0.1"
echo "   and corresponding commands for lwt_ssl, lwt_glib. If not releasing"
echo "   extra packages:"
echo "     rm -r src/lwt_react"
echo
echo "2. Pin packages in their final form, and test installation and linking:"
echo "     opam pin add -n .                   # for the main lwt"
echo "     opam pin add -n SOME/TMP/DIR/lwt_react-1.0.1"
echo "                                         # for each of react, ssl, glib"
echo "     opam install lwt"
echo "     opam install lwt_react"
echo "   and so on; then compile some test programs."
echo
echo "3. Commit to this repo and tag in it to create the main lwt release."
echo "   GitHub will create package archives automatically once the tag is"
echo "   pushed:"
echo "     git add --all --force"
echo "     git rm .gitignore"
echo "     git commit -m \"Release $VERSION\""
echo "     git tag -a $VERSION"
echo "   When tagging, include the changelog. For an example, run:"
echo "     git show 2.7.0"
echo
echo "4. Create .tar.gz archives manually for the extra packages:"
echo "     cd SOME/TMP/DIR"
echo "     tar cvzf lwt_react-1.0.1.tar.gz lwt_react-1.0.1"
echo
echo "5. Post everything to GitHub:"
echo "     git push origin $VERSION"
echo "   Attach lwt_react.tar.gz, etc., to the release manually."
echo
echo "X. To revert:"
echo "     git checkout master   # or whatever branch you were on"
echo "     git checkout -- ."
echo "     git clean -ffd"
echo "     git branch -D release-$VERSION"
