#!/bin/sh

DARCS_REPO=`pwd`
export DARCS_REPO

NAME=`awk '$1 == "Name:" {print $2}' _oasis`
VERSION=`awk '$1 == "Version:" {print $2}' _oasis`

darcs dist --dist-name $NAME-$VERSION
