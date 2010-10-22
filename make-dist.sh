#!/bin/sh

DARCS_REPO=`pwd`
export DARCS_REPO

NAME=`oasis query Name | tail -n 1`
VERSION=`oasis query Version | tail -n 1`

darcs dist --dist-name $NAME-$VERSION
