#!/bin/bash

NAME=kanren

raco pkg remove ${NAME}

pushd $(dirname $0) > /dev/null
this_dir=$(pwd)
popd > /dev/null
raco pkg install --type dir --name ${NAME} --copy ${this_dir}
