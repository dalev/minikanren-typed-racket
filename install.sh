#!/bin/bash

pushd $(dirname $0) > /dev/null
this_dir=$(pwd)
popd > /dev/null
raco pkg install --type dir --name kanren --copy ${this_dir}
