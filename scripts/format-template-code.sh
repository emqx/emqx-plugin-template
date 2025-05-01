#!/bin/bash

set -euo pipefail

if [ "$#" -gt 1 ]; then
    echo "Usage: $0 [--check]"
    exit 1
fi

check="false"
if [ "$#" -eq 1 ]; then
    if [ "$1" == "--check" ]; then
        check="true"
    else
        echo "Usage: $0 [--check]"
        exit 1
    fi
fi

tmp_dir=$(mktemp -d)
rebar="./rebar3"

function mask_template() {
    local file=$1
    sed -i 's/{{=@@ @@=}}/%% {{=@@ @@=}}/g' $file
    sed -i 's/@@\(\w\+\)@@/xxxx_\1_xxxx/g' $file
}

function unmask_template() {
    local file=$1
    sed -i 's/%% {{=@@ @@=}}/{{=@@ @@=}}/g' $file
    sed -i 's/xxxx_\(\w\+\)_xxxx/@@\1@@/g' $file
}

function format() {
    local file=$1
    if [ "$check" == "false" ]; then
        "$rebar" fmt -w --verbose $file
    else
        "$rebar" fmt -c --verbose $file
    fi
}

function handle_file() {
    local file=$1
    local tmp_file=$tmp_dir/$(basename $file)

    cp $file $tmp_file
    mask_template $tmp_file
    format $tmp_file
    unmask_template $tmp_file

    mv $tmp_file $file
}

cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

for file in $(ls emqx-plugin-templates/src/*.{erl,app.src}); do
    echo "Handling $file"
    handle_file $file
done

rm -rf $tmp_dir
