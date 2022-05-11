#!/usr/bin/env bash

set -euo pipefail

set -x

REL_VSN="$(erl -noshell -eval '{ok, C} = file:consult("rebar.config"), {_, [{release, {_Name, Vsn}, _Apps} | _]} = lists:keyfind(relx, 1, C), io:format("~s\n", [Vsn]), halt(0).')"
GIT_TAG="$(git describe --tags --exact)"

if [ "$GIT_TAG" != '' ] && [ "$GIT_TAG" != "$REL_VSN" ]; then
    echo "release version does not match git tag"
    echo "tag=$GIT_TAG"
    echo "vsn=$REL_VSN"
    exit 1
fi

