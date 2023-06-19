#!/usr/bin/env bash

set -euo pipefail

set -x

REBAR_CONFIG="rebar.config"
if [[ -n "$1" ]]; then
    REBAR_CONFIG="$1/rebar.config"
fi

REL_VSN="$(erl -noshell -eval '{ok, C} = file:consult("'"$REBAR_CONFIG"'"), {_, [{release, {_Name, Vsn}, _Apps} | _]} = lists:keyfind(relx, 1, C), io:format("~s\n", [Vsn]), halt(0).')"
GIT_TAG="$(git describe --tags --exact)"

if [ "$GIT_TAG" != '' ] && [ "$GIT_TAG" != "$REL_VSN" ]; then
    echo "release version does not match git tag"
    echo "tag=$GIT_TAG"
    echo "vsn=$REL_VSN"
    exit 1
fi
