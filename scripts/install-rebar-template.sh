#! /bin/bash

# This script does not handle REBAR_CACHE_DIR environment variable
# because its support is buggy in rebar3
# https://github.com/erlang/rebar3/issues/2762

cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

TEMPLATE_DIR="$HOME/.config/rebar3/templates/emqx-plugin-template"

rm -rf "$TEMPLATE_DIR"
mkdir -p "$TEMPLATE_DIR"

cp -r emqx-plugin-templates "$TEMPLATE_DIR/emqx-plugin-templates"
cp -r emqx-plugin.template "$TEMPLATE_DIR/emqx-plugin.template"

