#!/usr/bin/env bash

set -euo pipefail

# Default values
TAG=""
NAME=""
OUTPUT_DIR=""
WITH_AVSC=false

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --tag)
      TAG="$2"
      shift 2
      ;;
    --name)
      NAME="$2"
      shift 2
      ;;
    --output-dir)
      OUTPUT_DIR="$2"
      shift 2
      ;;
    --with-avsc)
      WITH_AVSC=true
      shift
      ;;
    *)
      echo "Unknown argument: $1"
      exit 1
      ;;
  esac
done

if [ -z "$TAG" ]; then
  echo "Error: --tag argument is required"
  exit 1
fi

if [ -z "$NAME" ]; then
  echo "Error: --name argument is required"
  exit 1
fi

if [ -z "$OUTPUT_DIR" ]; then
  echo "Error: --output-dir argument is required"
  exit 1
fi

set -x

rebar3 new emqx-plugin "$NAME" version="$TAG"

mv "$NAME/priv/config.hocon.example" "$NAME/priv/config.hocon"

if [ "$WITH_AVSC" = true ]; then
  mv "$NAME/priv/config_schema.avsc.enterprise.example" "$NAME/priv/config_schema.avsc"
  mv "$NAME/priv/config_i18n.json.example" "$NAME/priv/config_i18n.json"
fi

export BUILD_WITHOUT_QUIC=1
make -C "$NAME" rel

cp "$NAME"/_build/default/emqx_plugrel/*.tar.gz "$OUTPUT_DIR"
