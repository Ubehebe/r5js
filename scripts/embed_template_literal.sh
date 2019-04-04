#!/usr/bin/env bash

# Writes stdin to stdout, embedding it inside a JavaScript template literal.
# Used by the embed_template_literal Starlark rule.

VARIABLE_NAME=$1

echo "export const $VARIABLE_NAME = \`"
# backslash-escape backslash
sed -e 's/\\/\\\\/g' |
# backslash-escape backticks
sed -e 's/`/\\`/g'
echo '`;'