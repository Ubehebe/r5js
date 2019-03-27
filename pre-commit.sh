#!/usr/bin/env bash
# (ln -s this file to .git/hooks/pre-commit.)
set -e
bazel build ... # exit code propagated by set -e
