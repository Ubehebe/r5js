#!/usr/bin/env bash
# (ln -s this file to .git/hooks/pre-commit.)
set -e
bazel run :buildifier_pre_commit
bazel build ... # exit code propagated by set -e
