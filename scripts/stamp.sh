#!/usr/bin/env bash

# Called by --workspace_status_command in .bazelrc.
# This writes STABLE_HEAD and STABLE to bazel-out/stable-status.txt on each build,
# allowing them to be embedded in the banner. See //ui:banner_stamped for context.

echo "STABLE_HEAD $(git rev-parse HEAD)"
echo "STABLE_DATE $(git show -s --format=%ci HEAD)"