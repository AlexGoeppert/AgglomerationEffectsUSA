#!/bin/sh
set -eu

# The host checks out LFS data. R uses the same workspace and dependency cache.
mkdir -p "$R_LIBS_USER" "$RENV_PATHS_ROOT" /tmp/agglomeration-checks
exec docker run --rm --user "$(id -u):$(id -g)" --env HOME=/tmp \
  --volume "$GITHUB_WORKSPACE:/workspace" --workdir /workspace \
  --volume "$R_LIBS_USER:$R_LIBS_USER" --volume "$RENV_PATHS_ROOT:$RENV_PATHS_ROOT" \
  --volume /tmp/agglomeration-checks:/tmp/agglomeration-checks \
  --env R_LIBS_USER --env R_LIBS_SITE --env RENV_PATHS_ROOT --env RENV_CONFIG_CONSENT \
  --env CXXFLAGS --env APP_TEST_OUTPUT --env APP_MEMORY_LIMIT_MB \
  --env SHINYAPPS_TOKEN --env SHINYAPPS_SECRET \
  --entrypoint Rscript rocker/shiny-verse:4.5.3 --vanilla .github/scripts/run-locked.R "$@"
