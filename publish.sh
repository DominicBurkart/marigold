set -euxo pipefail

cd marigold-impl && \
  cargo publish && \
  cd ../marigold-grammar && \
  cargo publish && \
  cd ../marigold-macros && \
  cargo publish && \
  cd ../marigold &&
  cargo publish
