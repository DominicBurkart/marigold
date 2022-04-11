set -euxo pipefail

cd marigold-impl && \
  cargo publish && \
  cd ../marigold-grammar && \
  cargo publish && \
  sleep 60 && \
  cd ../marigold-macros && \
  cargo publish && \
  sleep 60 && \
  cd ../marigold &&
  cargo publish
