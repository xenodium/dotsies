#!/bin/bash
set -o nounset
set -o errexit

# Based on instructions from:
# http://blog.hardcodes.de/articles/63/building-clang-format-and-friends-on-osx-mountain-lion

readonly LLVM_DIR_PATH='/tmp/llvm'

update_repo() {
  pushd $PWD
  if [[ ! -d $1 ]]; then
    git clone $2
  else
    pushd $1
    git pull
    popd
  fi
  popd
}

update_all_repos() {
  update_repo "llvm" "http://llvm.org/git/llvm.git"
  pushd "${LLVM_DIR_PATH}/llvm/tools"
  update_repo "clang" "http://llvm.org/git/clang.git"
  pushd "clang/tools"
  update_repo "clang-tools-extra" "http://llvm.org/git/clang-tools-extra.git"
  popd
  popd
}

build_clang() {
  mkdir -p clang
  mkdir -p build
  pushd clang
  ../llvm/configure --enable-libcpp --enable-cxx11 --enable-debug-symbols=no --enable-optimized --prefix="${LLVM_DIR_PATH}/build"
  make install
  popd
}

mkdir -p $LLVM_DIR_PATH
pushd ${LLVM_DIR_PATH}
update_all_repos
build_clang
popd
