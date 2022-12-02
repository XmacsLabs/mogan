#!/usr/bin/env bash

# Ref: https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html
#
#   -e      Exit immediately if a pipeline, which may consist of a single simple command,
#           a list, or a compound command returns a non-zero status.
#   -u      Treat unset variables and parameters other than the special parameters ‘@’ or ‘*’,
#           or array variables subscripted with ‘@’ or ‘*’, as an error
#           when performing parameter expansion.
#   -x      Print a trace of simple commands, for commands, case commands, select commands,
#           and arithmetic for commands and their arguments or associated word lists after
#           they are expanded and before they are executed.
#   -o pipefail  If set, the return value of a pipeline is the value of the last (rightmost) command
#                to exit with a non-zero status, or zero if all commands in the pipeline exit successfully.
set -euxo pipefail

# Pick the max processors count from sysctl.
nproc() {
    sysctl -n hw.physicalcpu
}

build_mogan_base() {
    # Clean up the build directory first.
    rm -rf build
    mkdir build && cd build

    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=./Mogan.app/Contents/Resources ..
    make -j"$(nproc)" install
}

copy_assets() (
    # Code here are not critical.
    set +e

    mv Mogan.app/Contents/Resources/bin Mogan.app/Contents/Resources/share/Xmacs/
    cp /Applications/TeXmacs.app/Contents/Resources/share/TeXmacs/bin/gs Mogan.app/Contents/Resources/share/Xmacs/bin/
    mkdir -p Mogan.app/Contents/Resources/share/Xmacs/plugins/eukleides/bin
    cp /Applications/Mogan.app/Contents/Resources/share/Xmacs/plugins/eukleides/bin/eukleides Mogan.app/Contents/Resources/share/Xmacs/plugins/eukleides/bin

    # We opened a subshell `()` here; therefore,
    # we don't need to `set -e` as every
    # configurations here will be thrown.
)

deploy_app() {
    macdeployqt Mogan.app -verbose=1 -dmg
}

build_mogan_base

# “Copy Assets” is a optional step, and should not break
# the subsequent commands. `|| true` indicates `bash` to
# not stop at this command.
copy_assets || true

deploy_app
