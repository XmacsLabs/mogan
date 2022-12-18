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

if [[ "$(arch)" == "arm64" ]];then
    MOGAN_APP=build/macosx/arm64/release/Mogan.app
else
    MOGAN_APP=build/macosx/x86_64/release/Mogan.app
fi

# Pick the max processors count from sysctl.
nproc() {
    sysctl -n hw.physicalcpu
}

build_mogan_cmake() {
    # Clean up the build directory first.
    rm -rf build
    mkdir build && cd build

    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=./Mogan.app/Contents/Resources ..
    make -j"$(nproc)" install
}

build_mogan_xmake() {
    xmake config --yes
    xmake show
    xmake build --yes --verbose --diagnosis --jobs="$(nproc)" --all
    xmake install -o ${MOGAN_APP}/Contents/Resources/ mogan_install
}

prepare_assets() (
    # Code here are not critical.
    set +e

    # Cleaning
    rm -rf ${MOGAN_APP}/Contents/Resources/lib
    rm -rf ${MOGAN_APP}/Contents/Resources/include
    rm -rf ${MOGAN_APP}/Contents/Frameworks/QtQmlModels.framework
    rm -rf ${MOGAN_APP}/Contents/Frameworks/QtQml.framework
    rm -rf ${MOGAN_APP}/Contents/Frameworks/QtQuick.framework
    find ${MOGAN_APP} | grep ".DS_Store" | xargs -I% rm -rf %
    find ${MOGAN_APP} | grep "CMakeLists.txt" | xargs -I% rm -rf %

    # Plugins
    mkdir -p ${MOGAN_APP}/Contents/Resources/share/Xmacs/plugins/eukleides/bin
    cp /Applications/Mogan.app/Contents/Resources/share/Xmacs/plugins/eukleides/bin/eukleides \
       ${MOGAN_APP}/Contents/Resources/share/Xmacs/plugins/eukleides/bin

    # GS
    cp /Applications/TeXmacs.app/Contents/Resources/share/TeXmacs/bin/gs \
       ${MOGAN_APP}/Contents/Resources/share/Xmacs/bin/

    codesign --force --deep --sign - ${MOGAN_APP}

    # We opened a subshell `()` here; therefore,
    # we don't need to `set -e` as every
    # configurations here will be thrown.
)

deploy_app() {
    hdiutil create build/Mogan.dmg -fs HFS+ -srcfolder ${MOGAN_APP}
}

# “Copy Assets” is a optional step, and should not break
# the subsequent commands. `|| true` indicates `bash` to
# not stop at this command.
prepare_assets || true

deploy_app
