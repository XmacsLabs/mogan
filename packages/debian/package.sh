#!/bin/bash

if [ -L ${BASH_SOURCE-$0} ]; then
  FWDIR=$(dirname $(readlink "${BASH_SOURCE-$0}"))
else
  FWDIR=$(dirname "${BASH_SOURCE-$0}")
fi

APP_HOME="$(cd "${FWDIR}/../.."; pwd)"

VERSION="2025.1.1"

ln -s $APP_HOME/packages/debian $APP_HOME/debian
cp $APP_HOME/debian/control.in $APP_HOME/debian/control
sed -e "s/@DEVEL_VERSION@/$VERSION/" -e "s/@DEVEL_RELEASE@/1/" \
  $APP_HOME/debian/changelog.in \
  > $APP_HOME/debian/changelog

cd $APP_HOME
rm -rf $APP_HOME/TeXmacs/docs/tests
export INSTALL_DIR=$APP_HOME/debian/mogan-stem/usr
dpkg-buildpackage -us -uc -b

$APP_HOME/debian/rules clean
rm $APP_HOME/debian/changelog
rm $APP_HOME/debian/control
unlink $APP_HOME/debian
