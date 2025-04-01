rm -rf /tmp/goldfish; git clone git@gitee.com:XmacsLabs/goldfish.git /tmp/goldfish
rm -rf TeXmacs/plugins/goldfish/src/* 
rm -rf TeXmacs/plugins/goldfish/goldfish/liii/*
rm -rf TeXmacs/plugins/goldfish/goldfish/scheme/*
rm -rf TeXmacs/plugins/goldfish/goldfish/srfi/*
cp -r /tmp/goldfish/src/* TeXmacs/plugins/goldfish/src/
cp -r /tmp/goldfish/goldfish/* TeXmacs/plugins/goldfish/goldfish/