## mogan.app website generator by vitepress

### How to build
assume cwd is MOGAN_ROOT

- before build
```shell
yarn install
```
- build webiste
```shell
bin/build_website
```

- preview website
```
bin/preview_website
```

- after build
```shell
cp CNAME website
rm website/README.html
```
add wasm version (optional)

```shell
cd website
git clone https://github.com/Yufeng-shen/Yufeng-Shen.github.io.git wasm
rm -rf wasm/.git
```
