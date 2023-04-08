## mogan.app website generator by vitepress

### How to build
assume cwd is MOGAN_ROOT

- before build
```shell
cp README.md docs/guide/what-is-mogan.md
cp README_ZH.md docs/zh/guide/what-is-mogan.md
```
- build webiste
```shell
yarn install
yarn docs:build
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
