cp README.md docs/guide/what-is-mogan.md
cp CONTRIBUTING.md docs/guide/CONTRIBUTING.md
yarn install
yarn docs:build
cp CNAME website
rm website/README.html