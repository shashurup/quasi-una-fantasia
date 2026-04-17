#!/bin/sh

npm install @xterm/xterm@5.5.0
cp -upv node_modules/@xterm/xterm/css/xterm.css extra/resources/public/css/
cp -upv node_modules/@xterm/xterm/lib/xterm.js extra/lib/

npm install chart.js@4.5.0
cp -upv node_modules/chart.js/dist/chart.umd.js extra/lib/
cp -upv node_modules/chart.js/dist/chart.umd.min.js extra/lib/

npm install ol@10.6.1
cp -upv node_modules/ol/dist/ol.js extra/lib/
cp -upv node_modules/ol/ol.css extra/resources/public/css/

mkdir -p raw-deps
cd raw-deps
if test ! -d highlight.js
then
    git clone -b 11.11.1 git@github.com:highlightjs/highlight.js.git
fi
cd highlight.js
npm install
npm run build-cdn asciidoc c clojure csharp css diff dockerfile \
                  go ini java javascript json lisp lua makefile markdown \
                  nginx nix pgsql python ruby scheme shell sql typescript \
                  vim xml yaml
cp -upv build/highlight.js ../../extra/lib/
cp -upv build/highlight.min.js ../../extra/lib/
cd ../../

npm install marked@18.0.1
cp -upv node_modules/marked/lib/marked.umd.js extra/lib/
