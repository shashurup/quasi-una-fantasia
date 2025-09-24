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
