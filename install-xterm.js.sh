#!/bin/sh

npm install xterm@5.1.0
cp -upv node_modules/xterm/css/xterm.css extra/resources/public/css/
cp -upv node_modules/xterm/lib/xterm.js extra/resources/public/js/
