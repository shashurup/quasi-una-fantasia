#!/bin/sh

npm install @xterm/xterm@5.5.0
cp -upv node_modules/@xterm/xterm/css/xterm.css extra/resources/public/css/
cp -upv node_modules/@xterm/xterm/lib/xterm.js extra/lib/
