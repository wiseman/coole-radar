# coole-radar

![Screenshot](screenshot.png?raw=true "Screenshot")

## To run:

```
npm install
./node_modules/.bin/shadow-cljs compile script

# Probably works best if you can set TERM to xterm-256color.
node out/script.js https://vrs.heavymeta.org/VirtualRadar/
```

Press q or ESC or ctrl-c to exit.

`✈` is a fixed wing aircraft, `x` is a helicopter.

Zoom in and out with `+`/`-`. Reset zoom with `0`.


## Development:

```
npx shadow-cljs watch script test
```
