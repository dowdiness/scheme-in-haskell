# scheme-in-haskell

Scheme written in haskell based on [Write Yourself a Scheme in 48 hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)

Only implemented by chapter 8

## TODO

- [ ] IO Primitives and File I/O
- [ ] Standard Library for Scheme
- [ ] Configuration to load Standard Library

## Usage

This project is based on Stack. To compile this, use:

```bash
stack build
```

If you do

```bash
stack exec scheme-in-haskell-exe  "(* 2 (+ 2 6))"
```

You'll get

```bash
16
```

If you don't install GHC and stack yet, please use [GHCup](https://www.haskell.org/ghcup/)
