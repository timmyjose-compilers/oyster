# oyster

A small parser-combinator library in Idris, adapted from the paper, ["Monadic Parsing in Haskell"](https://www.cs.nott.ac.uk/~pszgmh/pearl.pdf) and Chapter 13 of the 
book, "Programming in Haskell" by Graham Hutton.

## Usage

Build the library using:

```
  $ make build
```

and install it using:

```
  $ make install
```

## Sample

The `samples` directory contains some sample clients using the library. For these examples, you need to invoke Idris with the package passed in (after installing the package):

```
  $ idris2 -p oyster <ClientName>
 ```

## LICENSE

See [LICENSE.md](LICENSE.md).