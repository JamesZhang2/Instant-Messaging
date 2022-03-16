# How to Install and Build Our System

## Installation

The newest versions of Opium and Piaf are not compatible, so you need to create two OPAM switches and switch between them when running the server and the client.

### Server

First, create an OPAM switch for the server:

```text
opam switch create server ocaml-base-compiler.4.12.0
```

If prompted, run

```text
eval $(opam env)
```

Then run

```text
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc cryptokit
```

Now install packages specific to the server:

```text
opam install -y opium
```

You may need to install dependencies for the Opium and Cryptokit packages using `sudo apt install` or `opam install`.

### Client

First, create an OPAM switch for the client:

```text
opam switch create client ocaml-base-compiler.4.12.0
```

If prompted, run

```text
eval $(opam env)
```

Then run

```text
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc cryptokit
```

Now install packages specific to the client:

```text
opam install -y piaf
opam install ANSITerminal
```

You may need to install dependencies for the Piaf and Cryptokit package using `sudo apt install` or `opam install`.

## Build & Run

Notes:

1. The root of the `src` directory is not meant to be built. Don't try to run `dune build` or `make` at the root of the `src` directory, because either Opium or Piaf will not be found depending on which OPAM switch you're currently on. Instead, `cd` into the respective directories and run `make` there.
2. You should start the server before running the client.

### Server

Make sure the current OPAM switch is `server`. If it isn't, run `opam switch server`. Go to the server directory using `cd src/server`. Then run `make run`. The server should be up and running.

### Client

Open a new Ubuntu terminal. Switch the OPAM switch to `client` using `opam switch client`. Go to the client directory using `cd src/client`. Then run `make run`. The client interface should be up and running.
