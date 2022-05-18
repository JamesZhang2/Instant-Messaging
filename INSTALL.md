# How to Install and Build Our System

## Installation

First, create an OPAM switch:

```text
opam switch create project ocaml-base-compiler.4.12.0
```

If prompted, run

```text
eval $(opam env)
```

Then run

```text
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc
```

Now install packages specific to this project:

```text
opam install -y opium sqlite3 cohttp cohttp-lwt-unix ANSITerminal yojson 
```

You may need to install dependencies for these packages using `sudo apt install` or `opam install`.

## Build & Run

You should start the server before running the client.

### Server

Go to the server directory using `cd src/server`. Then run `make run`. The server should be up and running.

### Client

Open a new terminal. Go to the client directory using `cd src/client`. Then run `make run`. The client interface should be up and running.

## Restart the whole IM system

Go to the server directory using `cd src/server`. Then run `make clean`. The data files in the server side should have been all deleted. 

Then, go to the client directory using `cd src/client`. Then run `make clean` again. The data files in the client side should have been all deleted. 

Be sure to run `make clean` on both server and client side. 

*Note: running `make clean` permanently erases all data and cannot be restored.*