## Universal Ocaml Json encode/decode

Universal Json for both natively Ocaml/Reason and Js apps.

This project was inspired by [melange-json](https://github.com/melange-community/melange-json) and [YoJson](https://github.com/ocaml-community/yojson)

This library can be used on both melange an native ocaml without external contract dependency.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)

## Installation

```sh
opam install universal-json
```

## Usage

ToDo

### Shared

#### For client dune config add

```dune
 (libraries universal-json.js)
```

#### For native dune config add

```dune
 (libraries universal-json.native)
```