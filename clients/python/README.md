# A Python client for Myxine

[Myxine](https://github.com/kwf/myxine) is a language-agnostic local
server that lets you build interactive applications in the browser using a
RESTful API. This package defines simple Python bindings for using Myxine to
quickly prototype surprisingly high-performance GUIs.

Myxine itself runs as a local server, separately from these bindings. It is
built in Rust, and can be installed using the standard Rust build tool cargo:

``` bash
$ cargo install myxine
```

This Python package does not manage the myxine server process; it assumes that
it is already running in the background (either started by an end-user, or
managed by your own Python application).

**Package versioning and stability:** This package should be considered in
"alpha" stability at present. No compatibility between alpha versions is
guaranteed.
