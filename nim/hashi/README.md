# hashi benchmark adapter

[hashi](https://github.com/kaitakeradiology/hashi) is an HTTP/1.1 and
WebSocket server for [Nimony](https://github.com/nim-lang/nimony), the Nim 3
compiler. Handlers are `.passive` procs — sequential code that suspends at I/O
points and resumes on `std/ioring`'s worker pool — so a parked request costs a
continuation frame rather than a thread. It has no dependencies beyond the
Nimony standard library.

This adapter uses the production path: the router with `:param` captures, the
same `serve()` entry point an application calls, with the per-request access
log switched off.

Nimony is pre-release and has no release to download, so the build stage clones
it at a pinned commit and bootstraps it with the image's Nim 2, as hashi's own
CI does. hashi itself is an ordinary dependency in `server.nimble`, resolved by
`pnak` — Nimony's package manager, which reads the same `.nimble` format as
Nimble and writes the module search path. Both pins (the compiler commit in
`config.yaml`, the hashi tag in `server.nimble`) are bumped by hand.
