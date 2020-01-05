# coffea
Java decompiler written in pure Rust with a focus on speed

## Status
The decompilation part of this project is still far from complete.
The blockers on feature-completeness are control flow reconstruction and generics -- both massive tasks.
For now, it only handles `.class` files. `.jar` files are blocked on writing a high-level, streaming `zip` format parser.

As a .class file parser, it works well, though this API is not currently exposed in a meaningful way.
