# SourceTransform

Utilities to deal with java source code.

What it can do:

  * Read inheritance and member information from class files and create an inheritance map.
  * Read a json transformer and some mappings to produce additional mappings for the source code.
  * Rename local variables according to the json transformer.
  * Custom inspections (These are not really functional yet)

SourceTransform is meant to be used together with [Srg2Source](https://github.com/MinecraftForge/Srg2Source).