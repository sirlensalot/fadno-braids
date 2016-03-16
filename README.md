# fando-braids: Braid representations in Haskell

Braids represented as Haskell types with support for generation and transformations.

# Braid Typeclass

 `Braid b a` is a typeclass over the braid rep itself and its value type. Since a goal of this library is to use braids for non-mathematical purposes (ie music composition), a Braid can be indexed over any `Integral` type, to support braids representing pitch values in a register for instance.

 # Generators

 All braids are represented using Artin generators as `Gen`, with `Polarity` defining the "power" of a generator as `O`ver or `U`nder.

 Generator indexes differ from the literature in that they are generally *0-indexed* whereas Artin generators are 1-indexed. However, again these braids can represent other ranges of numbers as branch indexes.

 # Braid instances

 `Artin` creates canonical, "one-at-a-time", generator braids.

 `MultiGen` creates "compressed", "many-at-a-time" braids.

 `DimBraid` is for creating "padded" braids, since generators cannot express the absence of a cross.

 # Braid builders

 `bandGen` creates Birman/Ko/Lee-style band generators. In addition, stylized braid builders like `buildBraid` and `terraceBraid` are offered.

 # Transformations/Moves

 In addition to operations like `merge` etc, the type `Move` represents Reidemeister-type isotopy moves. `makeTree` unfolds a potentially-infinite tree representing all possible applications of a move.

 # Graphics

 `drawBraid` and `drawStrands` allow drawings of braids, admitting extra functions for colorizing etc.
