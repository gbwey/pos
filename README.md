# pos: a library for positive numbers

[![Hackage](https://img.shields.io/hackage/v/pos.svg?colorB=5d0ef0&style=flat)](https://hackage.haskell.org/package/pos)

## Includes the following features:
 * a type Pos for representing positive numbers, excluding zero
 * presets for commonly used positive values such as _1P, _2P, _3P, ...
 * arithmetic operations
 * functions for safely converting to and from Pos
 
Pos is designed to work with nonempty containers and also with configuration where zero is invalid.

Where Pos is used:
   The cybus package uses the Pos type with multidimensional arrays and indices where each dimension of the array is nonzero and the number of dimensions is nonzero.
   Pos is also used in the primus package for dealing with nonempty containers.

The Num instance is excluded to limit potential runtime errors arising from the use of fromIntegral, subtraction, or negation.
The Num1 class in the primus package offers a safer alternative to Num since it respects inherent bounds for those number types. Instances for Natural, Word, Pos, Int, etc. are defined.


