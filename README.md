## Copyright 2024 Toma-Ioan Dumitrescu


## DSL Embedded for Images

## Description

Implemented geometrical transformations, regions, classical geometrical figures, optimize
and combine transformation tools, from two representations: shallow and deep embeddings.

Shallow.hs: concrete representation of images, a region being a characteristic function
of type Point -> Bool. A transformation changes the coordinates of a set of points, but
it's defined on a point. The majority of functions are in style "point-free", implementing:
checking if a point is in a region, defining regions, combining regions, visualization
of regions in the console, simple transformation (scaling and translation), combining
transformation, applying transformations over regions.

Deep.hs: defining regions will generate an AST (Abstract Syntax Tree). Implementations
for finding the shallow representation of regions, decomposing imbricated sequences
of geometrical transformations in liniarized forms, fuse similar consecutive
transformations, and optimizing transformations of an AST.

Folds.hs: fold mechanism for RegionAST, for hiding recursive interpretation of
substructures and for applying optimization at the compiler level. The mechanism
is implemented by instantiating different classes and by defining functions
for: AST and regions print format (Show class), creating regions using
arithmetic operators (Num class), applying functions over some fields with
some types (Functor class), reducing regions and transformations AST, compozitional
operations over transformations and regions.
