# Notes

[Katex](https://docs.gitlab.com/ee/user/markdown.html#math) for the math

## Ideas
### 12/15/2020
Instead of having an all in one file where each function is plugged into another, do the following:
Generalized Input File with stages:
 - Input:
    - Mathematics File path: a general mathematics input file (in Haskell)
    - Optimization Settings and special inputs
    - Image information
 - Stage 1: Turn Mathematics into Expr form and output to intermediary file
 - Stage 2: read expr and optimize over it, output as another expr intermediary file
 - Stage 3: Output specified code from the stuff above
 - Stage 4: Run mathematics across image if in Haskell
