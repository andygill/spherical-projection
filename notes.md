# Notes

[Katex](https://docs.gitlab.com/ee/user/markdown.html#math) for the math

## 12/16/2020
Today I finished my comments in files and README. I have added almost all simple reductions possible for the optimizer into Props.hs. I have noticed the additive distributive property fails every time, but works with distribution across the additive inverse (subtraction) operation. Same thing occurs when I try to change the power when multipying 2 bases (ex: 2 * 2^3). It works correctly when you divide a single thing and itself to a power, but not when multiplying (ex: 2 / 2^3 works). I found a bunch of divide by zeros and 0^0 errors, as well as changing the eval in Expr to account for negative exponents (just turn 2^-2 to 1/2^1). The sin(asin) and cos(acos) composition fails in both directions every time. I think it is because of rounding and boundary errors, so i may need to use the nearZero function on the difference.

## 12/15/2020
### Ideas
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
