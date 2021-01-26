# Notes

[Katex](https://docs.gitlab.com/ee/user/markdown.html#math) for the math

## 1/19/2021
### TODO
1. Make a function to "rotate" the origin of the input image for lambert. The lambert projection is centered at z = -1, so I want to be able to determine where the new origin should be
2. See if forcing a hemi-sphere to to become the whole sphere, and then applying the lambert projection gives the fisheye effect
3. Make the opposite form of the lambert projection

### Notes:
What I actually did today:
 - added a rotate utility function that rotates an image's axis after being put onto a sphere's surface
 - applied rotate to the lambert function to see what images look like when you "look" at them from different angles

## 1/18/2021
### TODO
1. make a model of a 2D image
 - Instead, I just made the lambert projection... It looked like the fisheye so why not?

## 1/15/2021
### TODO
1. Wireframe a Basic Landing Page for spherical projections
2. test how to transform vertex point on a sphere via projection (just transform each vertex and keep connections)

Helpful for working with faces and vertices, [article](https://threejsfundamentals.org/threejs/lessons/threejs-custom-geometry.html)

### Working on Jekyll
**[Minima theme](https://github.com/jekyll/minima)** - this will be a helpful reference to see what I need to add or not add. (Layouts, themes, colors, etc)

#### NEED TO ADD
1. Custom Pages for each of the Projections
    - Possibly a drop down mechanic
2. Make a template for how we want each projection to look like
3. Data and asset files for Three.js

## 1/13/2021
Had meeting with Prof. Gill. Notes:
 - Make sure to get the sharing access and links to images since we are going to publish it on a website
 - AIM: Make an animation taking a sphere to a plane back onto a sphere

#### NEED TO DO (refresher):
Return to Haskell code where I transition a sphere to a projection. Honestly we can make up out own test sphere and see how the image looks after.
R -> G -> B;    R at z = 1, G at z = 0, B at z = -1 and the color transition along the color spectrum

IDEA: Have a planar image as a background and a sphere with the same mesh. As you move your mouse along the plane form a point moves on the sphere and vice versa

## 1/12/2021

- HELPFUL SITE FOR [EQUIRECTANGULAR IMAGES AND PROJECTION](https://www.maptoglobe.com/explore)

- [Another one](https://en.wikipedia.org/wiki/Equirectangular_projection#/media/File:Equirectangular_projection_SW.jpg)

## 1/11/2021
### Installing Ruby and Jekyll for local site testing
This article takes you through the process. [Page](https://docs.github.com/en/free-pro-team@latest/github/working-with-github-pages/creating-a-github-pages-site-with-jekyll#prerequisites)

1. Install Ruby and then Bundler under the prerequisites section of the webpage.
    - Make sure to install Ruby Base and the MSYS2 thing and leave the checkbox marked to run 'ridk install' which will install the environment needed
2. run gem install bundler jekyll

page saying how to [test locally](https://docs.github.com/en/free-pro-team@latest/github/working-with-github-pages/testing-your-github-pages-site-locally-with-jekyll)

### Adding JS to Jekyll examples
Example from a blog: [EX](https://blog.emmatosch.com/2016/03/09/using-custom-javascript-in-jekyll-blogs.html)

## 12/28/2020
Helpful [tutorial](https://gamedevelopment.tutsplus.com/series/lets-build-a-3d-graphics-software-engine--gamedev-12718) on basic 3D engines
Maybe we should just use Three.js to test our projections?
http://paulbourke.net/geometry/tiling/
https://github.com/mrdoob/three.js

## 12/16/2020
Today I finished my comments in files and README. I have added almost all simple reductions possible for the optimizer into Props.hs. I have noticed the additive distributive property fails every time, but works with distribution across the additive inverse (subtraction) operation. Same thing occurs when I try to change the power when multipying 2 bases (ex: 2 * 2^3). It works correctly when you divide a single thing and itself to a power, but not when multiplying (ex: 2 / 2^3 works). I found a bunch of divide by zeros and 0^0 errors, as well as changing the eval in Expr to account for negative exponents (just turn 2^-2 to 1/2^1). The sin(asin) and cos(acos) composition fails in both directions every time. I think it is because of rounding and boundary errors, so i may need to use the nearZero function on the difference.
### Ideas
- Make test_sphere.hs a file meant for testing the projections on a multicolor wireframe sphere

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
