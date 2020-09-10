# Test Image Information

## How to run
`:main <transform number> <file format> <input file path> <output file path>`

Transform Numbers:
1. inverseFisheyeTransform
2. unFisheye
3. inversePanoToLittlePlanet
4. panoToLittlePlanet
5. panoToGnomic

File formats
- jpeg
- png
- tga

### pano.jpeg
I do not know where this came from, but it is panorama with nearly 36) degree view

###fish_city_4mm_f_2-8.jpeg
This is some fisheye image I got from the internet found (here)[https://images.app.goo.gl/mCvXL4icc4okP7WZ6]. It has an aperture value of 4 mm (f/2.8), so it is (4/2.8). You can enter this value into where the aperture input is in the unFisheye transform

### pano_fish.jpeg
This is the output after running inverseFisheyeTransform on pano.jpeg w/ aperture of 35/4 I believe

### unfish_city1_4mm_f_2-8.jpeg
This is the output of running unFisheye on fish_city w/ 4mm f/2.8 for aperture and each x value is multiplied by -pi instead of pi

### unfish_city2_4mm_f_2-8.jpeg
This is the output of running unFisheye on fish_city w/ 4mm f/2.8 for aperture and each x value is multiplied by -pi/2 instead of pi
