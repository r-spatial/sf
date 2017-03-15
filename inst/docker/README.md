# Docker files to build sf

This directory has two subdirectories with Docker files, one for installing base R (released) with all system dependencies, the other one building on top of that R-devel from source (downloaded from svn), in order to be able to build `sf` with r-devel and all system requirements. The images are built on ubuntu:16.04 (xenial), and use the ubuntugis-unstable ppa for system dependencies.

## build r base

Go into the directory `base` and type

    docker build . -t sf

this will build a docker image of approx. 1.5 Gb size, called `sf`. It will also install package `sf` directly from github.

You can run a container from this image interactively with 

	docker run -ti sf /bin/bash

If you want to run it while mounting the current working directory (`pwd`) to `/pkg` inside the container, and want to remove the container after you exited, use:

	docker run -v `pwd`:/pkg --rm -ti sf /bin/bash

## build r-devel

_After_ you built docker image `sf` above, on top of that you can install r-devel; this requires a complete latex distribution and counts up to 3.6 Gb. Build it by executing the following command in the `devel` directory: 

    docker build . -t sf_devel

and run the container interactively with

	docker run -ti sf_devel /bin/bash

to start r-devel, use the `Rdevel` command. Now, you can check `sf` against R-devel.
