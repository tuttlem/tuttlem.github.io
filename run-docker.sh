#!/bin/bash

#
# This script will use the default jekyll:jekyll docker image to
# host this blog in a re-load state

docker run --rm -v $(pwd):/srv/jekyll -p 4000:4000 -e MENTOS_TIMEOUT=500000 jekyll/jekyll 
