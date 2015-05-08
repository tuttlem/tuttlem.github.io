#!/bin/bash

#
# This script will use the default jekyll:jekyll docker image to
# host this blog in a re-load state

docker run --rm -v $(pwd):/srv/jekyll -p 127.0.0.1:4000:4000 jekyll/jekyll