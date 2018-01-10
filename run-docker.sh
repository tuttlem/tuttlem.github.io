#!/bin/bash

docker run --rm --interactive -v $(pwd):/srv/jekyll -p 4000:4000 -e MENTOS_TIMEOUT=500000 jekyll/jekyll:pages jekyll serve --host 0.0.0.0 --watch --incremental
