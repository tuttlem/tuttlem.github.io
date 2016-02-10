#!/bin/bash

docker run --rm --interactive -v $(pwd):/src -p 4000:4000 -e MENTOS_TIMEOUT=500000 grahamc/jekyll serve --host 0.0.0.0 --watch 
