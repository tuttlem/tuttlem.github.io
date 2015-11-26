#!/bin/bash

# ----------------------------------------------------
# Template blogpost setup script
#
# Creates a new template markdown file for the cogs
# and levers blog and opens the file automatically in
# emacs.
# ---------------------------------------------------

DATE_SLUG=$(date +%Y-%m-%d)
TITLE=$(echo "$@" | tr -s '[:space:]' '\n' | tr -s '[:upper:]' '[:lower:]' | paste -sd-)
FNAME=_posts/$DATE_SLUG-$TITLE.md

cat >$FNAME <<EOL
---
layout: post
title: $@
date: $DATE_SLUG
comments: false
categories: [ "" ]
---

EOL

subl $FNAME

