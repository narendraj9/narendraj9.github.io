#!/usr/bin/env bash

# variables to be set properly
BLOG_REPO='https://github.com/narendraj9/narendraj9.github.io'
CNAME='www.vicarie.in'

echo ${CNAME} > _site/CNAME
rm -rf _site/.git
cd _site && git init && git add .
cd _site && git config user.email "nobody@circleci.com"
cd _site && git config user.name CircleCI
cd _site && git commit -m "Generated on `date`"
cd _site && git remote add origin ${BLOG_REPO}
cd _site && git push -f origin master:master
