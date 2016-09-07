#!/bin/sh

if [ ! -d site ]; then
  echo "No site found to encode."
  exit 1
fi

rm -f site.tar
tar cvf site.tar site/

sudo xl destroy halvm-web
sudo xl create halvm-web.config -c
