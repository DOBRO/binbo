#!/bin/sh

# Install asciidoctor and pandoc

### On Linux:
# sudo apt install asciidoctor
# sudo apt install pandoc

### On macOS:
# brew install asciidoctor
# brew install pandoc

rm -f README.md
rm -f README.xml
rm -fr ./doc/*
asciidoctor -b docbook README.adoc
pandoc -f docbook -t gfm README.xml -o README.md
rebar3 ex_doc
rm -f README.md
rm -f README.xml
