#!/bin/bash

open_html=${1:-"true"}


Rscript -e 'library(methods); rmarkdown::render("gadAtlResults.Rmd")'

gnome-open gadAtlResults.html

