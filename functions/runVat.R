#devtools::install_github(repo="erlasturl/vat", ref = "erlasturl_fish")
library("vat")
library('data.table')

setwd('~/Dropbox/Paul_IA')
obj <- create_vadt(outdir = "OutM45BioV158FMV79_PF/", funfile = "GroupsIceland.csv", 
                   ncout = "Out", startyear = 1948, toutinc = 365, 
                   biolprm = "BioV165.prm", fishing = TRUE, fishfile = 'FisheriesIceland.csv', toutfinc = 365)

vadt(obj, anim = NULL)
