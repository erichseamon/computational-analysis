# FOR504template.R
# this is a template script for Forestry 504 Computational Analysis.
# Date: 09.04.2014
# Author: Erich Seamon
#
#
#
rm(list = ls())


data = read.table('MSH_STRUCTURE_PLOT_YEAR.csv', header = T,skip = 1)
NROW(data)
avp_alpine = 