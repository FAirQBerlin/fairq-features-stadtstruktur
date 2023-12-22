##########################################
#
# Script to set memory limit on VMLinux4
#
##########################################

library("unix")
library("R.utils")

mem_limit <- 215e9
hsize(mem_limit)
rlimit_as(mem_limit, mem_limit)
