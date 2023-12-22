# 00 Preparation ---------------------------------------------------------------
cat("System information:\n")
for (i in seq_along(sysinfo <- Sys.info()))
  cat(" ", names(sysinfo)[i], ":", sysinfo[i], "\n")
options(warn = 2)

library("fairqFeaturesStadtstruktur")
library("methods")
sessionInfo()

# 01 Start ETL -----------------------------------------------------------------

status <- main()

q(save = "no", status = status)
