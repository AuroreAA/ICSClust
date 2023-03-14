# install.packages("robustbase", repos = c("http://R-Forge.R-project.org",
#                                          "http://cran.at.r-project.org"), dep = TRUE)
# 
# 
# library(rrcov)
# data(hbk)
# hbk.x <- data.matrix(hbk[, 1:3])
# 
# # Reweighted MCD
# RMCD <- CovMcd(hbk.x, nsamp = "deterministic")
# summary(RMCD)
# 
# # MCD
# MCD <- CovMcd(hbk.x, raw.only = TRUE, nsamp = "deterministic")
# #Error in dim(mcd$coeff) <- c(5, p) : attempt to set an attribute on NULL
# 
# MCD <- CovMcd(hbk.x, raw.only = TRUE, nsamp = 500)
# MCD
# 
# 
# 
