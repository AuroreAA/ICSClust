# 
# # Ex1: iris ---------------------------------------------------------------
# 
# X <- iris[,-5]
# res <- ICS(X)
# select_plot(res)
# select_plot(res, type = "lines")
# 
# component_plot(res)
# component_plot(res, select = NULL)
# component_plot(res, select = c(1,4))
# component_plot(res, clusters = iris[,5])
# component_plot(res, select = c(1,4), clusters = iris[,5])
# component_plot(X, select = c(1,4), clusters = iris[,5])
# 
# 
# a = 1:5
# component_plot(a)
# select_plot(a)
# 
# select <- med_crit(res, select_only = TRUE)
# component_plot(res, select = select)
# select_plot(select)
# 
# select <- med_crit(res, nb_select = 1, select_only = FALSE)
# component_plot(res, select = select)
# select_plot(res, type = "lines")
# select_plot(select, type = "lines",  int = 0.2)
# select_plot(select, screeplot = FALSE, type = "lines", color = "lightblue")
# 
# select <- var_crit(res, nb_select = 2, select_only = FALSE)
# select_plot(select)
# select_plot(select, screeplot = FALSE)
# 
# 
# select <- normal_crit(res, level = 0.5, select_only = FALSE)
# select_plot(select)
# 
# select <- discriminatory_crit(res, clusters = iris[,5], select_only = FALSE)
# select_plot(select)
# select_plot(select, screeplot = FALSE)
# 
# 
# # Ex2: HTP ----------------------------------------------------------------
# library(ICSOutlier)
# data(HTP)
# X <- HTP[,1:10]
# res <- ICS(X)
# select_plot(res)
# component_plot(res)
# component_plot(res, select = NULL)
# component_plot(res, select = TRUE)
# component_plot(res, select = FALSE)
