# Package development
devtools::load_all()
#renv::install("C:/Users/auror/OneDrive - Erasmus University Rotterdam/Packages/ICS_1.3-1.tar.gz")
#renv::install("C:/Users/auror/Documents/EUR/OneDrive - Erasmus University Rotterdam/Packages/ICS_1.3-1.tar.gz")

# res = ICS::ICS(iris[,1:4])
# ICS::components(res, select = "IC.4")


usethis::use_package("ICS")
usethis::use_package("moments")
usethis::use_package("ICtest")
usethis::use_package("heplots")
usethis::use_package("tclust")
usethis::use_package("cluster")
usethis::use_package("mclust")
usethis::use_package("amap")
usethis::use_package("fpc")
usethis::use_package("RcppRoll")
usethis::use_package("GGally")
usethis::use_package("ggplot2")
usethis::use_package("mvtnorm")
usethis::use_package("rrcov")
usethis::use_package("scales")
usethis::use_package("otrimle")


usethis::rename_files("index_crit.R", "select_crit.R")

## Edit DESCRIPTION
usethis::use_gpl_license(version = 3, include_future = TRUE)
usethis::use_readme_rmd()

person("Aurore", "Archimbaud", email = "aurore.archimbaud@live.fr",
       role = c("aut", "cre"), comment = c(ORCID = "0000-0002-6511-9091"))
person("Andreas", "Alfons", email = "alfons@ese.eur.nl",
       role = c("aut"))


## Summary ----
usethis::use_git()
usethis::use_github()


# Tests --------------
usethis::use_testthat(3)
usethis::use_test("rcpp_fun_test") # to create the test
testthat::test_file("tests/testthat/test-tcov.R") # to check the test

usethis::use_test("ICSClust") # to create the test
testthat::test_file("tests/testthat/test-ICSClust.R")

usethis::use_test("plots") # to create the test
testthat::test_file("tests/testthat/test-plots.R")


# GitHub Actions ------
usethis::use_github_action_check_standard()
