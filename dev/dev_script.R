# Package development
devtools::load_all()

usethis::use_package("ICS")
usethis::use_package("moments")
usethis::use_package("ICtest")
usethis::use_package("heplots")
usethis::use_package("tclust")
usethis::use_package("cluster")
usethis::use_package("mclust")

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
testthat::test_file("tests/testthat/test-rcpp_fun_test.R") # to check the test

usethis::use_test("ICSClust") # to create the test
testthat::test_file("tests/testthat/test-ICSClust.R")

# GitHub Actions ------
usethis::use_github_action_check_standard()