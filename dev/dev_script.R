## Edit DESCRIPTION
usethis::use_gpl_license(version = 3, include_future = TRUE)
usethis::use_readme_rmd()

## Summary ----
usethis::use_git()
usethis::use_github()


# Tests --------------
usethis::use_testthat(3)
usethis::use_test("rcpp_fun_test") # to create the test
testthat::test_file("tests/testthat/test-rcpp_fun_test.R") # to check the test


# GitHub Actions ------
usethis::use_github_action_check_standard()
