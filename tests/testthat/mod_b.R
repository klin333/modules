
mod_a_uncached <- modules::use(testthat::test_path('mod_a.R'))
f <- mod_a_uncached$func_a

mod_a <- modules::use_cached(testthat::test_path('mod_a.R'))

print("in mod b")

func_b <- function() {
  mod_a$func_a()
  print("func_b")
}

set_module_a_setting <- function(setting) {
  mod_a$set_module_setting(setting)
}

get_module_a_setting <- function() {
  mod_a$get_module_setting()
}
