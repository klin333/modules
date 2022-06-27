
test_that("use cached", {

  modules::invalidate_cache()
  expect_output(mod_b <- modules::use_cached(test_path('mod_b.R')), '\\[1\\] "in mod a"\n\\[1\\] "in mod b"')
  expect_output(mod_a <- modules::use_cached(test_path('mod_a.R')), NA)

  mod_b$set_module_a_setting(2)
  expect_equal(mod_b$get_module_a_setting(), 2)
  expect_equal(mod_a$get_module_setting(), 2)

  mod_a$set_module_setting(3)
  expect_equal(mod_b$get_module_a_setting(), 3)
  expect_equal(mod_a$get_module_setting(), 3)


  modules::invalidate_cache()
  expect_output(mod_a <- modules::use_cached(test_path('mod_a.R')), "in mod a")
  expect_output(mod_b <- modules::use_cached(test_path('mod_b.R')), "in mod b")

  mod_b$set_module_a_setting(2)
  expect_equal(mod_b$get_module_a_setting(), 2)
  expect_equal(mod_a$get_module_setting(), 2)

  mod_a$set_module_setting(3)
  expect_equal(mod_b$get_module_a_setting(), 3)
  expect_equal(mod_a$get_module_setting(), 3)

})
