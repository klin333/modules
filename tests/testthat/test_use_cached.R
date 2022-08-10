
test_that("use cached", {

  # check cached use doesn't get into infinite recursion
  invalidate_cache()
  mod_b <- use_cached(test_path('mod_b.R'))
  expect_warning(mod_b <- use_cached(test_path('mod_b.R')), NA)

  # check cached versions get used, and environments are separated
  modules::invalidate_cache()
  expect_output(mod_b <- modules::use_cached(test_path('mod_b.R')), '\\[1\\] "in mod a"\n\\[1\\] "in mod b"')
  expect_output(mod_a <- modules::use_cached(test_path('mod_a.R')), NA)

  mod_b$set_module_a_setting(2)
  expect_equal(mod_b$get_module_a_setting(), 2)
  expect_equal(mod_a$get_module_setting(), 1)

  mod_a$set_module_setting(3)
  expect_equal(mod_b$get_module_a_setting(), 2)
  expect_equal(mod_a$get_module_setting(), 3)


  modules::invalidate_cache()
  expect_output(mod_a <- modules::use_cached(test_path('mod_a.R')), "in mod a")
  expect_output(mod_b <- modules::use_cached(test_path('mod_b.R')), "in mod b")

  mod_b$set_module_a_setting(2)
  expect_equal(mod_b$get_module_a_setting(), 2)
  expect_equal(mod_a$get_module_setting(), 1)

  mod_a$set_module_setting(3)
  expect_equal(mod_b$get_module_a_setting(), 2)
  expect_equal(mod_a$get_module_setting(), 3)

  # check handling of different enclosing environments
  modules::invalidate_cache()
  mod_a <- modules::use_cached(test_path('mod_a.R'))
  mod_a_cached <- modules::use_cached(test_path('mod_a.R'))

  expect_equal(mod_a$func_mean(10), 10)
  expect_equal(mod_a_cached$func_mean(10), 10)

  expect_true(get_env_id(environment(mod_a$make_func)) == get_env_id(parent.env(environment(mod_a$func_mean))))
  expect_true(get_env_id(environment(mod_a_cached$make_func)) == get_env_id(parent.env(environment(mod_a_cached$func_mean))))


  # check
  modules::invalidate_cache()
  mod_a <- modules::use_cached(test_path('mod_a.R'))
  mod_a <- modules::use_cached(test_path('mod_a.R'))

  mod_a$set_module_setting(20)
  expect_equal(mod_a$use_module_setting(), 20)


  # check original copy never gets changed
  modules::invalidate_cache()
  mod_a <- modules::use_cached(test_path('mod_a.R'))
  mod_a$set_module_setting(10)
  mod_a1 <- modules::use_cached(test_path('mod_a.R'))
  mod_a2 <- modules::use_cached(test_path('mod_a.R'))

  expect_equal(mod_a1$use_module_setting(), 1)
  expect_equal(mod_a2$use_module_setting(), 1)
  mod_a1$set_module_setting(20)
  expect_equal(mod_a$use_module_setting(), 10)
  expect_equal(mod_a1$use_module_setting(), 20)
  expect_equal(mod_a2$use_module_setting(), 1)

})
