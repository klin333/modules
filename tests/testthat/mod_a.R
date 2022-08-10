
print("in mod a")
Sys.sleep(1)

func_a <- function() {
  print("func_a")
}

sd <- stats::sd

pkgenv <- new.env(parent = emptyenv())
pkgenv$env_setting <- 1

bare_setting <- 1

set_module_setting <- function(setting) {
  pkgenv$env_setting <- setting
  bare_setting <<- setting
}

get_module_setting <- function() {
  stopifnot(pkgenv$env_setting == bare_setting)
  return(pkgenv$env_setting)
}

use_module_setting <- function() {
  get_module_setting()
}

make_func <- function(func) {
  function(x) {
    func(x)
  }
}

func_mean <- make_func(mean)
