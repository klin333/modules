
print("in mod a")
Sys.sleep(1)

func_a <- function() {
  print("func_a")
}

module_setting <- 1

set_module_setting <- function(setting) {
  module_setting <<- setting
}

get_module_setting <- function() {
  return(module_setting)
}
