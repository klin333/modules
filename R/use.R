

.pkgenv <- new.env(parent = emptyenv())


#' Use Module File - Cached
#' @description
#' A cached version of `use`. Only supports module file R scripts.
#' Useful if module scripts take too long to parse, especially when there are nested modules.
#'
#' Returned cached modules are copy on modify, so altering elements of a module will not affect other copies else where.
#' Module function enclosing environments are also recursively cloned to prevent global effects across cached modules.
#'
#' Cache invalidation is based on modified time of `module_file`.
#' However, the modified time of nested modules are not checked.
#' In such case, use `invalidate_cache` for manual cache invalidation.
#' @param module_file path to module R script file
#' @param ... parameters passed to `use`
#' @export
#' @rdname use_cached
use_cached <- function(module_file, ...) {

  #  _____________                    ____________
  # | module list |                  | encl env 1 |
  # |             |                  |            |
  # |   func_a ---|--> function ---->|            |
  # |             |          ^       |            |
  # |             |          |-------|-- func_a   |
  # |             |                  |            |
  # |   func_b ---|--> function ---->|            |
  # |             |          ^       |            |
  # |             |          |-------|-- func_b   |
  # |             |                  |____________|
  # |             |                         ^
  # |             |                   ______|_____
  # |             |                  | encl env 2 |
  # |   func_c ---|--> function ---->|            |
  # |             |          ^       |            |
  # |             |          |-------|-- func_c   |
  # |_____________|                  |____________|
  #
  # 1) modify functions in module list,
  #    set their enclosing environment to a clone of original enclosing environment
  # 2) change the parent of cloned environment to the cloned parent environment
  # 3) change the enclosing environment of functions within cloned environments, to be the cloned environments
  #  _____________                    _____________
  # | module list |                  | clone env 1 |
  # |             |                  |  (step 1)   |   (step 3)
  # |   func_a ---|--> function ---->|             |<-------------+
  # |             |                  |             |              |
  # |             |                  |   func_a ---|--> function--+
  # |             |                  |             |
  # |   func_b ---|--> function ---->|             |<-------------+
  # |             |                  |             |              |
  # |             |                  |   func_b ---|--> function--+
  # |             |                  |_____________|
  # |             |                         ^ (step 2)
  # |             |                   ______|______
  # |             |                  | clone env 2 |
  # |   func_c ---|--> function ---->|  (step 1)   |<-------------+
  # |             |                  |             |              |
  # |             |                  |   func_c ---|--> function--+
  # |_____________|                  |_____________|


  if (!fs::is_absolute_path(module_file)) {
    module_file <- file.path(getwd(), module_file) # convert to absolute path for cache key
  }
  if (!(is.character(module_file) && length(module_file) == 1 && file.exists(module_file))) {
    stop("invalid module file path")
  }
  mtime <- file.info(module_file)$mtime
  cache_entry <- .pkgenv$cache[[module_file]]
  if (!is.null(cache_entry) && cache_entry$mtime == mtime) {
    module <- cache_entry$module
    # module elements are copy on modify,
    # but module environments need to be cloned to prevent cross talk.
    # functions within one module could have different enclosing environments.
    clone_envs <- list()
    for (i in names(module)) {
      if (rlang::is_closure(module[[i]])) {
        enclosing_env <- environment(module[[i]])
        env_id <- get_env_id(enclosing_env)
        if (!(env_id %in% names(clone_envs))) {
          clone_envs[[env_id]] <- clone_env_recursive(enclosing_env)
        }
        environment(module[[i]]) <- clone_envs[[env_id]]
      }
    }
    # change the parent of cloned environment to the cloned parent environment
    for (i in names(module)) {
      if (is.function(module[[i]])) {
        environment(module[[i]]) <- replace_parent_env(environment(module[[i]]), clone_envs)
      }
    }

    # change the enclosing environment of functions within cloned environments, to be the cloned environments
    for (clone_env in clone_envs) {
      for (i in ls(envir = clone_env)) {
        x <- get(i, envir = clone_env)
        if (rlang::is_closure(x)) {
          enclosing_env <- environment(x)
          env_id <- get_env_id(enclosing_env)
          if (!in_search_envs(enclosing_env)) {
            if (env_id %in% names(clone_envs)) {
              environment(x) <- clone_envs[[env_id]]
              clone_env[[i]] <- x
            } else {
              # too hard to handle properly
              warning(sprintf("environment of %s is not fully right...", i))
            }
          }
        }
      }
    }

  } else {
    module <- use(module_file, ...)
    cache <- .pkgenv$cache # must re-access cache after use()
    cache_entry <- list(
      module = module,
      mtime = mtime
    )
    cache[[module_file]] <- cache_entry
    assign('cache', cache, envir = .pkgenv)
  }
  return(module)
}


in_search_envs <- function(env) {
  any(unlist(lapply(rlang::search_envs(), identical, env))) || rlang::is_namespace(env)
}

clone_env_recursive <- function(env, depth = 1) {

  MAX_RECURSE_DEPTH <- 100 # guard against weird infinite recursion

  if (depth > MAX_RECURSE_DEPTH) {
    warning("modules got into deep recursion, early stopping recursion")
    clone_env <- env
  } else if (in_search_envs(env)) {
    # if env is on search path, eg a package namespace, global env or base env etc, don't clone
    clone_env <- env
  } else {
    clone_env <- rlang::env_clone(env)
    for (v in ls(envir = clone_env, all.names = TRUE)) {
      x <- get(v, envir = clone_env)
      if (is.environment(x)) {
        clone_env[[v]] <- clone_env_recursive(x, depth = depth + 1)
      }
    }
  }

  clone_env
}


# change the parent of cloned environment to the cloned parent environment
replace_parent_env <- function(env, clone_envs) {
  if (get_env_id(parent.env(env)) %in% names(clone_envs)) {
    parent_clone <- clone_envs[[get_env_id(parent.env(env))]]
    parent.env(env) <- replace_parent_env(parent_clone, clone_envs)
  }
  env
}


get_env_id <- function(env) {
  # when can safely assume rlang1.0+, can deprecate this in favour of rlang::obj_address
  stopifnot(is.environment(env))
  sub('<environment: (.*)>', '\\1', capture.output(env)[1])
}


#' @rdname use_cached
#' @export
invalidate_cache <- function() {
  assign('cache', NULL, envir = .pkgenv)
}


#' Use a module as dependency
#'
#' Use and/or register a module as dependency. The behaviour of use is similar
#' to \link{import} but instead of importing from packages, we import from a
#' module. A module can be defined in a file, or be an object.
#'
#' @param module (character, module) a file or folder name, or an object that
#'   can be interpreted as a module: any list-like object would do.
#' @param ... (character, or unquoted expression) names to use from module.
#' @param where (environment) typically the calling environment. Should only be
#'   relevant for testing.
#' @param attach (logical) whether to attach the module to the search path.
#' @param reInit (logical) we can use a module as is, or reinitialize it. The
#'   default is to reinitialize. This is only relevant should the module be
#'   state-full.
#'
#' @details
#' \link{import} and \code{use} can replace \link{library} and \link{attach}.
#'   However they behave differently and are only designed to be used within
#'   modules. Both will work when called in the \code{.GlobalEnv} but here they
#'   should only be used for development and debugging of modules.
#'
#' \code{use} adds a layer to a local search path if \code{attach} is
#'   \code{TRUE}. More precisely to the calling environment, which is the
#'   environment supplied by \code{where}. Regardless of the \code{attach}
#'   argument, \code{use} will return the module invisibly.
#'
#' \code{use} supplies a special mechanism to find the argument \code{module}:
#'   generally you can supply a file name or folder name as character. You can
#'   also reference objects/names which 'live' outside the module scope. If
#'   names are not found within the scope of the module, they are searched for
#'   in the environment in which the module has been defined. This happens
#'   during initialization of the module, when the \code{use} function is
#'   called.
#'
#' Modules can live in files. \code{use} should be used to load them. A module
#'   definition in a file does not need to use the \link{module} constructor
#'   explicitly. Any R script can be used as the body of a module.
#'
#' When a folder is referenced in \code{use} it is transformed into a list of
#'   modules. This is represented as a nested list mimicking the folder
#'   structure. Each file in that folder becomes a module.
#'
#' @export
#' @examples
#' m1 <- module({
#'   foo <- function() "foo"
#' })
#' m2 <- module({
#'   use(m1, attach = TRUE)
#'   bar <- function() "bar"
#'   m1foo <- function() foo()
#' })
#' m2$m1foo()
#' m2$bar()
#'
#' \dontrun{
#' someFile <- tempfile(fileext = ".R")
#' writeLines("foo <- function() 'foo'", someFile)
#' m3 <- use(someFile)
#' m3$foo()
#' otherFile <- tempfile(fileext = ".R")
#' writeLines("bar <- function() 'bar'", otherFile)
#' m4 <- use(otherFile)
#' m4$bar()
#' m5 <- use(tempdir())
#' m5
#' }
use <- function(module, ..., attach = FALSE, reInit = TRUE, where = parent.frame()) {

  moduleName <- as.character(substitute(module))
  module <- useTryFindModule(module, moduleName, where, match.call())
  name <- if (is.character(module)) module else moduleName
  module <- as.module(module, reInit = reInit, envir = where)
  module <- useGetSelection(module, match.call(expand.dots = TRUE))

  if (attach) addDependency(
    module,
    names(module),
    where,
    makeAssignment,
    name
  )

  invisible(module)

}

useTryFindModule <- function(module, moduleName, envir, mc) {
  m <- try(module, TRUE)
  if (is.error(m)) {
    m1 <- try(
      eval(mc$module, get(useTopenvNameWithinModule(), envir = envir)), TRUE)
    if (is.error(m1)) stop(simpleError(useGetErrorMessage(m), mc))
    else m <- m1
  }
  m
}

is.error <- function(x) {
  inherits(x, "try-error")
}

useGetErrorMessage <- function(x) {
  attributes(x)$condition$message
}

useGetSelection <- function(module, mc) {
  namesToImport <- deparseEllipsis(mc, c("module", "attach", "reInit", "where"))
  if (length(namesToImport) == 0) module
  else module[namesToImport]
}

useTopenvNameWithinModule <- function() {
  ".__topenv__"
}
