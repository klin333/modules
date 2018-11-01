[![Build Status](https://travis-ci.org/wahani/modules.png?branch=master)](https://travis-ci.org/wahani/modules)
[![codecov.io](https://codecov.io/github/wahani/modules/coverage.svg?branch=master)](https://codecov.io/github/wahani/modules?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/modules)](https://cran.r-project.org/package=modules)
![Downloads](http://cranlogs.r-pkg.org/badges/modules)
# Modules in R

Provides modules as an organizational unit for source code. Modules
enforce to be more rigorous when defining dependencies and have
a local search path. They can be used as a sub unit within packages
or in scripts.

## Installation

From CRAN:

```r
install.packages("modules")
```

From GitHub:


```r
if (require("devtools")) install_github("wahani/modules")
```

# Introduction

The key idea of this package is to provide a unit of source code which has it's
own scope. The main and most reliable infrastructure for such organizational
units in the R ecosystem is a package. Modules can be used as stand alone,
ad-hoc substitutes for a package or as a sub-unit within a package.

When modules are defined inside of packages they act as bags of functions (like
objects as in object-oriented-programming). Outside of packages modules define
entities which only know of the base environment, i.e. within a module the base
environment is the only *package* on the *search path*. Also they are always
represented as a list inside R.

Some core features:


```r
library("modules")
m <- module({
  boringFunction <- function() "boring output"
})
m$boringFunction()
```

```
## [1] "boring output"
```

Since they are isolated from the `.GlobalEnv` the following object `x` can not
be found:


```r
x <- "hey"
m <- module({
  someFunction <- function() x
})
m$someFunction()
```

```
## Error in m$someFunction(): object 'x' not found
```


## Imports

If you rely on exported objects of a package you can refer to them explicitly
using `::`:


```r
m <- module({
  functionWithDep <- function(x) stats::median(x)
})
m$functionWithDep(1:10)
```

```
## [1] 5.5
```

Or you can use `import` for *attaching* single objects or packages and `use` for
*attaching* or loading a module:


```r
m <- module({

  import("stats", "median") # make median from package stats available

  functionWithDep <- function(x) median(x)

})
m$functionWithDep(1:10)
```

```
## [1] 5.5
```


```r
m <- module({

  import("stats")

  functionWithDep <- function(x) median(x)

})
m$functionWithDep(1:10)
```

```
## [1] 5.5
```

## Exports

It may also be of interest to control which objects are visible for the client.
You can do that with the `export` function. Note that export accepts regular
expressions which are indicated by a leading '^'.


```r
m <- module({

  export("fun")

  fun <- identity # public
  privateFunction <- identity

  # .named are always private
  .privateFunction <- identity

})

names(m)
```

```
## [1] "fun"
```

## Example: Modules as Parallel Process

One example where you may want to have more control of the enclosing environment 
of a function is when you parallelize your code. First consider the case when a 
*naive* implementation fails.


```r
library("parallel")
dependency <- identity
fun <- function(x) dependency(x) 

cl <- makeCluster(2)
clusterMap(cl, fun, 1:2)
```

```
## Error in checkForRemoteErrors(val): 2 nodes produced errors; first error: could not find function "dependency"
```

```r
stopCluster(cl)
```

To make the function `fun` self contained we can define it in a module. 


```r
m <- module({
  dependency <- identity
  fun <- function(x) dependency(x) 
})

cl <- makeCluster(2)
clusterMap(cl, m$fun, 1:2)
```

```
## [[1]]
## [1] 1
## 
## [[2]]
## [1] 2
```

```r
stopCluster(cl)
```

Note that the parallel computing facilities in `R` always provide a way to
handle such situations. Here it is just a matter of organization if you believe
the function itself should handle its dependencies or the parallel interface.


# Related Projects

There exist several projects with similar goals. First of all, the package
[klmr/modules](https://github.com/klmr/modules) aims at providing a unit similar
to what [Python](https://www.python.org/)-modules are. This project is obviously
interesting for you when you have prior knowledge in Python. In contrast to
`klmr/modules` modules defined here do not aim for a full replacement of
R-packages -- rather they provide a sub-unit within R's package
ecosystem. Otherwise there is considerable overlap of features between the two
packages.

Second you may be interested in
[import](https://cran.r-project.org/package=import) which provides convenient
syntax for stating dependencies in script files. This is something which is also
covered here, although, when you are only interested in a replacement for
`library` the package `import` is more focused. 

`modules` in this package can act as objects as in object-orientation. In 
contrast to [R6](https://cran.r-project.org/package=R6) and reference classes
implemented in the methods package here these objects are immutable by default.
Furthermore it is not being made easy to change state of a module; but it is not
difficult to do that if you really want to: see the section on coupling below.
Furthermore inheritance is not a feature, instead you have various possibilities
for object composition.

The development of the `modules` package has been inspired by other languages:
[F#](https://fsharpforfunandprofit.com/posts/organizing-functions/), 
[Erlang](http://learnyousomeerlang.com/modules) and 
[julia](http://docs.julialang.org/en/release-0.4/manual/modules/).


# Scripts as modules

You can load scripts as modules when you refer to a file (or directory) in a
call to `use`. Inside such a script you can use `import` and `use` in the same
way you typically use `library`. A major difference is, that library will not
only attach the stated package but also all packages in the depends field of
that package. This is something you have to do manually (explicitly) with
`import`. Consider the following example where we create a module in a temporary
file with its dependencies.


```r
code <- "
import('stats', 'median')
functionWithDep <- function(x) median(x)
"

fileName <- tempfile(fileext = ".R")
writeLines(code, fileName)
```

Then we can load such a module into this session by the following:


```r
m <- use(fileName)
m$functionWithDep(1:2)
```

```
## [1] 1.5
```


# Nested Modules

You can also write nested modules, which means you define modules inside
modules. In this case dependencies of the top level module are accessible to its
children:


```r
m <- module({

  import("stats", "median")
  import("modules", "module")

  anotherModule <- module({
    fun <- function(x) median(x)
  })

})

m$anotherModule$fun(1:2)
```

```
## [1] 1.5
```


# Parameterized Modules

Sometimes it can be useful to pass arguments to a module. If you have a
background in object oriented programming you may find this natural. From a
functional perspective we define parameters shared by a list of closures. This
is achieved by making the enclosing environment of the module available to the
module itself. Note that inside a package this would be the default behaviour.


```r
m <- function(param) {
  module(topEncl = environment(), {
    fun <- function() param
  })
}

m(1)$fun()
```

```
## [1] 1
```


# Documentation

If you want proper documentation for your functions or modules you really want a
package. However, there are some simple things you can do for ad-hoc
documentation of modules which is to use comments:


```r
module({
  fun <- function(x) {
    ## A function for illustrating documentation
    ## x (numeric)
    x
  }
})
```

```
## fun:
## function(x)
## ## A function for illustrating documentation
## ## x (numeric)
```



# Modules in Packages

You can use modules inside packages in the same way as illustrated above. When a
module is defined inside a R-package its search path connects to the packages
namespace. So it sees all objects within the package and has access to all its
dependencies. You can always change this by specifying the argument `topEncl`
when calling `module`. Do not use `import` inside this setting but instead rely
on the package to handle your dependencies. Attaching other modules using `use`
and `expose` can make sense but is a matter of preference. You should definitely
not rely on modules in files and load them with `use`!


# Modules with Object Orientation

## S3

S3 method dispatch can be problematic because of the special search mechanism of
`UseMethod`. What will work, however, is wrapping the generic function in a
wrapper function.


```r
m <- module({
  .generic <- function(x) UseMethod("generic")
  generic.numeric <- function(x) cat("method for x ~ numeric")
  generic <- function(x) .generic(x)
})
m$generic(1)
```

```
## method for x ~ numeric
```

## S4

By default the *set* functions of the methods package have side effects in the
top level environment. So you would have to set the appropriate environment for
the argument 'where'. However if you really have the need for S4 generics,
classes and methods you should consider writing a package instead; or if you are
already in a package define them in the scope of the package.


# Modules and Coupling

Best is to only use `::` and `use` with `attach = FALSE` to be explicit with 
your dependencies. However, there are some other options you have, which will
result in stronger forms of coupling between modules.

## Modules to Model Mutable State

This is in itself abstract but in principle you can not only put functions in
your bag (module) but any R-object. This transforms into something which is
probably associated with object-orientation. You can like this or not, here I
simply use it to illustrate the strongest form of coupling between two modules I
can come up with. 

In the following I define a module to encapsulate some value and have a *get*
and *set* method for it:


```r
mutableModule <- module({
  .num <- NULL
  get <- function() .num
  set <- function(val) .num <<- val
})
mutableModule$get()
```

```
## NULL
```

```r
mutableModule$set(2)
```

## Coupling Between Modules

In the next module we can use `mutableModule` and rebuild the interface to
`.num`.


```r
complectModule <- module({
  use(mutableModule, attach = TRUE)
  getNum <- function() get()
  set(3)
})
mutableModule$get()
```

```
## [1] 2
```

```r
complectModule$getNum()
```

```
## [1] 3
```

Depending on your expectations with respect to the above code it comes at a
surprise that we can get and set that value from an attached module; Furthermore
it is not changed in `mutableModule`. This is because `use` will trigger a
re-initialization of any module you plug in. You can override this behaviour:


```r
complectModule <- module({
  use(mutableModule, attach = TRUE, reInit = FALSE)
  getNum <- function() get()
  set(3)
})
mutableModule$get()
```

```
## [1] 3
```

```r
complectModule$getNum()
```

```
## [1] 3
```

This is not all we can do. Also we can use `expose`. This function will take
everything in a module and expose it to the environment from which it is called.


```r
complectModule <- module({
  expose(mutableModule, reInit = TRUE)
  set(4)
})
mutableModule$get()
```

```
## [1] 3
```

```r
complectModule$get()
```

```
## [1] 4
```

And of course we can do this with `reInit = FALSE` should this be desirable. In
this case both modules are essentially a copy of a reference.


```r
complectModule <- module({
  expose(mutableModule, reInit = FALSE)
  set(1)
})
mutableModule$get()
```

```
## [1] 1
```

```r
complectModule$get()
```

```
## [1] 1
```
