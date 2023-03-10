Simple Memory Profile
================

% % % % % % % % % % % %

``` r
library("R.utils")

oopts <- options(
  "withCapture/newline" = FALSE,
  mc.cores = 2L,
  digits = 2L
)
set.seed(0xBEEF) ## Just to minimize diff when rebuilt

## WORKAROUND: Avoid spurious memory allocations that occurs
## occasional when running in a fresh R session
options(profmem.threshold = 1000)

## Initiate the random number generator, because otherwise it will
## be done and captured during the first profiling below.  It is not
## fully clear to me what the R internals behind this is.  For instance,
## it is not enough to call `sample.int(n = 1)`.  But calling rnorm(n = 1)
## seems to avoid the problem.
rnorm(n=1)
```

# Simple Memory Profiling in R

## Introduction

The `profmem()` function of the
[profmem](https://cran.r-project.org/package=profmem) package provides
an easy way to profile the memory usage of an R expression. It logs all
memory allocations done in R. Profiling memory allocations is helpful
when we, for instance, try to understand why a certain piece of R code
consumes more memory than expected.

The `profmem()` function builds upon existing memory profiling features
available in R. It logs *every* memory allocation done by plain R code
as well as those done by native code such as C and Fortran. For each
entry, it records the size (in bytes) and the name of the functions on
the call stack. For example,

``` r`
if (capabilities('profmem')) {
gc()
}
```

``` r
<%=withCapture({
library("profmem")
options(profmem.threshold = 2000)

p <- profmem({
  x <- integer(1000)
  Y <- matrix(rnorm(n = 10000), nrow = 100)
})
p
})%>
```

``` r
( p <- na.omit(p) )
# From this, we find that <%= p$bytes[1] %> bytes are allocated for integer 
# vector `x`, which is # because each integer value occupies 4 bytes of memory.  
# The additional 40 bytes are due to the # internal data structure used for each 
# variable R.  The size of this allocation can also be
# confirmed by the value of `object.size(x)`.
# We also see that `rnorm()`, which is called via `matrix()`, allocates 
# <%= p$bytes[2] %> + <%= #
# p$bytes[3] %> bytes, where the first one reflects the <%= length(Y) %> double 
# values each 
# occupying 8 bytes.  The second one reflects some unknown allocation done 
# internally by the 
# native code that `rnorm()` uses.
# Finally, the following entry reflects the memory allocation of 
# <%= p$bytes[4] %> bytes done by # `matrix()` itself.
# } else { 
```

**WARNING: This vignette was compiled with an R version that was built
with memory profiling disabled, cf.??`capabilities('profmem')`. Please
redo!**

\<% } \## if (capabilities(???profmem???)) %\>

## An example where memory profiling can make a difference

Assume we want to set a 100-by-100 matrix with missing values except for
element (1,1) that we assign to be zero. This can be done as:

``` r
<%=withCapture({
x <- matrix(nrow = 100, ncol = 100)
x[1, 1] <- 0
x[1:3, 1:3]
})%>
```

This looks fairly innocent, but it turns out that it is very
inefficient - both when it comes to memory and speed. The reason is that
the default value used by `matrix()` is `NA`, which is of type
*logical*. This means that initially `x` is a *logical* matrix not a
*numeric* matrix. When we the assign the (1,1) element the value `0`,
which is a *numeric*, the matrix first has to be coerced to *numeric*
internally and then the zero is assigned. Profiling the memory will
reveal this;

\<% if (capabilities(???profmem???)) { %\> \<% gc() %\>

``` r
<%=withCapture({
p <- profmem({
  x <- matrix(nrow = 100, ncol = 100)
  x[1, 1] <- 0
})
print(p, expr = FALSE)
})%>
```

The first entry is for the logical matrix with 10,000 elements (= 4 \*
10,000 bytes + small header) that we allocate. The second entry reveals
the coercion of this matrix to a numeric matrix (= 8 \* 10,000
elements + small header).

To avoid this, we make sure to create a numeric matrix upfront as:

``` r
<%=withCapture({
p <- profmem({
  x <- matrix(NA_real_, nrow = 100, ncol = 100)
  x[1, 1] <- 0
})
print(p, expr = FALSE)
})%>
```

Using the
[microbenchmark](https://cran.r-project.org/package=microbenchmark)
package, we can also quantify the extra overhead in processing time that
is introduced due to the logical-to-numeric coercion;

``` r
<%=withCapture({
library("microbenchmark")
stats <- microbenchmark(
  bad   = {
    x <- matrix(nrow = 100, ncol = 100)
    x[1, 1] <- 0
  },
  good  = {
    x <- matrix(NA_real_, nrow = 100, ncol = 100)
    x[1, 1] <- 0
  },
  times = 100,
  unit = "ms"
)
stats
})%>
```

The inefficient approach is 1.5-2 times slower than the efficient one.

The above illustrates the value of profiling your R code???s memory usage
and thanks to `profmem()` we can compare the amount of memory allocated
of two alternative implementations. Being able to write memory-efficient
R code becomes particularly important when working with large data sets,
where an inefficient implementation may even prevent us from performing
an analysis because we end up running out of memory. Moreover, each
memory allocation will eventually have to be deallocated and in R this
is done automatically by the garbage collector, which runs in the
background and recovers any blocks of memory that are allocated but no
longer in use. Garbage collection takes time and therefore slows down
the overall processing in R even further.

\<% } else { %\>

**WARNING: This vignette was compiled with an R version that was built
with memory profiling disabled, cf.??`capabilities('profmem')`. Please
redo!**

\<% } \## if (capabilities(???profmem???)) %\>

## What is logged?

The `profmem()` function uses the `utils::Rprofmem()` function for
logging memory allocation events to a temporary file. The logged events
are parsed and returned as an in-memory R object in a format that is
convenient to work with. All memory allocations that are done via the
native `allocVector3()` part of R???s native API are logged, which means
that nearly all memory allocations are logged. Any objects allocated
this way are automatically deallocated by R???s garbage collector at some
point. Garbage collection events are *not* logged by `profmem()`.
Allocations *not* logged are those done by non-R native libraries or R
packages that use native code `Calloc() / Free()` for internal objects.
Such objects are *not* handled by the R garbage collector.

### Difference between `utils::Rprofmem()` and `utils::Rprof(memory.profiling = TRUE)`

In addition to `utils::Rprofmem()`, R also provides
`utils::Rprof(memory.profiling = TRUE)`. Despite the close similarity of
their names, they use completely different approaches for profiling the
memory usage. As explained above, the former logs *all individual*
(`allocVector3()`) memory allocation whereas the latter probes the
*total* memory usage of R at regular time intervals. If memory is
allocated and deallocated between two such probing time points,
`utils::Rprof(memory.profiling = TRUE)` will not log that memory whereas
`utils::Rprofmem()` will pick it up. On the other hand, with
`utils::Rprofmem()` it is not possible to quantify the total memory
*usage* at a given time because it only logs *allocations* and does
therefore not reflect deallocations done by the garbage collector.

## Requirements

In order for `profmem()` to work, R must have been built with memory
profiling enabled. If not, `profmem()` will produce an error with an
informative message. To manually check whether an R binary was built
with this enable or not, do:

``` r
<%=withCapture({
capabilities('profmem')
})%>
```

The overhead of running an R installation with memory profiling enabled
compared to one without is neglectable / non-measurable.

Volunteers of the R Project provide and distribute pre-built binaries of
the R software for all the major operating system via
[CRAN](https://cran.r-project.org/). [It has been
confirmed](https://github.com/HenrikBengtsson/profmem/issues/2) that the
R binaries for Windows, macOS (both by CRAN and by the AT&T Research
Lab), and for Linux (\*) all have been built with memory profiling
enabled. (\*) For Linux, this has been confirmed for the Debian/Ubuntu
distribution but yet not for the other Linux distributions.

In all other cases, to enable memory profiling, which is *only* needed
if `capabilities("profmem")` returns `FALSE`, R needs to be *configured*
and *built from source* using:

``` sh
$ ./configure --enable-memory-profiling
$ make
```

For more information, please see the ???R Installation and Administration???
documentation that comes with all R installations.

------------------------------------------------------------------------

Copyright Henrik Bengtsson, 2016-2018

\<% \## Clean up options(oopts) %\>
