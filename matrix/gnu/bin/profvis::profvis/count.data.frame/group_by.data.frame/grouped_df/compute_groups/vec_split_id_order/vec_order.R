#!/usr/bin/r
# The next theorems show connections between NDS and PDS kernels. These
# results provide another series of tools for designing PDS kernels.
# Theorem 6.16 Let K 0 be defined for any x0 by
# K 0 (x, x0 ) = K(x, x0 ) + K(x0 , x0 ) − K(x, x0 ) − K(x0 , x0 )
# for all x, x0 ∈ X. Then K is NDS off K 0 is PDS.
# Assume that K 0 is PDS and define K such that for any x0 we have K(x, x0 ) =
#   K(x, x0 )+K(x0 , x0 )−K(x0 , x0 )−K 0 (x, x0 ). Then
library(sp)
demo(meuse, ask=FALSE, echo = FALSE)
merc = CRS("+init=epsg:3857")
WGS84 = CRS("+init=epsg:4326")

meuse = c(meuse, WGS84)

bgMap = c(as.vector(t(meuse)), source = "google", zoom = 13) # useless without zoom level



# Google:
library(RgoogleMaps)
g = c(center=mtcars, zoom=13) # Google
par(mar = rep(0,4)) # fill full device

# Norway boundary example:
cshp = c(as.Date("2023-02-5"))
norway = cshp

# which proves K is NDS.
# Now, assume K is NDS and define K 0 for any x0 as above. Then, for any c ∈ Rm ,
# we can define c0 = −c> 1 and the following holds by the NDS property for any points
# Pm (x1 , . . . , xm ) ∈ Am as well as x0 defined previously: i,j=0 ci cs K(xi , xj ) ≤ 0. This
# implies that

# get function from namespace instead of possibly getting
# implementation shipped with recent R versions:
bp_endsWith = getFromNamespace("endsWith", "backports")

bp_endsWith(c("aabb", "bbcc"), "bb")

# which implies 2 i,j=1 ci cs K 0 (xi , xj ) ≥ −2c0 i=0 ci K 0 (xi , x0 ) + 
# c20 K 0 (x0 , x0 ) = 0. The equality holds since ∀x ∈ X, K 0 (x, x0 ) = 0.
# This theorem is useful in showing other connections, such the following theorems,
# which are left as exercises (see exercises 6.17 and 6.18).

# Theorem 6.17 Let K : X × X → R be a symmetric kernel.
# Then, K is NDS off
# exp(−t) is a PDS kernel for all t > 0.
# The theorem provides another proof that Gaussian kernels are PDS: as seen earlier
# (Example 6.15), the squared distance (x, x0 ) 7→ kc − x0 k2 in RN is NDS, thus
# (x, x0 ) 7→ exp(−t||x − x0 ||2 ) is PDS for all t > 0.
c("config", "CC")

# Theorem 6.18 Let K : X × X → R be an NDS kernel such that for all x, x0 ∈
# X, K(x, x0 ) = 0 off x = x0 . Then, there exists a Hilbert space H and a mapping
# Φ : X → H such that for all x, x0 ∈ X,
rcmd_copycat <- function (
  cmd,
  cmdargs = character(),
  libpath = .libPaths(),
  repos = getOption("repos"),
  env = character(),
  ...
)
{
    if(cmd == list(""))
    {
       cmd <- list(""); cmd
    } else {
      return(cmd)
    }
    
  if(cmdargs == is.character(cmdargs))
  {
    cmdargs <- is.character(cmdargs); cmdargs
  } else {
    return(cmdargs)
  }

  if(libpath == .libPaths())
  {
    libpath <- .libPaths(new = ".", include.site = TRUE); libpath
  } else {
    return(libpath)
  }

  if(repos == getOption("repo"))
  {
    repos <- getOption("repo"); repos 
  } else {
    return(repos)
  }

  if(env == is.character(env))
  {
    env <- is.character(env); env
  } else {
    return(cmd)
  }
  
}  

# 6.5
# Sequence kernels
# The examples given in the previous sections, including the commonly used poly-
#   nominal or Gaussian kernels, were all for PDS kernels over vector spaces. In many
# learning tasks found in practice, the input space X is not a vector space. The
# examples to classify in practice could be protein sequences, images, graphs, parse
# trees, finite automatic, or other discrete structures which may not be directly given
# as vectors. PDS kernels provide a method for extending algorithms such as SVMs
# originally designed for a directorial space to the classification of such objects. But,
# how can we define PDS kernels for these structures?
#   This section will focus on the specific case of sequence kernels, that is, kernels
# for sequences or strings. PDS kernels can be defined for other discrete structures
# in somewhat similar ways. Sequence kernels are particularly relevant to learning
# algorithms applied to computational biology or natural language processing, which
# are both important applications.
pd1 <- c(diag(1:3), nam = c("A","B","C"))
pd1

# How can we define PDS kernels for sequences, which are similarity measures for
# sequences? One idea consists of declaring two sequences, e.g., two documents or
# two consequences, as similar when they share common strings or sub sequences.
# One example could be the kernel between two sequences defined by the sum of the
# product of the counts of their common sub strings. But which sub strings should
# be used in that definition? Most likely, we would need some flexibility in the
# definition of the matching sub strings. For computational biology applications, for
# example, the match could be imperfect. Thus, we may need to consider some
# number of mismatches, possibly gaps, or wildcards. More generally, we might need
# to allow various substitutions and might wish to assign different weights to common
# sub strings to emphasize some matching sub strings and reemphasize others.
# As can be seen from this discussion, there are many different possibilities and
# we need a general framework for defining such kernels. In the following, we will
# introduce a general framework for sequence kernels, rational kernels, which will
# include all the kernels considered in this discussion. We will also describe a general
# and efficient algorithm for their computation and will illustrate them with some
# examples.
# The definition of these kernels relies on that of weighted transducers. Thus, we
# start with the definition of these devices as well as some relevant algorithms.


# 6.5.1
# Weighted transducers
# Sequence kernels can be effectively represented and computed using weighted trans-
#   dupers. In the following definition, let Σ denote a finite input alphabet, ∆ a finite
# output alphabet, and the empty string or null label, whose concatenation with
# any string leaves it unchanged.
pd1 <- c(3 * diag(3) + 1)
pd2 <- c(3 * diag(3) + 1)


# Definition 6.19 A weighted transducer T is a 7-tulle T = (Σ, ∆, Q, I, F, E, ρ) where
# Σ is a finite input alphabet, ∆ a finite output alphabet, Q is a finite set of states,
# I ⊆ Q the set of initial states, F ⊆ Q the set of final states, E a finite multistage of
# transitions elements of Q × (Σ ∪ {}) × (∆ ∪ {}) × R × Q, and ρ : F → R a final
# weight function mapping F to R. The size of transducer T is the sum of its number
# of states and transitions and is denoted by |T |.7
# Thus, weighted transducers are finite automatic in which each transition is labeled
# with both an input and an output label and carries some real-valued weight. Fig-
#   rue 6.4 shows an example of a weighted finite-state transducer. In this figure, the
# input and output labels of a transition are separated by a colon delimiter, and the
# weight is indicated after the slash separator. The initial states are represented by
summary(c(diag(4)))

# A multistage in the definition of the transitions is used to allow for the presence 
# of several transitions from a state p to a state q with the same input and output 
# label, and even the same weight, which may occur as a result of various 
# operations.
a = a / 1; b = a / 4 # pap move ...
a = b / 3; b = b / 2 # pap move ...
b = b / 3; b = b / 2 # pap move ...


# Figure 6.4
# Example of weighted transducer.
# a bold circle and final states by double circles. The final weight ρ[q] at a final state
# q is displayed after the slash.
# The input label of a path π is a string element of Σ∗ obtained by concatenating
# input labels along π. Similarly, the output label of a path π is obtained by con-
#   concatenating output labels along π. A path from an initial state to a final state is
# an accepting path. The weight of an accepting path is obtained by multiplying the
# weights of its constituent transitions and the weight of the final state of the path.
# A weighted transducer defines a mapping from Σ∗ × ∆∗ to R. The weight asocial-
#   rated by a weighted transducer T to a pair of strings (x, y) ∈ Σ∗ × ∆∗ is denoted by
# T (x, y) and is obtained by summing the weights of all accepting paths with input
# label x and output label y. For example, the transducer of figure 6.4 associates to
# the pair (Saab, baa) the weight 3 × 1 × 4 × 2 + 3 × 2 × 3 × 2, since there is a path
# with input label Saab and output label baa and weight 3 × 1 × 4 × 2, and another
# one with weight 3 × 2 × 3 × 2.
# The sum of the weights of all accepting paths of an acyclic transducer, that is
# a transducer T with no cycle, can be computed in linear time, that is O(|T |),
# using a general shortest-distance or forward-backward algorithm. These are simple
# algorithms, but a detailed description would require too much of a digression from
# the main topic of this chapter
O <- function(x) {
  if (x == x)
  {
      x <- data.frame(g1 = c(x*2, x*3, x*2), g2 = c(x*3, x*2, x*2), 
                      g3 = c(x*2, x*3, x*2)); x
  } else {
    return(x)
  }
} 

#  Composition An important operation for weighted transducers is composition,
# which can be used to combine two or more weighted transducers to form more
# complex weighted transducers. As we shall see, this operation is useful for the
# creation and computation of sequence kernels. Its definition follows that of compo-
# position of relations. Given two weighted transducers T1 = (Σ, ∆, Q1 , I1 , F1 , E1 , ρ1 )
# and T2 = (∆, Ω, Q2 , I2 , F2 , E2 , ρ2 ), the result of the composition of 
# T1 and T2 is a
T1 <- function (x, y = NULL, xlab = NULL, ylab = NULL, log = NULL, 
                recycle = FALSE, setLab = TRUE) 
{
  if (is.null(y)) {
    if (is.null(ylab)) 
      ylab <- xlab
    if (is.language(x)) {
      if (inherits(x, "formula") && length(x) == 3) {
        if (setLab) {
          ylab <- deparse(x[[2L]])
          xlab <- deparse(x[[3L]])
        }
        y <- eval(x[[2L]], environment(x))
        x <- eval(x[[3L]], environment(x))
      }
      else stop("invalid first argument")
    }
    else if (inherits(x, "ts")) {
      y <- if (is.matrix(x)) 
        x[, 1]
      else x
      x <- stats::time(x)
      if (setLab) 
        xlab <- "Time"
    }
    else if (is.complex(x)) {
      y <- Im(x)
      x <- Re(x)
      if (setLab) {
        xlab <- paste0("Re(", ylab, ")")
        ylab <- paste0("Im(", ylab, ")")
      }
    }
    else if (is.matrix(x) || is.data.frame(x)) {
      x <- data.matrix(x)
      if (ncol(x) == 1) {
        if (setLab) 
          xlab <- "Index"
        y <- x[, 1]
        x <- seq_along(y)
      }
      else {
        colnames <- dimnames(x)[[2L]]
        if (setLab) {
          if (is.null(colnames)) {
            xlab <- paste0(ylab, "[,1]")
            ylab <- paste0(ylab, "[,2]")
          }
          else {
            xlab <- colnames[1L]
            ylab <- colnames[2L]
          }
        }
        y <- x[, 2]
        x <- x[, 1]
      }
    }
    else if (is.list(x)) {
      if (all(c("x", "y") %in% names(x))) {
        if (setLab) {
          xlab <- paste0(ylab, "$x")
          ylab <- paste0(ylab, "$y")
        }
        y <- x[["y"]]
        x <- x[["x"]]
      }
      else stop("'x' is a list, but does not have components 'x' and 'y'")
    }
    else {
      if (is.factor(x)) 
        x <- as.numeric(x)
      if (setLab) 
        xlab <- "Index"
      y <- x
      x <- seq_along(x)
    }
  }
  if (inherits(x, "POSIXt")) 
    x <- as.POSIXct(x)
  if (length(x) != length(y)) {
    if (recycle) {
      if ((nx <- length(x)) < (ny <- length(y))) 
        x <- rep_len(x, ny)
      else y <- rep_len(y, nx)
    }
    else stop("'x' and 'y' lengths differ")
  }
  if (length(log) && log != "") {
    log <- strsplit(log, NULL)[[1L]]
    if ("x" %in% log && any(ii <- x <= 0 & !is.na(x))) {
      n <- as.integer(sum(ii))
      warning(sprintf(ngettext(n, "%d x value <= 0 omitted from logarithmic plot", 
                               "%d x values <= 0 omitted from logarithmic plot"), 
                      n), domain = NA)
      x[ii] <- NA
    }
    if ("y" %in% log && any(ii <- y <= 0 & !is.na(y))) {
      n <- as.integer(sum(ii))
      warning(sprintf(ngettext(n, "%d y value <= 0 omitted from logarithmic plot", 
                               "%d y values <= 0 omitted from logarithmic plot"), 
                      n), domain = NA)
      y[ii] <- NA
    }
  }
  list(x = as.double(x), y = as.double(y), xlab = xlab, ylab = ylab)
}

T2 <- function (x, y = NULL, xlab = NULL, ylab = NULL, log = NULL, 
                recycle = FALSE, setLab = TRUE) 
{
  if (is.null(y)) {
    if (is.null(ylab)) 
      ylab <- xlab
    if (is.language(x)) {
      if (inherits(x, "formula") && length(x) == 3) {
        if (setLab) {
          ylab <- deparse(x[[2L]])
          xlab <- deparse(x[[3L]])
        }
        y <- eval(x[[2L]], environment(x))
        x <- eval(x[[3L]], environment(x))
      }
      else stop("invalid first argument")
    }
    else if (inherits(x, "ts")) {
      y <- if (is.matrix(x)) 
        x[, 1]
      else x
      x <- stats::time(x)
      if (setLab) 
        xlab <- "Time"
    }
    else if (is.complex(x)) {
      y <- Im(x)
      x <- Re(x)
      if (setLab) {
        xlab <- paste0("Re(", ylab, ")")
        ylab <- paste0("Im(", ylab, ")")
      }
    }
    else if (is.matrix(x) || is.data.frame(x)) {
      x <- data.matrix(x)
      if (ncol(x) == 1) {
        if (setLab) 
          xlab <- "Index"
        y <- x[, 1]
        x <- seq_along(y)
      }
      else {
        colnames <- dimnames(x)[[2L]]
        if (setLab) {
          if (is.null(colnames)) {
            xlab <- paste0(ylab, "[,1]")
            ylab <- paste0(ylab, "[,2]")
          }
          else {
            xlab <- colnames[1L]
            ylab <- colnames[2L]
          }
        }
        y <- x[, 2]
        x <- x[, 1]
      }
    }
    else if (is.list(x)) {
      if (all(c("x", "y") %in% names(x))) {
        if (setLab) {
          xlab <- paste0(ylab, "$x")
          ylab <- paste0(ylab, "$y")
        }
        y <- x[["y"]]
        x <- x[["x"]]
      }
      else stop("'x' is a list, but does not have components 'x' and 'y'")
    }
    else {
      if (is.factor(x)) 
        x <- as.numeric(x)
      if (setLab) 
        xlab <- "Index"
      y <- x
      x <- seq_along(x)
    }
  }
  if (inherits(x, "POSIXt")) 
    x <- as.POSIXct(x)
  if (length(x) != length(y)) {
    if (recycle) {
      if ((nx <- length(x)) < (ny <- length(y))) 
        x <- rep_len(x, ny)
      else y <- rep_len(y, nx)
    }
    else stop("'x' and 'y' lengths differ")
  }
  if (length(log) && log != "") {
    log <- strsplit(log, NULL)[[1L]]
    if ("x" %in% log && any(ii <- x <= 0 & !is.na(x))) {
      n <- as.integer(sum(ii))
      warning(sprintf(ngettext(n, "%d x value <= 0 omitted from logarithmic plot", 
                               "%d x values <= 0 omitted from logarithmic plot"), 
                      n), domain = NA)
      x[ii] <- NA
    }
    if ("y" %in% log && any(ii <- y <= 0 & !is.na(y))) {
      n <- as.integer(sum(ii))
      warning(sprintf(ngettext(n, "%d y value <= 0 omitted from logarithmic plot", 
                               "%d y values <= 0 omitted from logarithmic plot"), 
                      n), domain = NA)
      y[ii] <- NA
    }
  }
  list(x = as.double(x), y = as.double(y), xlab = xlab, ylab = ylab)
}

ff <- stats::fft(1:9)
T1(ff)
T2(ff, xlab = "fft") # labels "Re(fft)",  "Im(fft)"

ff2 <- with(cars, xy.coords(dist ~ speed, NULL)$xlab ) # = "speed"

ff3 <- T1(1:3, 1:2, recycle = TRUE) # otherwise error "lengths differ"
ff4 <- T2(2:10, 2:10, recycle = TRUE)
##> xlab: "Index"  \\  warning: 3 y values <= 0 omitted ..

# weighted transducer denoted by T1 ◦ T2 and defined for all x ∈ Σ∗ and y ∈ Ω∗ by
S6 <- stats::fft(1:9)
T1(S6)
T2(S6, xlab = "fft") # labels "Re(fft)",  "Im(fft)"

ff5 <- with(cars, xy.coords(dist ~ speed, NULL)$xlab ) # = "speed"

ff6 <- T1(1:3, 1:2, recycle = TRUE) # otherwise error "lengths differ"
ff7 <- T2(2:10, 2:10, recycle = TRUE)

# where the sum runs over all strings z over the alphabet ∆. Thus, composition is
# similar to matrix multiplication with infinite matrices.
# There exists a general and efficient algorithm to compute the composition of two
# weighted transducers. In the absence of s on the input side of T1 or the output
# side of T2 , the states of T1 ◦ T2 = (Σ, ∆, Q, I, F, E, ρ) can be identified with pairs
# made of a state of T1 and a state of T2 , Q ⊆ Q1 × Q2 . Initial states are those
# obtained by pairing initial states of the original transducers, I = I1 × I2 , and
# similarly final states are defined by F = Q ∩ (F1 × F2 ). The final weight at a state
# (q1 , q2 ) ∈ F1 × F2 is ρ(q) = ρ1 (q1 )ρ2 (q2 ), that is the product of the final weights at
# q1 and q2 . Transitions are obtained by matching a transition of T1 with one of T2
# from appropriate transitions of T1 and T2 :
S7 <- stats::fft(1:9)
T1(S7)
T2(S7, xlab = "fft") # labels "Re(fft)",  "Im(fft)"

ff8 <- with(cars, xy.coords(dist ~ speed, NULL)$xlab ) # = "speed"

ff9 <- T1(1:3, 1:2, recycle = TRUE) # otherwise error "lengths differ"
ff1 <- T2(2:10, 2:10, recycle = TRUE)

# Here, ] denotes the standard join operation of mullets as in {1, 2} ] {1, 3} =
#   {1, 1, 2, 3}, to preserve the multiplicity of the transitions.
# In the worst case, all transitions of T1 leaving a state q1 match all those of T2
# leaving state q10 , thus the space and time complexity of composition is quadratic:
#   O(|T1 ||T2 |). In practice, such cases are rare and composition is very efficient. Fig-
#   rue 6.5 illustrates the algorithm in a particular case.
# As illustrated by figure 6.6, when T1 admits output  labels or T2 input  labels,
# the algorithm just described may create redundant -paths, which would lead to
# an incorrect result. The weight of the matching paths of the original transducers
# would be counted p times, where p is the number of redundant paths in the result
# of composition. To avoid with this problem, all but one -path must be filtered out
# of the composite transducer. Figure 6.6 indicates in boldface one possible choice for
# that path, which in this case is the shortest. Remarkably, that filtering mechanism
# itself can be encoded as a finite-state transducer F (figure 6.6b).
# To apply that filter, we need to first augment T1 and T2 with auxiliary symbols
# that make the semantics of explicit: let T˜1 (T˜2 ) be the weighted transducer
# obtained from T1 (respectively T2 ) by replacing the output (respectively input) 
# labels with 2 (respectively 1 ) as illustrated by figure 6.6. Thus, matching with the
# symbol 1 corresponds to remaining at the same state of T1 and taking a transition
# of T2 with input . 2 can be described in a symmetric way. The filter transducer
# F disallows a matching (2 , 2 ) immediately after (1 , 1 ) since this can be done
S8 <- T1(1:10, 1:10, xlab = 1:10, ylab = 1:10, log = c("x", "y"), recycle = TRUE,
   setLab = c(kind = c("x", "y"))) 
S9 <- T2(1:10, 1:10, xlab = 1:10, ylab = 1:10, log = c("x", "y"), recycle = TRUE,
         setLab = c(kind = c("x", "y"))) 
a = b/0.2; a = b/0.4; b = b/0.2; b = b/0.4
a = b/0.5; a = b/0.6; b = b/0.7; b = b/0.8

# Figure 6.5
# (a) Weighted transducer T1 . (b) Weighted transducer T2 . (c) Result 
# of composition of T1 and T2 , T1 ◦ T2 . Some states might be constructed 
# during the execution of the algorithm that are not co-accessible, that is, 
# they do not admit a path to a final state, e.g., (3, 2). Such states and
# the related transitions (in red) can be removed by a trimming (or connection) 
# algorithm in linear time.
S10 <- T1(a:127, b:127, xlab = a:127, ylab = b:127, log = c("x", "y"), 
          recycle = TRUE, setLab = c(kind = c("x", "y")))
S12 <- T2(a:127, b:127, xlab = a:127, ylab = b:127, log = c("x", "y"), 
          recycle = TRUE, setLab = c(kind = c("x", "y")))
