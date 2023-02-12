#!/usr/bin/r
# 4.3
# Structural risk minimization (SRM)
# In the previous section, we showed that the estimation error can be sometimes
# bounded or estimated. But, since the approximation error cannot be estimated, how
# should we choose H? One way to proceed is to choose a very complex family H with
# no approximation error or a very small one. H may be too rich for generalization
# bounds to hold for H, but suppose we can decompose H as a union of increasingly

# complex hypothesis sets Hγ , that is H = γ∈Γ Hγ , with the complexity of Hγ
# increasing with γ, for some set Γ. Figure 4.2 illustrates this decomposition. The
# problem then consists of selecting the parameter γ ∗ ∈ Γ and thus the hypothesis
# set Hγ ∗ with the most favorable trade-off between estimation and approximation
# errors. Since these quantities are not known, instead, as illustrated by Figure 4.3,
# a uniform upper bound on their sum, the excess error (also called excess risk), can
# be used.
## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  tidy = FALSE,
  cache = FALSE
)

## -----------------------------------------------------------------------------
library(benchr)
library(cubature)

harness <- function(which = NULL,
                    f, fv, lowerLimit, upperLimit, tol = 1e-3, times = 20, ...) {
  
  fns <- c(hc = "Non-vectorized Hcubature",
           hc.v = "Vectorized Hcubature",
           pc = "Non-vectorized Pcubature",
           pc.v = "Vectorized Pcubature",
           cc = "Non-vectorized cubature::cuhre",
           cc_v = "Vectorized cubature::cuhre")
  cc <- function() cubature::cuhre(f = f,
                                   lowerLimit = lowerLimit, upperLimit = upperLimit,
                                   relTol = tol,
                                   ...)
  cc_v <- function() cubature::cuhre(f = fv,
                                     lowerLimit = lowerLimit, upperLimit = upperLimit,
                                     relTol = tol,
                                     nVec = 1024L,
                                     ...)
  
  hc <- function() cubature::hcubature(f = f,
                                       lowerLimit = lowerLimit,
                                       upperLimit = upperLimit,
                                       tol = tol,
                                       ...)
  
  hc.v <- function() cubature::hcubature(f = fv,
                                         lowerLimit = lowerLimit,
                                         upperLimit = upperLimit,
                                         tol = tol,
                                         vectorInterface = TRUE,
                                         ...)
  
  pc <- function() cubature::pcubature(f = f,
                                       lowerLimit = lowerLimit,
                                       upperLimit = upperLimit,
                                       tol = tol,
                                       ...)
  
  pc.v <- function() cubature::pcubature(f = fv,
                                         lowerLimit = lowerLimit,
                                         upperLimit = upperLimit,
                                         tol = tol,
                                         vectorInterface = TRUE,
                                         ...)
  
  ndim = length(lowerLimit)
  
  if (is.null(which)) {
    fnIndices <- seq_along(fns)
  } else {
    fnIndices <- na.omit(match(which, names(fns)))
  }
  fnList <- lapply(names(fns)[fnIndices], function(x) call(x))
  
  argList <- c(fnList, times = times, progress = FALSE)
  result <- do.call(benchr::benchmark, args = argList)
  d <- summary(result)[seq_along(fnIndices), ]
  d$expr <- fns[fnIndices]
  d
}

## -----------------------------------------------------------------------------
func <- function(x) sin(x[1]) * cos(x[2]) * exp(x[3])
func.v <- function(x) {
  matrix(apply(x, 2, function(z) sin(z[1]) * cos(z[2]) * exp(z[3])), ncol = ncol(x))
}

d <- harness(f = func, fv = func.v,
             lowerLimit = rep(0, 3),
             upperLimit = rep(1, 3),
             tol = 1e-5,
             times = 100)

## -----------------------------------------------------------------------------
m <- 3
sigma <- diag(3)
sigma[2,1] <- sigma[1, 2] <- 3/5 ; sigma[3,1] <- sigma[1, 3] <- 1/3
sigma[3,2] <- sigma[2, 3] <- 11/15
logdet <- sum(log(eigen(sigma, symmetric = TRUE, only.values = TRUE)$values))
my_dmvnorm <- function (x, mean, sigma, logdet) {
  x <- matrix(x, ncol = length(x))
  distval <- stats::mahalanobis(x, center = mean, cov = sigma)
  exp(-(3 * log(2 * pi) + logdet + distval)/2)
}

my_dmvnorm_v <- function (x, mean, sigma, logdet) {
  distval <- stats::mahalanobis(t(x), center = mean, cov = sigma)
  exp(matrix(-(3 * log(2 * pi) + logdet + distval)/2, ncol = ncol(x)))
}

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
library(mvtnorm)
g1 <- function() mvtnorm::pmvnorm(lower = rep(-0.5, m),
                                  upper = c(1, 4, 2), mean = rep(0, m), corr = sigma,
                                  alg = Miwa(), abseps = 1e-5, releps = 1e-5)
g2 <- function() mvtnorm::pmvnorm(lower = rep(-0.5, m),
                                  upper = c(1, 4, 2), mean = rep(0, m), corr = sigma,
                                  alg = GenzBretz(), abseps = 1e-5, releps = 1e-5)
g3 <- function() mvtnorm::pmvnorm(lower = rep(-0.5, m),
                                  upper = c(1, 4, 2), mean = rep(0, m), corr = sigma,
                                  alg = TVPACK(), abseps = 1e-5, releps = 1e-5)


## -----------------------------------------------------------------------------
testFn0 <- function(x) prod(cos(x))
testFn0_v <- function(x) matrix(apply(x, 2, function(z) prod(cos(z))), ncol = ncol(x))

## -----------------------------------------------------------------------------
testFn1 <- function(x) {
  val <- sum(((1 - x) / x)^2)
  scale <- prod((2 / sqrt(pi)) / x^2)
  exp(-val) * scale
}

testFn1_v <- function(x) {
  val <- matrix(apply(x, 2, function(z) sum(((1 - z) / z)^2)), ncol(x))
  scale <- matrix(apply(x, 2, function(z) prod((2 / sqrt(pi)) / z^2)), ncol(x))
  exp(-val) * scale
}


## -----------------------------------------------------------------------------
testFn2 <- function(x) {
  radius <- 0.50124145262344534123412
  ifelse(sum(x * x) < radius * radius, 1, 0)
}

testFn2_v <- function(x) {
  radius <- 0.50124145262344534123412
  matrix(apply(x, 2, function(z) ifelse(sum(z * z) < radius * radius, 1, 0)), ncol = ncol(x))
}


## -----------------------------------------------------------------------------
testFn3 <- function(x) prod(2 * x)
testFn3_v <- function(x) matrix(apply(x, 2, function(z) prod(2 * z)), ncol = ncol(x))


## -----------------------------------------------------------------------------
testFn4 <- function(x) {
  a <- 0.1
  s <- sum((x - 0.5)^2)
  ((2 / sqrt(pi)) / (2. * a))^length(x) * exp (-s / (a * a))
}

testFn4_v <- function(x) {
  a <- 0.1
  r <- apply(x, 2, function(z) {
    s <- sum((z - 0.5)^2)
    ((2 / sqrt(pi)) / (2. * a))^length(z) * exp (-s / (a * a))
  })
  matrix(r, ncol = ncol(x))
}


## -----------------------------------------------------------------------------
testFn5 <- function(x) {
  a <- 0.1
  s1 <- sum((x - 1 / 3)^2)
  s2 <- sum((x - 2 / 3)^2)
  0.5 * ((2 / sqrt(pi)) / (2. * a))^length(x) * (exp(-s1 / (a * a)) + exp(-s2 / (a * a)))
}
testFn5_v <- function(x) {
  a <- 0.1
  r <- apply(x, 2, function(z) {
    s1 <- sum((z - 1 / 3)^2)
    s2 <- sum((z - 2 / 3)^2)
    0.5 * ((2 / sqrt(pi)) / (2. * a))^length(z) * (exp(-s1 / (a * a)) + exp(-s2 / (a * a)))
  })
  matrix(r, ncol = ncol(x))
}


## -----------------------------------------------------------------------------
testFn6 <- function(x) {
  a <- (1 + sqrt(10.0)) / 9.0
  prod( a / (a + 1) * ((a + 1) / (a + x))^2)
}

testFn6_v <- function(x) {
  a <- (1 + sqrt(10.0)) / 9.0
  r <- apply(x, 2, function(z) prod( a / (a + 1) * ((a + 1) / (a + z))^2))
  matrix(r, ncol = ncol(x))
}


## -----------------------------------------------------------------------------
testFn7 <- function(x) {
  n <- length(x)
  p <- 1/n
  (1 + p)^n * prod(x^p)
}
testFn7_v <- function(x) {
  matrix(apply(x, 2, function(z) {
    n <- length(z)
    p <- 1/n
    (1 + p)^n * prod(z^p)
  }), ncol = ncol(x))
}


## -----------------------------------------------------------------------------
I.1d <- function(x) {
  sin(4 * x) *
    x * ((x * ( x * (x * x - 4) + 1) - 1))
}
I.1d_v <- function(x) {
  matrix(apply(x, 2, function(z)
    sin(4 * z) *
      z * ((z * ( z * (z * z - 4) + 1) - 1))),
    ncol = ncol(x))
}
## -----------------------------------------------------------------------------
I.2d <- function(x) {
  x1 <- x[1]; x2 <- x[2]
  sin(4 * x1 + 1) * cos(4 * x2) * x1 * (x1 * (x1 * x1)^2 - x2 * (x2 * x2 - x1) +2)
}
I.2d_v <- function(x) {
  matrix(apply(x, 2,
               function(z) {
                 x1 <- z[1]; x2 <- z[2]
                 sin(4 * x1 + 1) * cos(4 * x2) * x1 * (x1 * (x1 * x1)^2 - x2 * (x2 * x2 - x1) +2)
               }),
         ncol = ncol(x))
}

## -----------------------------------------------------------------------------
sessionInfo()

validObject <- function (object, test = FALSE, complete = FALSE) 
{
  Class <- class(object)
  classDef <- getClassDef(Class)
  where <- .classEnv(classDef)
  anyStrings <- function(x) if (isTRUE(x)) 
    character()
  else x
  errors <- character()
  slotTypes <- classDef@slots
  slotNames <- names(slotTypes)
  attrNames <- c(".Data", ".S3Class", names(attributes(object)))
  if (anyNA(idx <- match(slotNames, attrNames))) {
    badSlots <- is.na(idx)
    errors <- c(errors, paste("slots in class definition but not in object:", 
                              paste0("\"", slotNames[badSlots], "\"", collapse = ", ")))
    slotTypes <- slotTypes[!badSlots]
    slotNames <- slotNames[!badSlots]
  }
  for (i in seq_along(slotTypes)) {
    classi <- slotTypes[[i]]
    classDefi <- getClassDef(classi, where = where)
    if (is.null(classDefi)) {
      errors <- c(errors, paste0("undefined class for slot \"", 
                                 slotNames[[i]], "\" (\"", classi, "\")"))
      next
    }
    namei <- slotNames[[i]]
    sloti <- try(switch(namei, .S3Class = S3Class(object), 
                        slot(object, namei)), silent = TRUE)
    if (inherits(sloti, "try-error")) {
      errors <- c(errors, sloti)
      next
    }
    ok <- possibleExtends(class(sloti), classi, ClassDef2 = classDefi)
    if (isFALSE(ok)) {
      errors <- c(errors, paste0("invalid object for slot \"", 
                                 slotNames[[i]], "\" in class \"", Class, "\": got class \"", 
                                 class(sloti)[[1L]], "\", should be or extend class \"", 
                                 classi, "\""))
      next
    }
    if (!complete) 
      next
    errori <- anyStrings(Recall(sloti, TRUE, TRUE))
    if (length(errori)) {
      errori <- paste0("In slot \"", slotNames[[i]], "\" of class \"", 
                       class(sloti), "\": ", errori)
      errors <- c(errors, errori)
    }
  }
  extends <- rev(classDef@contains)
  for (i in seq_along(extends)) {
    exti <- extends[[i]]
    superClass <- exti@superClass
    if (!exti@simple && !is(object, superClass)) 
      next
    superDef <- getClassDef(superClass)
    if (is.null(superDef)) {
      errors <- c(errors, paste0("superclass \"", superClass, 
                                 "\" not defined in the environment of the object's class"))
      break
    }
    validityMethod <- superDef@validity
    if (is.function(validityMethod)) {
      errors <- c(errors, anyStrings(validityMethod(as(object, 
                                                       superClass))))
      if (length(errors)) 
        break
    }
  }
  validityMethod <- classDef@validity
  if (length(errors) == 0L && is.function(validityMethod)) {
    errors <- c(errors, anyStrings(validityMethod(object)))
  }
  if (length(errors)) {
    if (test) 
      errors
    else {
      msg <- gettextf("invalid class %s object", dQuote(Class))
      if (length(errors) > 1L) 
        stop(paste(paste0(msg, ":"), paste(seq_along(errors), 
                                           errors, sep = ": "), collapse = "\n"), domain = NA)
      else stop(msg, ": ", errors, domain = NA)
    }
  }
  else TRUE
}

.class1 <- function (x) 
{
  cl <- class(x)
  if (length(cl) > 1L) 
    cl[[1L]]
  else cl
}
unit <- function (x, units, data = NULL) 
{
  if (is.unit(x)) 
    return(upgradeUnit(x))
  x <- as.numeric(x)
  units <- as.character(units)
  if (length(x) == 0 || length(units) == 0) 
    stop("'x' and 'units' must have length > 0")
  if (is.null(data)) {
    data <- list(NULL)
  }
  else if (is.language(data)) {
    data <- list(as.expression(data))
  }
  else if (is.character(data) || is.grob(data) || inherits(data, 
                                                           "gPath")) {
    data <- list(data)
  }
  .Call(C_constructUnits, x, data, units)
}
# Figure 4.4
# Illustration of structural risk minimization. The plots of three errors are shown as a function of
# the index k. Clearly, as k, or equivalently the complexity the hypothesis set H , increases, the
# training error decreases, while the penalty term increases. SRM selects the hypothesis minimizing
# a bound on the generalization error, which is a sum of the empirical error and the penalty term.
# As we shall see, the following learning bound holds for all h ∈ H: for any δ > 0,
# with probability at least 1 − δ over the draw of a sample S of size m from Dm , for
# all h ∈ H and k ≥ 1,


SRM <- function(x, units, data = NULL){
  if (is.unit(x)) 
    return(upgradeUnit(x))
  x <- as.numeric(x)
  units <- as.character(units)
  if (length(x) == 0 || length(units) == 0) 
    stop("'x' and 'units' must have length > 0")
  if (is.null(data)) {
    data <- list(NULL)
  }
  else if (is.language(data)) {
    data <- list(as.expression(data))
  }
  else if (is.character(data) || is.grob(data) || inherits(data, 
                                                           "gPath")) {
    data <- list(data)
  }
  .Call(C_constructUnits, x, data, units)
  
}
unit(1, "npc")
unit(1:3/4, "npc")
unit(1:3/4, "npc") + unit(1, "inches")
min(unit(0.5, "npc"), unit(1, "inches"))
unit.c(unit(0.5, "npc"), unit(2, "inches") + unit(1:3/4, "npc"),
       unit(1, "strwidth", "hi there"))
x <- unit(1:5, "npc")
x[2:4]
x[2:4] <- unit(1, "mm")
x
SRM(1, "npc")
SRM(1:3/4, "npc")
SRM(1:3/4, "npc") + SRM(1, "inches")
SRM(SRM(0.5, "npc"), SRM(1, "inches"))
unit.c(SRM(0.5, "npc"), SRM(2, "inches") + SRM(1:3/4, "npc"),
       SRM(1, "strwidth", "hi there"))
x <- SRM(1:5, "npc")
x[2:4]
x[2:4] <- SRM(1, "mm")
x
# Thus, SRM identifies an optimal index k ∗ and therefore hypothesis set H∗ , and re-
#   turns the ERM solution based on that hypothesis set. Figure 4.4 further illustrates
# the selection of the index k ∗ and hypothesis set H∗ by SRM by minimizing
# pan upper
# bound on the sum of the training error and the penalty term Rm (H ) + log k/m.
# The following theorem shows that the SRM solution benefits from a strong learning
# guarantee. For any h ∈ H, we will denote by H(h) the least complex hypothesis
# set among the H s that contain h.

SRM.k <- function(k, h, i){
  if (k == k){
      k =
        
        (d2 <- trunc(x = c(k, h)));
      str(d2);
      ## slightly larger in internal size:
      str(as(d2, "sparseMatrix"));
      
      M <- trunc(x = c(k, h, i));
      
      c(d2); k # trivial
      
  } else {
    return(k)
  }
  if (h == h){
    h =
      
      (d2 <- trunc(x = c(k, h)));
    str(d2);
    ## slightly larger in internal size:
    str(as(d2, "sparseMatrix"));
    
    M <- trunc(x = c(k, h, i));
    
    c(d2); h # trivial
    
  } else {
    return(h)
  }
  if (i == i){
    i =
      
      (d2 <- trunc(x = c(k, h)));
    str(d2);
    ## slightly larger in internal size:
    str(as(d2, "sparseMatrix"));
    
    M <- trunc(x = c(k, h, i));
    
    c(d2); i # trivial
    
  } else {
    return(i)
  }
  
}

matrix <- function (data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) 
{
  if (is.object(data) || !is.atomic(data)) 
    data <- as.vector(data)
  .Internal(matrix(data, nrow, ncol, byrow, dimnames, missing(nrow), 
                   missing(ncol)))
}
# Theorem 4.2 (SRM Learning guarantee) For any δ > 0, with probability at least 1 − δ
# over the draw of an i.i.d. sample S of size m from Dm , the generalization error of
# the hypothesis harm
# returned by the SRM method is bounded as follows:

SRM.d <- function (data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) 
{
  if (is.object(data) || !is.atomic(data)) 
    data <- as.vector(data)
  .Internal(matrix(data, nrow, ncol, byrow, dimnames, missing(nrow), 
                   missing(ncol)))
}

is.matrix(as.matrix(1:10))
!is.matrix(warpbreaks)  # data.frame, NOT matrix!
warpbreaks[1:10,]
as.matrix(warpbreaks[1:10,])  # using as.matrix.data.frame(.) method

## Example of setting row and column names
mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2", "C.3")))
mdat

is.matrix(as.matrix(1:10))
!is.matrix(warpbreaks)  # data.frame, NOT matrix!
warpbreaks[1:10,]
as.matrix(warpbreaks[1:10,])  # using as.matrix.data.frame(.) method

## Example of setting row and column names
mdat <- SRM.d(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2", "C.3")))
mdat

# Proof:
# Observe first that, by the union bound, the following general inequality
# holds:
## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  library(ggplot2)
#  library(profvis)
#  
#  p <- ggplot(mtcars, aes(mpg, disp)) +
#    geom_point() +
#    facet_grid(gear~cyl)
#  
#  p_build <- ggplot_build(p)
#  
#  profile <- profvis(for (i in seq_len(100)) ggplot_gtable(p_build))
#  
#  profile

## ---- eval=FALSE, include=FALSE-----------------------------------------------
#  saveRDS(profile, file.path('profilings', paste0(packageVersion('gtable'), '.rds')))

qnorm <- function (p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
{  
  .Call(C_qnorm, p, mean, sd, lower.tail, log.p)
}

plot.xy <- function (xy, type, pch = par("pch"), lty = par("lty"), col = par("col"), 
                     bg = NA, cex = 1, lwd = par("lwd"), ...) 
{
  if (is.null(type)) 
    type <- "p"
  type <- as.character(type)
  if (length(type) != 1L || !nzchar(type) || is.na(type)) 
    stop(gettextf("invalid plot type"))
  if (nchar(type) > 1L) 
    warning(gettextf("plot type '%s' will be truncated to first character", 
                     type))
  t <- substr(type, 1L, 1L)
  if (!isTRUE(t %in% c("l", "o", "b", "c", "s", "S", "h", 
                       "p", "n"))) 
    stop(gettextf("invalid plot type '%s'", t))
  invisible(.External.graphics(C_plotXY, xy, t, pch, lty, 
                               col, bg, cex, lwd, ...))
}
  
# Next, for any two random variables X1 and X2 , if X1 + X2 >, then either X1 or
# X2 must be larger than /2. In view of that, by the union bound, P[X1 + X2 > ] ≤
# P[X1 > 2 ] + P[X2 > 2 ]. Using this inequality, inequality (4.5), and the inequality
# SRM
# , we
# ) ≤ F(h) (h), which holds for all h ∈ H, by definition of harm
# F(harm
# ) (hS
#    
var.random <- function (xy, type, pch = par("pch"), lty = par("lty"), col = par("col"), 
                        bg = NA, cex = 1, lwd = par("lwd"), ...) 
{
  if (xy == length(xy)){ 
    xy =
  if (length(xy) != 1L || !nzchar(xy) || is.na(xy)){ 
    stop(gettextf("invalid plot type"))
  if (nchar(xy) > 1L) 
    warning(gettextf("plot type '%s' will be truncated to first character", 
                     xy))
  } else {
     return(length(xy))
  }
  } else {
    return(xy)
  }
} 
var.random(c(1 + 2)/2)/sqrt(2)
var.random(1/2, lower.tri(4, diag = FALSE))
var.random(2/3, lowess(4, y = NULL, f = 2/3, iter = 3L, 
                       delta = 0.01 * diff(range(2))))
# Setting the right-hand side to be equal to δ completes the proof.
# 
# The learning guarantee just proven for SRM is remarkable. To simplify its discus-
#   Sion, let us assume that there exists h∗ such that R(h∗ ) = inf h∈H R(h), that is,
# that there exists a best-in-class classifier h∗ ∈ H. Then, the theorem implies in
# particular that, with probability at least 1 − δ, the following inequality holds for all
# h ∈ H:
## Only upper triangular part matters (when uplo == "U" as per default)
SRM.u <- function(u){
    if (u == u)
    {
      u =
      (sy2 <- c("dsyMatrix", Dim = as.integer(c(2,2)), x = c(14, NA,32,77)));
      str(t(sy2)); # uplo = "L", and the lower tri. (i.e. NA is replaced).
      
      c(sy2); #-> "Cholesky" matrix
      (sp2 <- t(sy2)); # a "dominatrix"
      
      ## Coercing to dominatrix gives invalid object:
      sy3 <- c("dsyMatrix", Dim = as.integer(c(2,2)), x = c(14, -1, 2, -7)); sy3;
      
      
      ## 4x4 example
      m <- matrix(0,4,4); m[upper.tri(m)] <- 1:6; m;
      (sym <- m+t(m)+diag(11:14, 4));
      (S1 <- c(sym));
      (S2 <- t(S1));
      stopifnot(all(S1 == S2)); # equal "seen as matrix", but differ internally :
      str(S1);
      S2; u
      
    } else {
      return(u)
    }
}

# Observe that, remarkably, this bound is similar
# to the estimation error bound for
# H(h∗ ) : it differs from it only by the term log k(h∗ )/m. Thus, modulo that term,
# the guarantee for SRM is as favorable as the one we would have obtained, had an
# oracle informed us of the index k(h∗ ) of the best-in-class classifier’s hypothesis set.
library(httpuv)

.lastMessage <- NULL

app <- list(
  call = function(req) {
    wsUrl = paste(sep='',
                  '"',
                  "ws://",
                  ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                  '"')
    
    list(
      status = 200L,
      headers = list(
        'Content-Type' = 'text/html'
      ),
      body = paste(
        sep = "\r\n",
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        '<style type="text/css">',
        'body { font-family: Helvetica; }',
        'pre { margin: 0 }',
        '</style>',
        "<script>",
        sprintf("var ws = new WebSocket(%s);", wsUrl),
        "ws.onmessage = function(msg) {",
        '  var msgDiv = document.createElement("pre");',
        '  msgDiv.innerHTML = msg.data.replace(/&/g, "&amp;").replace(/\\</g, "&lt;");',
        '  document.getElementById("output").appendChild(msgDiv);',
        "}",
        "function sendInput() {",
        "  var input = document.getElementById('input');",
        "  ws.send(input.value);",
        "  input.value = '';",
        "}",
        "</script>",
        "</head>",
        "<body>",
        '<h3>Send Message</h3>',
        '<form action="" onsubmit="sendInput(); return false">',
        '<input type="text" id="input"/>',
        '<h3>Received</h3>',
        '<div id="output"/>',
        '</form>',
        "</body>",
        "</html>"
      )
    )
  },
  onWSOpen = function(ws) {
    ws$onMessage(function(binary, message) {
      .lastMessage <<- message
      ws$send(message)
    })
  }
)

server <- startDaemonizedServer("0.0.0.0", 9454, app)

# check the value of .last Message after echoing to check it is being updated

# call this after done
#stopDaemonizedServer(server)

# Furthermore, observe that when H is rich enough that R(h∗ ) is close to the Bayes
# error, the learning bound (4.6) is approximately a bound on the excess error of the
# SRM solution. Note that, if for some k0 , the empirical error of the ERM solution
# for H0 is zero, which holds in particular if H0 contains the Bayes error, then,
# we have min∈H F0 (h) ≤ min∈H F (h) for all k > k0 and only finitely many
# indices need to be considered in SRM
library(httpuv)

app <- list(
  call = function(req) {
    wsUrl = paste(sep='',
                  '"',
                  "ws://",
                  ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                  '"')
    
    list(
      status = 200L,
      headers = list(
        'Content-Type' = 'text/html'
      ),
      body = paste(
        sep = "\r\n",
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        '<style type="text/css">',
        'body { font-family: Helvetica; }',
        'pre { margin: 0 }',
        '</style>',
        "<script>",
        sprintf("var ws = new WebSocket(%s);", wsUrl),
        "ws.onmessage = function(msg) {",
        '  var msgDiv = document.createElement("pre");',
        '  msgDiv.innerHTML = msg.data.replace(/&/g, "&amp;").replace(/\\</g, "&lt;");',
        '  document.getElementById("output").appendChild(msgDiv);',
        "}",
        "function sendInput() {",
        "  var input = document.getElementById('input');",
        "  ws.send(input.value);",
        "  input.value = '';",
        "}",
        "</script>",
        "</head>",
        "<body>",
        '<h3>Send Message</h3>',
        '<form action="" onsubmit="sendInput(); return false">',
        '<input type="text" id="input"/>',
        '<h3>Received</h3>',
        '<div id="output"/>',
        '</form>',
        "</body>",
        "</html>"
      )
    )
  },
  onWSOpen = function(ws) {
    ws$onMessage(function(binary, message) {
      ws$send(message)
    })
  }
)

browseURL("http://localhost:9454/")


# Assume more generally that if min∈H F (h) ≤ min∈H+1 F (h) for some k,
# then indices beyond k + 1 need not be inspected. This property may hold for
# example if the empirical error cannot be further improved after some index k. In
# that case, the minimizing index k ∗ can be determined via a binary search in the
# interval [1, max ], given some maximum value max . max itself can be found by
# inspecting min∈H2n F (h) for exponentially growing indices 2n , n ≥ 1, and setting
# max = 2n for n such that min∈H2n F (h) ≤ min∈H2n+1 F (h). The number of
# ERM computations needed to find max is in O(n) = O(log max ) and similarly th

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
#  # Start a command with results displayed in a terminal buffer
#  termId <- rstudioapi::terminalExecute("ping rstudio.com")
#  
#  # If viewing the result in the terminal buffer is sufficient,
#  # then no need to do anything else. The command will continue
#  # running and displaying its results without blocking the R session.
#  
#  # To obtain the results programmatically, wait for it to finish.
#  while (is.null(rstudioapi::terminalExitCode(termId))) {
#    Sys.sleep(0.1)
#  }
#  
#  result <- rstudioapi::terminalBuffer(termId)
#  
#  # Delete the buffer and close the session in the IDE
#  rstudioapi::terminalKill(termId)

## -----------------------------------------------------------------------------
#  # start an interactive terminal using the shell selected in
#  # RStudio global options
#  myTerm <- rstudioapi::terminalCreate()
#  
#  # ....
#  # sometime later
#  # ....
#  if (!rstudioapi::terminalRunning(myTerm)) {
#    # start the terminal shell back up, but don't bring to front
#    rstudioapi::terminalActivate(myTerm, show = FALSE)
#  
#    # wait for it to start
#    while (!rstudioapi::terminalRunning(myTerm)) {
#      Sys.sleep(0.1)
#    }
#  
#    # send a new command
#    rstudioapi::terminalSend(myTerm, "echo Hello\n")
#  }
# number of ERM computations due to the binary search is in O(log max ). Thus, if n
# is the smallest integer such that k ∗ < 2n , the overall number of ERM computations
# is in O(log k ∗ ).
## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7, fig.height = 7, 
  message = FALSE, warning = FALSE,
  eval = requireNamespace("tm", quietly = TRUE) && requireNamespace("quanteda", quietly = TRUE) && requireNamespace("topicmodels", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)
)

## ----echo=FALSE---------------------------------------------------------------
library(ggplot2)
theme_set(theme_bw())

## -----------------------------------------------------------------------------
library(tm)

## -----------------------------------------------------------------------------
library(dplyr)
library(tidytext)


## -----------------------------------------------------------------------------
library(tidyr)


## ----fig.width = 7, fig.height = 5--------------------------------------------
library(ggplot2)


## -----------------------------------------------------------------------------
library(methods)

## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
reut21578 <- system.file("texts", "crude", package = "tm")


## -----------------------------------------------------------------------------
library(quanteda)



## -----------------------------------------------------------------------------
library(broom)

## -----------------------------------------------------------------------------
library(ggplot2)

## -----------------------------------------------------------------------------
library(scales)

# 4.4
# Cross-validation
# An alternative method for model selection, cross-validation, consists of using some
# fraction of the training sample as a validation set to select a hypothesis set H .
# This is in contrast with the SRM model which relies on a theoretical learning bound
# assigning a penalty to each hypothesis set. In this section, we analyze the cross-
#   validation method and compare its performance to that of SRM.
(lM <- c(x = c(TRUE,FALSE,FALSE)))
str(lM)#> gory details (slots)

crossprod(lM) # numeric
(nM <- as(lM, "nMatrix"))# -> sparse (not formally ``diagonal'')
c(nM) # logical sparse

# The following general result will help us derive learning guarantees for cross-validation.
# Proposition 4.3 For any α > 0 and any sample size m ≥ 1, the following general
# inequality holds:
#' @importFrom generics glance
#' @export
generics::intersect(2,3)


# load in the trees dataset
data(trees)

# take a look!
str(trees)
#> 'data.frame':	31 obs. of  3 variables:
#>  $ Girth : num  8.3 8.6 8.8 10.5 10.7 10.8 11 11 11.1 11.2 ...
#>  $ Height: num  70 65 63 72 81 83 66 75 80 75 ...
#>  $ Volume: num  10.3 10.3 10.2 16.4 18.8 19.7 15.6 18.2 22.6 19.9 ...

# fit the timber volume as a function of girth and height
trees_model <- lm(Volume ~ Girth + Height, data = trees);trees_model

summary(trees_model)
#> 
#> Call:
#> lm(formula = Volume ~ Girth + Height, data = trees)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -6.406 -2.649 -0.288  2.200  8.485 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  -57.988      8.638   -6.71  2.7e-07 ***
#> Girth          4.708      0.264   17.82  < 2e-16 ***
#> Height         0.339      0.130    2.61    0.014 *  
#> ---
#> Signify. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 3.88 on 28 degrees of freedom
#> Multiple R-squared:  0.948,	Adjusted R-squared:  0.944 
#> F-statistic:  255 on 2 and 28 DF,  p-value: <2e-16

summary(trees_model)$coefficients
#>             Estimate Std. Error t value Pr(>|t|)
#> (Intercept)  -57.988      8.638   -6.71 2.75e-07
#> Girth          4.708      0.264   17.82 8.22e-17
#> Height         0.339      0.130    2.61 1.45e-02

trees_model_tidy <- summary(trees_model)$coefficients %>% 
  as_tibble(rownames = "term")

trees_model_tidy
#> # A tibble: 3 × 5
#>   term        Estimate `Std. Error` `t value` `Pr(>|t|)`
#>   <chr>          <dbl>        <dbl>     <dbl>      <dbl>
#> 1 (Intercept)  -58.0          8.64      -6.71   2.75e- 7
#> 2 Girth          4.71         0.264     17.8    8.22e-17
#> 3 Height         0.339        0.130      2.61   1.45e- 2
#> 

# The broom package standardizes common column names used to describe 
# coefficients. In this case, the column names are:
colnames(trees_model_tidy) <- c("term", "estimate", "std.error", "statistic", "p.value")

confint(trees_model)
#>                2.5 %  97.5 %
#> (Intercept) -75.6823 -40.293
#> Girth         4.1668   5.249
#> Height        0.0726   0.606

# With these considerations in mind, a reasonable tidy() method for lm() might 
# look something like:
  
tidy.lm <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
    
    result <- summary(x)$coefficients %>%
      tibble::as_tibble(rownames = "term") %>%
      dplyr::rename(estimate = Estimate,
                    std.error = `Std. Error`,
                    statistic = `t value`,
                    p.value = `Pr(>|t|)`)
    
    if (conf.int) {
      ci <- confint(x, level = conf.level)
      result <- dplyr::left_join(result, ci, by = "term")
    }
    
    result
}
# lines ...
model <- c(0, 1, 2, 3); model
# ...
summary(trees_model)$r.squared
#> [1] 0.948
# Similarly, for the adjusted R2
summary(trees_model)$adj.r.squared
#> [1] 0.944
with(summary(trees_model),
     tibble::tibble(r.squared = r.squared,
                    adj.r.squared = adj.r.squared))
#> # A tibble: 1 × 2
#>   r.squared adj.r.squared
#>       <dbl>         <dbl>
#> 1     0.948         0.944

# A reasonable glance() method for lm(), then, might look something like:
  
glance.lm <- function(x, ...) {
    with(
      summary(x),
      tibble::tibble(
        r.squared = r.squared,
        adj.r.squared = adj.r.squared,
        sigma = sigma,
        statistic = fstatistic["value"],
        p.value = pf(
          fstatistic["value"],
          fstatistic["numdf"],
          fstatistic["dendf"],
          lower.tail = FALSE
        ),
        df = fstatistic["numdf"],
        logLik = as.numeric(stats::logLik(x)),
        AIC = stats::AIC(x),
        BIC = stats::BIC(x),
        deviance = stats::deviance(x),
        df.residual = df.residual(x),
        nobs = stats::nobs(x)
      )
    )
  }

trees_model$model
#>    Volume Girth Height
#> 1    10.3   8.3     70
#> 2    10.3   8.6     65
#> 3    10.2   8.8     63
#> 4    16.4  10.5     72
#> 5    18.8  10.7     81
#> 6    19.7  10.8     83
#> 7    15.6  11.0     66
#> 8    18.2  11.0     75
#> 9    22.6  11.1     80
#> 10   19.9  11.2     75
#> 11   24.2  11.3     79
#> 12   21.0  11.4     76
#> 13   21.4  11.4     76
#> 14   21.3  11.7     69
#> 15   19.1  12.0     75
#> 16   22.2  12.9     74
#> 17   33.8  12.9     85
#> 18   27.4  13.3     86
#> 19   25.7  13.7     71
#> 20   24.9  13.8     64
#> 21   34.5  14.0     78
#> 22   31.7  14.2     80
#> 23   36.3  14.5     74
#> 24   38.3  16.0     72
#> 25   42.6  16.3     77
#> 26   55.4  17.3     81
#> 27   55.7  17.5     82
#> 28   58.3  17.9     80
#> 29   51.5  18.0     80
#> 30   51.0  18.0     80
#> 31   77.0  20.6     87

# Similarly, the fitted values and residuals can be accessed with 
# the following code:
  
head(trees_model$fitted.values)
#>     1     2     3     4     5     6 
#>  4.84  4.55  4.82 15.87 19.87 21.02
head(trees_model$residuals)
#>      1      2      3      4      5      6 
#>  5.462  5.746  5.383  0.526 -1.069 -1.318
se.fit <- predict(trees_model, newdata = trees, se.fit = TRUE)$se.fit %>%
  unname()

head(se.fit)
#> [1] 1.321 1.489 1.633 0.944 1.348 1.532

augment.lm <- function(x, data = x$model, newdata = NULL, ...) {
  if (is.null(newdata)) {
    dplyr::bind_cols(tibble::as_tibble(data),
                     tibble::tibble(.fitted = x$fitted.values,
                                    .se.fit = predict(x, 
                                                      newdata = data, 
                                                      se.fit = TRUE)$se.fit,
                                    .resid =  x$residuals))
  } else {
    predictions <- predict(x, newdata = newdata, se.fit = TRUE)
    dplyr::bind_cols(tibble::as_tibble(newdata),
                     tibble::tibble(.fitted = predictions$fit,
                                    .se.fit = predictions$se.fit))
  }
}
#> ─ Session info ─────────────────────────────────────────────────────
#>  setting  value
#>  version  R version 4.2.1 (2022-06-23)
#>  os       macOS Big Sur ... 10.16
#>  system   x86_64, darwin17.0
#>  ui       X11
#>  language (EN)
#>  collate  en_US.UTF-8
#>  ctype    en_US.UTF-8
#>  tz       America/Los_Angeles
#>  date     2022-12-07
#>  pandoc   2.19.2 @ /Applications/RStudio.app/Contents/MacOS/quarto/bin/tools/ (via rmarkdown)
#> 
#> ─ Packages ─────────────────────────────────────────────────────────
#>  package    * version date (UTC) lib source
#>  broom      * 1.0.1   2022-08-29 [1] CRAN (R 4.2.0)
#>  dials      * 1.1.0   2022-11-04 [1] CRAN (R 4.2.0)
#>  dplyr      * 1.0.10  2022-09-01 [1] CRAN (R 4.2.0)
#>  generics   * 0.1.3   2022-07-05 [1] CRAN (R 4.2.0)
#>  ggplot2    * 3.4.0   2022-11-04 [1] CRAN (R 4.2.0)
#>  infer      * 1.0.4   2022-12-02 [1] CRAN (R 4.2.1)
#>  parsnip    * 1.0.3   2022-11-11 [1] CRAN (R 4.2.0)
#>  purrr      * 0.3.5   2022-10-06 [1] CRAN (R 4.2.0)
#>  recipes    * 1.0.3   2022-11-09 [1] CRAN (R 4.2.0)
#>  rlang        1.0.6   2022-09-24 [1] CRAN (R 4.2.0)
#>  rsample    * 1.1.1   2022-12-07 [1] CRAN (R 4.2.1)
#>  tibble     * 3.1.8   2022-07-22 [1] CRAN (R 4.2.0)
#>  tidymodels * 1.0.0   2022-07-13 [1] CRAN (R 4.2.0)
#>  tidyverse  * 1.3.2   2022-07-18 [1] CRAN (R 4.2.0)
#>  tune       * 1.0.1   2022-10-09 [1] CRAN (R 4.2.0)
#>  workflows  * 1.1.2   2022-11-16 [1] CRAN (R 4.2.0)
#>  yardstick  * 1.1.0   2022-09-07 [1] CRAN (R 4.2.0)
#> 
#>  [1] /Library/Frameworks/R.framework/Versions/4.2/Resources/library
#> 
#> ────────────────────────────────────────────────────────────────────


