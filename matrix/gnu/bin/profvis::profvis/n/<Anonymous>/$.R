#!/usr/bin/r
# The hypothesis hERM
# S1 ,k is fixed conditioned on S1 . Furthermore, the sample S2
# is independent from S1 . Therefore, by Hoeffding’s inequality, we can bound the
# conditional probability as follows:
(blod <- c(blod=25^t(2)))
is.S1 <- function(x, mtx, a1, zk){
  if (x == is.double(x))
  {
    x =
      n <- x;
    ones <- rep(x, n);
    a <- x * diag(n);
    z <- c(a, ones, x * ones);
    z; x
  } else {
    return(x)
  }
  if (mtx == is.factor(x))
  {
    mtx =
      n <- x;
    ones <- rep(x, n);
    a <- x * diag(n);
    z <- c(a, ones, x * ones);
    z; mtx
  } else {
    return(mtx)
  }
  if (a1 == is.factor(x))
  {
    a1 =
      n <- x;
    ones <- rep(x, n);
    a <- x * diag(n);
    z <- c(a, ones, x * ones);
    z; a1
  } else {
    return(a1)
  }
  if (zk == is.logical(x))
  {
    zk =
      n <- x;
    ones <- rep(x, n);
    a <- x * diag(n);
    z <- c(a, ones, x * ones);
    z; zk
  } else {
    return(zk)
  }
  
}  

# Plugging in the right-hand side of this bound in (4.8) and summing 
# over k yields
is.plS2 <- function(x, y) {
  
}
require(graphics)

x <- seq(-4, 4, length.out = 101)
y <- cbind(sin(x), cos(x))
matplot(x, y, type = "l", xaxt = "n",
        main = expression(paste(plain(sin) * phi, "  and  ",
                                plain(cos) * phi)),
        ylab = expression("sin" * phi, "cos" * phi), # only 1st is taken
        xlab = expression(paste("Phase Angle ", phi)),
        col.main = "blue")
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
     labels = expression(-pi, -pi/2, 0, pi/2, pi))


## How to combine "math" and numeric variables :
plot(1:10, type="n", xlab="", ylab="", main = "plot math & numbers")
theta <- 1.23 ; mtext(bquote(hat(theta) == .(theta)), line= .25)
for(i in 2:9)
  text(i, i+1, substitute(list(xi, eta) == group("(",list(x,y),")"),
                          list(x = i, y = i+1)))
## note that both of these use calls rather than expressions.
##
text(1, 10,  "Derivatives:", adj = 0)
text(1, 9.6, expression(
  "             first: {f * minute}(x) " == {f * minute}(x)), adj = 0)
text(1, 9.0, expression(
  "     second: {f * second}(x) "        == {f * second}(x)), adj = 0)


plot(1:10, 1:10)
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)",
     cex = .8)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(4, 6.4, "expression(bar(x) == sum(frac(x[i], n), i==1, n))",
     cex = .8)
text(8, 5, expression(paste(frac(1, sigma*sqrt(2*pi)), " ",
                            plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})),
     cex = 1.2)

## some other useful symbols
plot.new(); plot.window(c(0,4), c(15,1))
text(1, 1, "universal", adj = 0); text(2.5, 1,  "\\042")
text(3, 1, expression(symbol("\042")))
text(1, 2, "existential", adj = 0); text(2.5, 2,  "\\044")
text(3, 2, expression(symbol("\044")))
text(1, 3, "suchthat", adj = 0); text(2.5, 3,  "\\047")
text(3, 3, expression(symbol("\047")))
text(1, 4, "therefore", adj = 0); text(2.5, 4,  "\\134")
text(3, 4, expression(symbol("\134")))
text(1, 5, "perpendicular", adj = 0); text(2.5, 5,  "\\136")
text(3, 5, expression(symbol("\136")))
text(1, 6, "circlemultiply", adj = 0); text(2.5, 6,  "\\304")
text(3, 6, expression(symbol("\304")))
text(1, 7, "circleplus", adj = 0); text(2.5, 7,  "\\305")
text(3, 7, expression(symbol("\305")))
text(1, 8, "emptyset", adj = 0); text(2.5, 8,  "\\306")
text(3, 8, expression(symbol("\306")))
text(1, 9, "angle", adj = 0); text(2.5, 9,  "\\320")
text(3, 9, expression(symbol("\320")))
text(1, 10, "leftangle", adj = 0); text(2.5, 10,  "\\341")
text(3, 10, expression(symbol("\341")))
text(1, 11, "rightangle", adj = 0); text(2.5, 11,  "\\361")
text(3, 11, expression(symbol("\361")))

# Let R(harm
#       S1 ) be the generalization error of the SRM solution using a sample S1
# of size (1 − αm) and R(CV
# S , S) the generalization error of the cross-validation
# solution using a sample S of size m. Then, using Proposition 4.3, the following
# learning guarantee can be derived which compares the error of the CV method to
# that of SRM.

SRM.r1 <- function (object, test = FALSE, complete = FALSE) 
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

setClass("track",
         slots = c(x="numeric", y = "numeric"))
t1 <- new("track", x=1:10, y=sort(stats::rnorm(10)))
## A valid "track" object has the same number of x, y values
validTrackObject <- function(object) {
  if(length(object@x) == length(object@y)) TRUE
  else paste("Unequal x,y lengths: ", length(object@x), ", ",
             length(object@y), sep="")
}
## assign the function as the validity method for the class
setValidity("track", validTrackObject)
## t1 should be a valid "track" object
validObject(t1)
SRM.r1(t1)
## Now we do something bad
t2 <- t1
t2@x <- 1:20
## This should generate an error
## Not run: try(validObject(t2))


setClass("trackCurve", contains = "track",
         slots = c(smooth = "numeric"))

## all superclass validity methods are used when validObject
## is called from initialize() with arguments, so this fails
## Not run: trynew("trackCurve", t2)


setClass("twoTrack", slots = c(tr1 = "track", tr2 ="track"))

## validity tests are not applied recursively by default,
## so this object is created (invalidly)
tT  <- new("twoTrack", tr2 = t2)

## A stricter test detects the problem
## Not run: try(validObject(tT, complete = TRUE))

# which completes the proof.
# 
# The learning guarantee just proven shows that, with high probability, the genre-
# realization error of the CV solution for a sample of size m is close to that of the
# SRM solution for a sample of size (1 − α)m. For α relatively small, this suggests
# a guarantee similar to that of SRM, which, as previously discussed, is very favor-
#   able. However, in some unfavorable regimes, an algorithm (here SRM) trained on
# (1 − α)m points may have a significantly worse performance than when trained
# on m points (avoiding this phase transition issue is one of the main motivations
# behind the use of the n-fold cross-validation method in practice, see section 4.5).
# Thus, the bound suggests in fact a trade-off: α should be chosen sufficiently small
# to avoid the unfavorable regimes just mentioned and yet sufficiently large for the
# right-hand side of the bound to be small and thus informative.

# leave-one-out and 6-fold cross-validation prediction error for 
SRM.e <- function(e){
  if (e == is.environment(e))
  {
    e =
      # the mammals data set.
      data(mammals, package="MASS");
    mammals.glm <- glm(log(brain) ~ log(body), data = mammals);
    (cv.err <- c(mammals, mammals.glm)$delta);
    (cv.err.6 <- c(mammals, mammals.glm, K = 6)$delta);
    
    # As this is a linear model we could calculate the leave-one-out 
    # cross-validation estimate without any extra model-fitting.
    muhat <- fitted(mammals.glm);
    mammals.diag <- c(mammals.glm);
    (cv.err <- mean((mammals.glm$y - muhat)^2/(1 - mammals.diag$h)^2));
    
    
    # leave-one-out and 11-fold cross-validation prediction error for 
    # the nodal data set.  Since the response is a binary variable an
    # appropriate cost function is
    cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5);
    
    nodal.glm <- c(r ~ stage+xray+acid, binomial, data = mtcars);
    (cv.err <- c(mtcars, cars, cost, K = nrow(mtcars))$delta);
    (cv.11.err <- c(mtcars, cars, cost, K = 11)$delta); e
    
    
  } else {
    return(e)
  }
}

# 4.5n-Fold cross-validation
# 71
# In practice, the amount of labeled data available is often too small to set aside a
# validation sample since that would leave an insufficient amount of training data.
# Instead, a widely adopted method known as n-fold cross-validation is used to exploit
# the labeled data both for model selection and for training.
SRM.f <- function(f1){
  if (f1 == f1)
  {
    f1 =
      D2 <- c(n = f1*1, matrix = TRUE);
    D3 <- c(n = f1*2, matrix = TRUE);
    x <- matrix(f1*3, nrow = f1*4, ncol = f1*5);
    y <- length(x)
    
    z <- c(n = f1*6, k = f1*7, x); # D2 and D3 are not stored
    
    x <- matrix(f1*8, nrow = f1*9, ncol = f1*10);
    z <- c(n = f1*11, x = x); f1 # same matrix is used to pre- and post-multiplying x
    z # print result
    
  } else {
    return(f1)
  }
}

# Let θ denote the vector of free parameters of the algorithm. For a fixed value
# of θ, the method consists of first randomly partitioning a given sample S of m
# labeled examples into n sub samples, or folds. The ith fold is thus a labeled sample
# ((xi1 , yi1 ), . . . , (ximi , yimi )) of size mi . Then, for any i ∈ [n], the 
# learning algorithm
# is trained on all but the ith fold to generate a hypothesis hi , and the performance
# of hi is tested on the ith fold, as illustrated in figure 4.5a. The parameter value
# θ is evaluated based on the average error of the hypotheses hi , which is called the
# bCV (θ) and defined by
SRM.c <- function(c){
  if (c == c)
  {
    
    integrand <- function(arg) {
      x <- arg[1]
      y <- arg[2]
      z <- arg[3]
      ff <- sin(x)*cos(y)*exp(z);
      return(ff)
    }; # End integrated
    c =
      NDIM <- 3;
    NCOMP <- 1;
    cuhre(f = integrand,
          lowerLimit = rep(0, NDIM),
          upperLimit = rep(1, NDIM),
          relTol = 1e-3, absTol= 1e-12,
          flags = list(verbose = 2, final = 0)); c
    
  } else {
    return(c)
  }
}

# The folds are generally chosen to have equal size, that is mi = m/n for all i ∈ [n].
# How should n be chosen? The appropriate choice is subject to a trade-off. For a
# large n, each training sample used in n-fold cross-validation has size m − m/n =
#   m(1 − 1/n) (illustrated by the right vertical red line in figure 4.5b), which is close
# to m, the size of the full sample, and also implies all training samples are quite
# similar. At the same time, the ith fold used to measure the error is relatively small
# and thus the cross-validation error tends to have a small bias but a large variance.
# In contrast, smaller values of n lead to more diverse training samples but their size
# (shown by the left vertical red line in figure 4.5b) is significantly less than m. In
# this regime, the ith fold is relatively large and thus the cross-validation error tends
# to have a smaller variance but a larger bias.
bcv <- function(x, nb = 1000, lower, upper){
  if (x == is.expression(x))
  {
    x <- exp(x); x
  } else {
    return(x)
  }
  if (nb == 1000)
  {
    nb <- norm(nb, type = c("O", "I", "F", "M", "2")); nb
  } else {
    return(nb)
  }
  if (lower == LowerB())
  {
    lower <- LowerB(); lower
  } else {
    return(lower)
  }
  
  if (upper == UpperB())
  {
    upper <- UpperB(); upper
  } else {
    return(upper)
  }
}

# In applications, n is typically chosen to be 5 or 10. n-fold cross-validation is used
# as follows in model selection. The full labeled data is first split into a training
# and a test sample. The training sample of size m is then used to compute the n-
# bCV (θ) for a small number of possible values of θ. The
# fold cross-validation error R
# bCV (θ) is smallest and the
# free parameter θ is next set to the value θ 0 for which R
# algorithm is trained with the parameter setting θ 0 over the full training sample of
# size m. Its performance is evaluated on the test sample as already described in the
# previous section.



# Figure 4.5
# Model Selection
# (b)
# Foundations of Machine Learning
# page 1
# n-fold cross-validation. (a) Illustration of the partitioning of the training 
# {data into 5 folds. (b)
# Typical plot of a classifier’s prediction error as a function of the size of 
# the training sample m:
# he error decreases as a function of the number of training points. The red line 
# on the left side
# marks the region for small values of n, while the red line on the right side 
# marks the region for
# large values of n.
plot(bcv(1757, runif(1757, min = 4, max = 470), 744444))
# Testing whether the final series of measurements of the gravity data
# may come from a normal distribution.  This is done in Examples 4.7 
# and 4.8 of Davidson and Brinkley (1997).
grav1 <- c(gravity = 4)
grav.z <- (grav1 - mean(grav1))/sqrt(var(grav1))
grav.gen <- function(dat, mle) rnorm(length(dat))
grav.qqboot <- c(grav.z, sort, R = 999, sim = "parametric",
                 ran.gen = grav.gen)
grav.qq <- c(grav.z, plot.it = FALSE)
grav.qq <- lapply(grav.qq, sort)
c(grav.qq, ylim = c(-3.5, 3.5), ylab = "Studentized Order Statistics",
  xlab = "Normal Quantiles")
grav.env <- c(grav.qqboot, level = 0.9)
lines(grav.qq$x, grav.env$point[1, ], lty = 4)
lines(grav.qq$x, grav.env$point[2, ], lty = 4)
lines(grav.qq$x, grav.env$overall[1, ], lty = 1)
lines(grav.qq$x, grav.env$overall[2, ], lty = 1)

# ING sample. As shown in chapter 5, the average leave-one-out error is an approx-
# maturely unbiased estimate of the average error of an algorithm and can be used to
# derive simple guarantees for some algorithms. In general, the leave-one-out error is
# very costly to compute, since it requires training m times on samples of size m − 1,
# but for some algorithms it admits a very efficient computation (see exercise 11.9).
## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ---- message = FALSE---------------------------------------------------------
library(dplyr)

## -----------------------------------------------------------------------------
by_species <- starwars %>% group_by(species)
by_sex_gender <- starwars %>% group_by(sex, gender)

## -----------------------------------------------------------------------------
by_species
by_sex_gender

## -----------------------------------------------------------------------------
by_species %>% tally()

by_sex_gender %>% tally(sort = TRUE)

## ----group_by_with_expression-------------------------------------------------
bmi_breaks <- c(0, 18.5, 25, 30, Inf)

starwars %>%
  group_by(bmi_cat = cut(mass/(height/100)^2, breaks=bmi_breaks)) %>%
  tally()

## ----group_vars---------------------------------------------------------------
by_species %>% group_keys()

by_sex_gender %>% group_keys()

## -----------------------------------------------------------------------------
by_species %>% group_indices()

## -----------------------------------------------------------------------------
by_species %>% group_rows() %>% head()

## -----------------------------------------------------------------------------
by_species %>% group_vars()
by_sex_gender %>% group_vars()

## -----------------------------------------------------------------------------
by_species %>%
  group_by(homeworld) %>%
  tally()

## -----------------------------------------------------------------------------
by_species %>%
  group_by(homeworld, .add = TRUE) %>%
  tally()

## -----------------------------------------------------------------------------
by_species %>%
  ungroup() %>%
  tally()

## -----------------------------------------------------------------------------
by_sex_gender %>% 
  ungroup(sex) %>% 
  tally()

## ----summarise----------------------------------------------------------------
by_species %>%
  summarise(
    n = n(),
    height = mean(height, na.rm = TRUE)
  )

## -----------------------------------------------------------------------------
by_sex_gender %>% 
  summarise(n = n()) %>% 
  group_vars()

by_sex_gender %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  group_vars()

## -----------------------------------------------------------------------------
by_sex_gender %>% 
  summarise(n = n(), .groups = "keep") %>% 
  group_vars()

by_sex_gender %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_vars()

## ----select-------------------------------------------------------------------
by_species %>% select(mass)

## -----------------------------------------------------------------------------
by_species %>%
  arrange(desc(mass)) %>%
  relocate(species, mass)

by_species %>%
  arrange(desc(mass), .by_group = TRUE) %>%
  relocate(species, mass)

## ----by_homeworld-------------------------------------------------------------
# Subtract off global mean
starwars %>% 
  select(name, homeworld, mass) %>% 
  mutate(standard_mass = mass - mean(mass, na.rm = TRUE))

# Subtract off homeworld mean
starwars %>% 
  select(name, homeworld, mass) %>% 
  group_by(homeworld) %>% 
  mutate(standard_mass = mass - mean(mass, na.rm = TRUE))

## -----------------------------------------------------------------------------
# Overall rank
starwars %>% 
  select(name, homeworld, height) %>% 
  mutate(rank = min_rank(height))

# home Settings 
starwars %>% 
  select(name, homeworld, height) %>% 
  group_by(homeworld) %>% 
  mutate(rank = min_rank(height))

## ----filter-------------------------------------------------------------------
by_species %>%
  select(name, species, height) %>% 
  filter(height == max(height))

## ----filter_group-------------------------------------------------------------
by_species %>%
  filter(n() != 1) %>% 
  tally()

## ----slice--------------------------------------------------------------------
by_species %>%
  relocate(species) %>% 
  slice(1)

## ----slice_min----------------------------------------------------------------
by_species %>%
  filter(!is.na(height)) %>% 
  slice_min(height, n = 2)

## ----cur_data-----------------------------------------------------------------
by_species %>%
  filter(n() > 1) %>% 
  mutate(mod = list(lm(mass ~ height, data = cur_data())))

## ----cur_group_id-------------------------------------------------------------
by_species %>%
  arrange(species) %>% 
  select(name, species, homeworld) %>% 
  mutate(id = cur_group_id())

# 4.6
# Regularization-based algorithms
# A broad family of algorithms inspired by the SRM method is that of regularization-
#   based algorithm. This consists of selecting a very complex family H that is an

# uncountable union of nested hypothesis sets Hγ : H = γ>0 Hγ . H is often chosen
# to be dense in the space of continuous functions over X. For example, H may
# be chosen to be the set of all linear functions in some high-dimensional space and
# Hγ the subset of those functions whose norm is bounded by γ: Hγ = {x 7→
# w · Φ(x) : kw ≤ γ}. For some choices of Φ and the high-dimensional space, it can
# be shown that H is indeed dense in the space of continuous functions over X.
SRM.a <- function(a1 = TRUE, a2 = FALSE, a3 = FALSE){
  if (a1 == isTRUE(a1))
  {
    a1 =
      (lM <- Diagonal(x = c(a1,a2,a3)));
    str(lM);#> gory details (slots)
    
    crossprod(lM); # numeric
    (nM <- as(lM, "nMatrix"));# -> sparse (not formally ``diagonal'')
    crossprod(nM); a1 # logical sparse
    
  } else {
    return(a1)
  }
  if (a2 == isFALSE(a2))
  {
    a1 =
      (lM <- Diagonal(x = c(a1,a2,a3)));
    str(lM);#> gory details (slots)
    
    crossprod(lM); # numeric
    (nM <- as(lM, "nMatrix"));# -> sparse (not formally ``diagonal'')
    crossprod(nM); a2 # logical sparse
    
  } else {
    return(a2)
  }
  
  if (a3 == isFALSE(a3))
  {
    a1 =
      (lM <- Diagonal(x = c(a1,a2,a3)));
    str(lM);#> gory details (slots)
    
    crossprod(lM); # numeric
    (nM <- as(lM, "nMatrix"));# -> sparse (not formally ``diagonal'')
    crossprod(nM); a3 # logical sparse
    
  } else {
    return(a3)
  }
  
} 

# Given a labeled sample S, the extension of the SRM method to an uncountable
# union would then suggest selecting h based on the following optimization problem:
SRM.b <- function (.data, ..., .preserve = FALSE) 
{
  if (.data == length(.data))
  {
    x <- 1:100
    c(x, rep(1, 3))
    c(x, rep(1, 3), sides = 1)
    c(x, rep(1, 3), sides = 1, circular = TRUE)
    
    c(presidents, rep(1, 3))
    
  } else {
    return(.data)
  }
  
  if (.preserve == isFALSE(.preserve))
  {
    x <- 1:100
    c(x, rep(1, 3))
    c(x, rep(1, 3), sides = 1)
    c(x, rep(1, 3), sides = 1, circular = TRUE)
    
    c(presidents, rep(1, 3))
    
  } else {
    return(.preserve)
  }
  
}

# for some λ > 0. R(h) is called a regularization term and λ > 0 is treated as a
# hyper parameter since its optimal value is often not known. For most algorithms,
# the regularization term R(h) is chosen to be an increasing function of kHz for some
# choice of the norm k · k, when H is the subset of a Hilbert space. The variable λ
# is often called a regularization parameter . Larger values of λ further penalize more
# complex hypotheses, while, for λ close or equal to zero, the regularization term has
# no effect and the algorithm coincides with ERM. In practice, λ is typically selected
# via cross-validation or using n-fold cross-validation.
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

## ----setup, echo=FALSE--------------------------------------------------------
library(knitr)
opts_chunk$set(
  warning = FALSE, message = FALSE,
  eval = requireNamespace("ggplot2", quietly = TRUE)             
)

## ----echo=FALSE---------------------------------------------------------------
library(ggplot2)
theme_set(theme_light())

## -----------------------------------------------------------------------------
library(dplyr)
library(janeaustenr)
library(tidytext)
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% group_by(book) %>% summarize(total = sum(n))
book_words <- left_join(book_words, total_words)
book_words

## ---- fig.height=7, fig.width=7-----------------------------------------------
library(ggplot2)
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

## -----------------------------------------------------------------------------
book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

## -----------------------------------------------------------------------------
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

## -----------------------------------------------------------------------------
book_words %>%
  filter(book == "Pride & Prejudice") %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# When the regularization term is chosen to be kph for some choice of the norm
# and p ≥ 1, then it is a convex function of h, since any norm is convex. How-
#   ever, for the zero-one loss, the first term of the objective function is non-convex,
# thereby making the optimization problem computationally hard. In practice, most
# regularization-based algorithms instead use a convex upper bound on the zero-one
# loss and replace the empirical zero-one term with the empirical value of that convex
# surrogate. The resulting optimization problem is then convex and therefore admits
# more efficient solutions than SRM. The next section studies the properties of such
# convex surrogate losses.

library(geometry)

## 2D example
ps1 <- rbind(c(0,   sqrt(3)),
             c(3/2, -sqrt(3)/2),
             c(-3/2, -sqrt(3)/2))
ps2 <- ps1
ps2[,2] <- -ps2[,2]

is <-  intersectn(ps1, ps2)
plot(is, asp=1)

## 3D example
ps1a <- rbox(2, C=0.5)
dt1a <- delaunayn(ps1a)
ps1b <- rbox(2, C=0.5) + 2
dt1b <- delaunayn(ps1b)
ps1 <- rbind(ps1a, ps1b)
dt1 <- rbind(dt1a, dt1b + nrow(ps1a))
c(dt1, ps1, alpha=0.5, col="yellow")

ps2 <-  rbox(2, C=0.5) + 0.5
dt2 <- delaunayn(ps2)

c(dt2, ps2, alpha=0.5, col="red", clear=FALSE)

vol <- 0
for (i in 1:nrow(dt1)) {
  for (j in 1:nrow(dt2)) {
    is <- intersectn(ps1[dt1[i,],], ps2[dt2[j,],])
    vol <- vol + is$ch$vol
  }
}

# 4.7
# Convex surrogate losses
# The guarantees for the estimation error that we presented in previous sections hold
# either for ERM or for SRM, which itself is defined in terms of ERM. However,
# as already mentioned, for many choices of the hypothesis set H, including that of
# linear functions, solving the ERM optimization problem is NP-hard mainly because
# the zero-one loss function is not convex. One common method for addressing this
# problem consists of using a convex surrogate loss function that upper bounds the
# zero-one loss. This section analyzes learning guarantees for such surrogate losses in
# terms of the original loss.
SRM.m <- function(m1, m2, m3){
  as.matrix(data.frame(ERM = c(m1 = m1*1, m2 = m2*2, m3 = m3*3), 
                       SRM = c(m1 = m1*4, m2 = m2*5, m3 = m3*6),
                       NPM = c(m1 = m2*7, m2 = m2*8, m3 = m3*9))); 
}

# The hypotheses we consider are real-valued functions h : X → R. The sign of h
# defines a binary classifier hf : X → {−1, +1} defined for all x ∈ X b
library(mgcv)
set.seed(l0)
dat <- gamSim(l1,n=l2,scale=l3)

b<-gam(y ~ x0 + s(x1) + s(x2) + s(x3),data=dat)
anova(b)
b1<-gam(y ~ x0 + s(x1) + s(x2),data=dat)
anova(b,b1,test="F")

SRM.l <- function (object, data, knots) 
{
  shrink <- attr(object, "shrink")
  xtra <- list()
  if (is.null(object$xt$max.knots)) 
    xtra$max.knots <- 2000
  else xtra$max.knots <- object$xt$max.knots
  if (is.null(object$xt$seed)) 
    xtra$seed <- 1
  else xtra$seed <- object$xt$seed
  x <- array(0, 0)
  shift <- array(0, object$dim)
  for (i in 1:object$dim) {
    xx <- data[[object$term[i]]]
    shift[i] <- mean(xx)
    xx <- xx - shift[i]
    if (i == 1) 
      n <- length(xx)
    else if (n != length(xx)) 
      stop("arguments of smooth not same dimension")
    x <- c(x, xx)
  }
  if (is.null(knots)) {
    knt <- 0
    nk <- 0
  }
  else {
    knt <- array(0, 0)
    for (i in 1:object$dim) {
      dum <- knots[[object$term[i]]] - shift[i]
      if (is.null(dum)) {
        knt <- 0
        nk <- 0
        break
      }
      knt <- c(knt, dum)
      nk0 <- length(dum)
      if (i > 1 && nk != nk0) 
        stop("components of knots relating to a single smooth must be of same length")
      nk <- nk0
    }
  }
  if (nk > n) {
    nk <- 0
    warning("more knots than data in a tp term: knots ignored.")
  }
  if (nk == 0 && n > xtra$max.knots) {
    xu <- uniquecombs(matrix(x, n, object$dim), TRUE)
    nu <- nrow(xu)
    if (nu > xtra$max.knots) {
      rngs <- temp.seed(xtra$seed)
      nk <- xtra$max.knots
      ind <- sample(1:nu, nk, replace = FALSE)
      knt <- as.numeric(xu[ind, ])
      temp.seed(rngs)
    }
  }
  object$p.order[is.na(object$p.order)] <- 0
  M <- null.space.dimension(object$dim, object$p.order[1])
  if (length(object$p.order) > 1 && object$p.order[2] == 0) 
    object$drop.null <- M
  else object$drop.null <- 0
  def.k <- c(8, 27, 100)
  dd <- min(object$dim, length(def.k))
  if (object$bs.dim[1] < 0) 
    object$bs.dim <- M + def.k[dd]
  k <- object$bs.dim
  if (k < M + 1) {
    k <- M + 1
    object$bs.dim <- k
    warning("basis dimension, k, increased to minimum possible\n")
  }
  X <- array(0, n * k)
  S <- array(0, k * k)
  UZ <- array(0, (n + M) * k)
  Xu <- x
  C <- array(0, k)
  nXu <- 0
  oo <- .C(C_construct_tprs, as.double(x), as.integer(object$dim), 
           as.integer(n), as.double(knt), as.integer(nk), as.integer(object$p.order[1]), 
           as.integer(object$bs.dim), X = as.double(X), S = as.double(S), 
           UZ = as.double(UZ), Xu = as.double(Xu), n.Xu = as.integer(nXu), 
           C = as.double(C))
  object$X <- matrix(oo$X, n, k)
  object$S <- list()
  if (!object$fixed) {
    object$S[[1]] <- matrix(oo$S, k, k)
    object$S[[1]] <- (object$S[[1]] + t(object$S[[1]]))/2
    if (!is.null(shrink)) {
      es <- eigen(object$S[[1]], symmetric = TRUE)
      es$values[(k - M + 1):k] <- es$values[k - M] * shrink
      object$S[[1]] <- es$vectors %*% (as.numeric(es$values) * 
                                         t(es$vectors))
    }
  }
  UZ.len <- (oo$n.Xu + M) * k
  object$UZ <- matrix(oo$UZ[1:UZ.len], oo$n.Xu + M, k)
  Xu.len <- oo$n.Xu * object$dim
  object$Xu <- matrix(oo$Xu[1:Xu.len], oo$n.Xu, object$dim)
  object$df <- object$bs.dim
  object$shift <- shift
  if (!is.null(shrink)) 
    M <- 0
  object$rank <- k - M
  object$null.space.dim <- M
  if (object$drop.null > 0) {
    ind <- 1:(k - M)
    if (FALSE) {
      np <- nat.param(object$X, object$S[[1]], rank = k - 
                        M, type = 0)
      object$P <- np$P
      object$S[[1]] <- diag(np$D)
      object$X <- np$X[, ind]
    }
    else {
      object$S[[1]] <- object$S[[1]][ind, ind]
      object$X <- object$X[, ind]
      object$cmX <- colMeans(object$X)
      object$X <- sweep(object$X, 2, object$cmX)
    }
    object$null.space.dim <- 0
    object$df <- object$df - M
    object$bs.dim <- object$bs.dim - M
    object$C <- matrix(0, 0, ncol(object$X))
  }
  class(object) <- "tprs.smooth"
  object
}  

require(mgcv); n <- 100; set.seed(2)
x <- runif(n); y <- x + x^2*.2 + rnorm(n) *.1

## is smooth significantly different from straight line?
summary(gam(y~s(x,m=c(2,0))+x,method="REML")) ## not quite

## is smooth significantly different from zero?
summary(gam(y~s(x),method="REML")) ## yes!

## Fool beam(...,discrete=TRUE) into (strange) nested
## model fit...
set.seed(2) ## simulate some data... 
dat <- gamSim(1,n=400,dist="normal",scale=2)
dat$x1a <- dat$x1 ## copy x1 so beam allows 2 copies of x1
## Following removes identifiable problem, by removing
## linear terms from second smooth, and then re-inserting
## the one that was not a duplicate (x2)...
b <- bam(y~s(x0,x1)+s(x1a,x2,m=c(2,0))+x2,data=dat,discrete=TRUE)

## example of knot based tors...
k <- 10; m <- 2
y <- y[order(x)];x <- x[order(x)]
b <- gam(y~s(x,k=k,m=m),method="REML",
         knots=list(x=seq(0,1,length=k)))
X <- model.matrix(b)
par(mfrow=c(1,2))
plot(x,X[,1],ylim=range(X),type="l")
for (i in 2:ncol(X)) lines(x,X[,i],col=i)

## compare with exigent based (default)
b1 <- gam(y~s(x,k=k,m=m),method="REML")
X1 <- model.matrix(b1)
plot(x,X1[,1],ylim=range(X1),type="l")
for (i in 2:ncol(X1)) lines(x,X1[,i],col=i)
## see ?game
