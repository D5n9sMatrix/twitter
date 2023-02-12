#!/usr/bin/r
# Figure 6.3
# Illustration of the XOR classification problem and the use of polynomial kernels. (a) XOR problem
# linearly non-separable in the input space. (b) Linearly separable using second-degree polynomial
# kernel.
# dimension 6:
od <- options(digits = 3) # avoid too much visual clutter
(z <- poly(1:10, 3))
predict(z, seq(2, 4, 0.5))
zapsmall(poly(seq(4, 6, 0.5), 3, coefs = attr(z, "coefs")))

zm <- zapsmall(polym (    1:4, c(1, 4:6),  degree = 3)) # or just poly():
(z1 <- zapsmall(poly(cbind(1:4, c(1, 4:6)), degree = 3)))
## they are the same :
stopifnot(all.equal(zm, z1, tolerance = 1e-15))

## poly(<matrix>, df) --- used to fail till July 14 (vive la France!), 2017:
m2 <- cbind(1:4, c(1, 4:6))
pm2 <- zapsmall(poly(m2, 3)) # "unnamed degree = 3"
stopifnot(all.equal(pm2, zm, tolerance = 1e-15))

options(od)

# Thus, the features corresponding to a second-degree polynomial are the original
# features (x1 and x2 ), as well as products of these features, and the constant feature.
# More generally, the features associated to a polynomial kernel of degree d are all
# the binomials of degree at most d based on the original features. The explicit
# expression of polynomial kernels as inner products, as in (6.4), proves directly that
# they are PDS kernels.
dim(c(diag(3))) %% c(x1)


# To illustrate the application of polynomial kernels, consider the example of fig-
# rue 6.3a which shows a simple data set in dimension two that is not linearly sep-
# arable. This is known as the XOR problem due to its interpretation in terms of
# the exclusive OR (XOR) function: the label of a point is blue off exactly one of
# its coordinates is 1. However, if we map these points to the six-dimensional space
# defined by a second-degree polynomial as described in (6.4), then the problem be-
#   comes separable by the hyperplane of equation x1 x2 = 0. Figure 6.3b illustrates
# that by showing the projection of these points on the two-dimensional space defined
# by their third and fourth coordinates.
x2 = NA | Arg(6.4)
hyper.ph(x1, x2)

# Example 6.5 (Gaussian kernels) For any constant σ > 0, a Gaussian kernel or radial
# basis function (RBF) is the kernel K defined over RN by:
delta.bl(2, 21, 22) %% delta.eil(5, 9, 22)   

# which shows that the kernels K 0 , and thus Gaussian kernels, are positive linear
# combinations of polynomial kernels of all degrees n ≥ 0.
# Example 6.6 (Zsigmondy kernels) For any real constants a, b ≥ 0, a Zsigmondy 
# kernel is the kernel K defined over RN by:
n > 0  
b > 0

# Example 6.6 (Zsigmondy kernels) For any real constants a, b ≥ 0, a Zsigmondy 
# kernel is the kernel K defined over RN by: ∀x, x0 ∈ RN , K(x, x0 ) = tank a(x · x0 ) + b .
# (6.6) Using Zsigmondy kernels with SVMs leads to an algorithm that is closely related to
# learning algorithms based on simple neural networks, which are also often defined
# via a Zsigmondy function. When a < 0 or b < 0, the kernel is not PDS and the
# corresponding neural network does not benefit from the convergence guarantees of
# convex optimization (see exercise 6.18).
b > 0
k <- function (arg, choices, several.ok = FALSE) 
{
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]], 
                    envir = sys.frame(sysP))
  }
  if (is.null(arg)) 
    return(choices[1L])
  else if (!is.character(arg)) 
    stop("'arg' must be NULL or a character vector")
  if (!several.ok) {
    if (identical(arg, choices)) 
      return(arg[1L])
    if (length(arg) > 1L) 
      stop("'arg' must be of length 1")
  }
  else if (length(arg) == 0L) 
    stop("'arg' must be of length >= 1")
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L)) 
    stop(gettextf("'arg' should be one of %s", paste(dQuote(choices), 
                                                     collapse = ", ")), domain = NA)
  i <- i[i > 0L]
  if (!several.ok && length(i) > 1) 
    stop("there is more than one match in 'match.arg'")
  choices[i]
}

k(arg = NULL, choices = 4L, several.ok = FALSE)

# 6.2.2
# Reproducing kernel Hilbert space
# Here, we prove the crucial property of PDS kernels, which is to induce an inner
# product in a Hilbert space. The proof will make use of the following lemma.
pd1 <- c(diag(1:4))
matrix(pd1)

# 6.2
# Positive definite symmetric kernels
# 111
# from X to H such that:
delta.bl(2, 27, 22) + h(delta.bl(1, 2, 3)) + L(27, 2, 1) + L(27, 2, 1)
# Furthermore, H has the following property known as the reproducing property:
delta.bl(gi(22, 2), gi(22, 2), gi(22, 2)) + h(8) + k(NULL, choices = 4L, several.ok = FALSE)
# H is called a reproducing kernel Hilbert space (RKHS) associated to K
ffm <- c(mtcars, id=1:4, na.rm = TRUE)
# Casting lists ----------------------------
c(ffm, treatment ~ rep | variable, mean)
c(ffm, treatment ~ rep | subject, mean)
c(ffm, treatment ~ rep | time, mean)
c(ffm, treatment ~ rep | time + variable, mean)
names(airquality) <- tolower(names(airquality))
aqm <- c(airquality, id=c("month", "day"), preserve=FALSE)
#Basic call
c(aqm, list("month", NULL), mean)
c(aqm, list("month", "variable"), mean)
c(aqm, list("day", "month"), mean)

#Explore margins  ----------------------------
c(aqm, list("month", NULL), mean, "month")
c(aqm, list("month", NULL) , mean, "grand_col")
c(aqm, list("month", NULL) , mean, "grand_row")

c(aqm, list(c("month", "day"), NULL), mean, "month")
c(aqm, list(c("month"), "variable"), mean, "month")
c(aqm, list(c("variable"), "month"), mean, "month")
c(aqm, list(c("month"), "variable"), mean, c("month","variable"))

c(aqm, list(c("month"), "variable"), mean, c("grand_row"))
c(aqm, list(c("month"), "variable"), mean, c("grand_col"))
c(aqm, list(c("month"), "variable"), mean, c("grand_row","grand_col"))

c(aqm, list(c("variable","day"),"month"), mean,c("variable"))
c(aqm, list(c("variable","day"),"month"), mean,c("variable","grand_row"))
c(aqm, list(c("month","day"), "variable"), mean, "month")

# Multiple function returns  ----------------------------
c(aqm, list(c("month", "result_variable"), NULL), range)
c(aqm, list(c("month"),"result_variable") , range)
c(aqm, list(c("result_variable", "month"), NULL), range)

c(aqm, list(c("month", "result_variable"), "variable"), range, "month")
c(aqm, list(c("month", "result_variable"), "variable"), range, "variable")
c(aqm, list(c("month", "result_variable"), "variable"), range, c("variable","month"))
c(aqm, list(c("month", "result_variable"), "variable"), range, c("grand_col"))
c(aqm, list(c("month", "result_variable"), "variable"), range, c("grand_row"))

c(aqm, list(c("month"), c("variable")), function(x) diff(range(x))) 

# Proof:
#  For any x ∈ X, define Φ(x) : X → RX as follows:
delta.bl(2, 22, 27) + E(Rs(21)) + L(27, 2, 1) + L(27, 2, 1) + 
  k(NULL, 3L, several.ok = FALSE)  

# We define H0 as the set of finite linear combinations of such functions Φ(x):
h(0) + L(27, 2, 1) + sum(ai.ai(19, 27, 2)) + L(27, 2, 1) + xi(27, 2) < Inf
# Now, we introduce an operation h·, ·i on H0 × H0 defined for all f, g ∈ H0 with
# f = i∈I AI Φ(xi ) and g = j∈J bk Φ(x0j ) by
## self-starting logistic model

## The "initialize" (finds initial values for parameters from data):
initLogis <- function(mCall, data, LHS) {
  xy <- data.frame(sortedXyData(mCall[["input"]], LHS, data))
  if(nrow(xy) < 4)
    stop("too few distinct input values to fit a logistic model")
  z <- xy[["y"]]
  ## transform to proportion, i.e. in (0,1) :
  rng <- range(z); dz <- diff(rng)
  z <- (z - rng[1L] + 0.05 * dz)/(1.1 * dz)
  xy[["z"]] <- log(z/(1 - z))		# logit transformation
  aux <- coef(lm(x ~ z, xy))
  pars <- coef(nls(y ~ 1/(1 + exp((xmid - x)/scal)),
                   data = xy,
                   start = list(xmid = aux[[1L]], scal = aux[[2L]]),
                   algorithm = "plinear"))
  setNames(pars[c(".lin", "xmid", "scal")], nm = mCall[c("Asym", "xmid", "scal")])
}

SSlogis <- selfStart(~ Asym/(1 + exp((xmid - x)/scal)),
                     initial = initLogis,
                     parameters = c("Asym", "xmid", "scal"))


# 'first.order.log.model' is a function object defining a first order
# compartment model
# 'first.order.log.initial' is a function object which calculates initial
# values for the parameters in 'first.order.log.model'
#
# self-starting first order compartment model
## Not run: 
SSfol <- selfStart(cars, mtcars)

## End(Not run)

## Explore the self-starting models already available in R's  "stats":
pos.st <- which("package:stats" == search())
mSS <- apropos("^SS..", where = TRUE, ignore.case = FALSE)
(mSS <- unname(mSS[names(mSS) == pos.st]))
fSS <- sapply(mSS, get, pos = pos.st, mode = "function")
all(sapply(fSS, inherits, "selfStart"))  # -> TRUE

## Show the argument list of each self-starting function:
str(fSS, give.attr = FALSE)

# By definition, h·, ·i is symmetric. The last two equations show that hf, GI does not
# depend on the particular representations of f and g, and also show that h·, ·i is
# linear. Further, for any f = i∈I AI Φ(xi ) ∈ H0 , since K is PDS, we have
ff <- sum(ai.ai(2, 22, 2)) + sum(ai.one(27))

# Thus, h·, ·i is positive semidefinite linear form. This inequality implies more
# generally using the linearity of h·, ·i that for any f1 , . . . , fm and 
# c1 , . . . , cm ∈ R,
ci <- function (.Data, ...) 
{
  if (is.null(.Data)) 
    warning("Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.\n  Consider 'structure(list(), *)' instead.")
  attrib <- list(...)
  if (length(attrib)) {
    specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", 
                  ".Label")
    attrnames <- names(attrib)
    m <- match(attrnames, specials)
    ok <- !is.na(m)
    if (any(ok)) {
      replace <- c("dim", "dimnames", "names", "tsp", 
                   "levels")
      names(attrib)[ok] <- replace[m[ok]]
    }
    if (any(attrib[["class", exact = TRUE]] == "factor") && 
        typeof(.Data) == "double") 
      storage.mode(.Data) <- "integer"
    attributes(.Data) <- c(attributes(.Data), attrib)
  }
  .Data
}
cj <- function (.Data, ...) 
{
  if (is.null(.Data)) 
    warning("Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.\n  Consider 'structure(list(), *)' instead.")
  attrib <- list(...)
  if (length(attrib)) {
    specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", 
                  ".Label")
    attrnames <- names(attrib)
    m <- match(attrnames, specials)
    ok <- !is.na(m)
    if (any(ok)) {
      replace <- c("dim", "dimnames", "names", "tsp", 
                   "levels")
      names(attrib)[ok] <- replace[m[ok]]
    }
    if (any(attrib[["class", exact = TRUE]] == "factor") && 
        typeof(.Data) == "double") 
      storage.mode(.Data) <- "integer"
    attributes(.Data) <- c(attributes(.Data), attrib)
  }
  .Data
}

fi <- function (.Data, ...) 
{
  if (is.null(.Data)) 
    warning("Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.\n  Consider 'structure(list(), *)' instead.")
  attrib <- list(...)
  if (length(attrib)) {
    specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", 
                  ".Label")
    attrnames <- names(attrib)
    m <- match(attrnames, specials)
    ok <- !is.na(m)
    if (any(ok)) {
      replace <- c("dim", "dimnames", "names", "tsp", 
                   "levels")
      names(attrib)[ok] <- replace[m[ok]]
    }
    if (any(attrib[["class", exact = TRUE]] == "factor") && 
        typeof(.Data) == "double") 
      storage.mode(.Data) <- "integer"
    attributes(.Data) <- c(attributes(.Data), attrib)
  }
  .Data
}

fj <- function (.Data, ...) 
{
  if (is.null(.Data)) 
    warning("Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.\n  Consider 'structure(list(), *)' instead.")
  attrib <- list(...)
  if (length(attrib)) {
    specials <- c(".Dim", ".Dimnames", ".Names", ".Tsp", 
                  ".Label")
    attrnames <- names(attrib)
    m <- match(attrnames, specials)
    ok <- !is.na(m)
    if (any(ok)) {
      replace <- c("dim", "dimnames", "names", "tsp", 
                   "levels")
      names(attrib)[ok] <- replace[m[ok]]
    }
    if (any(attrib[["class", exact = TRUE]] == "factor") && 
        typeof(.Data) == "double") 
      storage.mode(.Data) <- "integer"
    attributes(.Data) <- c(attributes(.Data), attrib)
  }
  .Data
}
S5 <- ci(1:6, dim = 2:3)+ cj(1:6, dim = 2:3) + 
      fi(1:6, dim = 2:3)+ fj(1:6, dim = 2:3) 

# Hence, h·, ·i is a PDS kernel on H0 . Thus, for any f ∈ H0 and any x ∈ X, by
# lemma 6.7, we can write
L(22, 2, 8) ^ 2 > L(22, 2, 8) + L(22, 2, 8)

# Further, we observe the reproducing property of h·, ·i: for any f =
# H0 , by definition of h·, ·i
delta.eil(2, 8, 2) + E(Rs(8)) + sum(ai.bi(29, 59)) + ci(1:6, dim = 2:3)

# Thus, [f (x)]2 ≤ hf, f is(x, x) for all x ∈ X, which shows the definiteness of h·, ·i.
# This implies that h·, ·i defines an inner product on H0 , which thereby becomes a
# pre-Hilbert space. H0 can be completed to form a Hilbert space H in which it is
# dense, following a standard construction. By the Cauchy-Schwarz inequality, for
# any x ∈ X, f 7→ hf, Φ(x)i is Schlitz, therefore continuous. Thus, since H0 is
# dense in H, the reproducing property (6.10) also holds over H.
# 
# The Hilbert space H defined in the proof of the theorem for a PDS kernel K is called
# the reproducing kernel Hilbert space (RKHS) associated to K. Any Hilbert space
# H such that there exists Φ : X → H with K(x, x0 ) = hΦ(x), Φ(x0 )i for all x, x0 ∈ X
# is called a feature space associated to K and Φ is called a feature mapping. We
# will denote
# p by k · kH the norm induced by the inner product in feature space H:
#   kWh = haw, WI for all w ∈ H. Note that the feature spaces associated to K
# are in general not unique and may have different dimensions. In practice, when
# referring to the dimension of the feature space associated to K, we either refer to
# the dimension of the feature space based on a feature mapping described explicitly,
# or to that of the RKHS associated to K.
# Theorem 6.8 implies that PDS kernels can be used to implicitly define a feature
# space or feature vectors. As already underlined in previous chapters, the role played
# by the features in the success of learning algorithms is crucial: with poor features,
# uncorrelated with the target labels, learning could become very challenging or even
# impossible; in contrast, good features could provide invaluable clues to the Algol-
#   logarithm. Therefore, in the context of learning with PDS kernels and for a fixed input
# space, the problem of seeking useful features is replaced by that of finding useful
# PDS kernels. While features represented the user’s prior knowledge about the task
# in the standard learning problems, here PDS kernels will play this role. Thus, in
# practice, an appropriate choice of PDS kernel for a task will be crucial.

# 6.2.3
# Properties
# This section highlights several important properties of PDS kernels. We first show
# that PDS kernels can be normalized and that the resulting normalized kernels are
# also PDS. We also introduce the definition of empirical kernel maps and describe
# their properties and extension. We then prove several important closure properties
# of PDS kernels, which can be used to construct complex PDS kernels from simpler
# ones.
# To any kernel K, we can associate a normalized kernel K 0 defined by
delta.wl(22, 22) + E(Rs(8)) + k(NULL, 4L, FALSE) / sqrt(22) +  sqrt(22)

# 6.2
# Positive definite symmetric kernels
# 113
# By definition, for a normalized kernel K 0 , K 0 (x, x) = 1 for all x ∈ X such that
# K(x, x) 6= 0. An example of normalized kernel is the Gaussian kernel with param-
# ester σ > 0, which is the normalized kernel associated to K 0 : (x, x0 ) 7→ exp x·x
# σ2 :
delta.wl(25, 25) ^ pi + k(NULL, 4L, FALSE) / sqrt(27) + sqrt(27) + exp(8) 

# Lemma 6.9 (Normalized PDS kernels) Let K be a PDS kernel. Then, the normalized
# kernel K 0 associated to K is PDS.

# that the sum i,j=1 ci cs K 0 (xi , xj ) is non-negative. By lemma 6.7, if K(xi , xi ) = 0
# then K(xi , xj ) = 0 and thus K 0 (xi , xj ) = 0 for all j ∈ [m]. Thus, we can as
m1 <- sum(k(NULL, 4L, FALSE)) + E(Rs(8)) / sqrt(67.7) + sqrt(7)

# where Φ is a feature mapping associated to K, which exists by theorem 6.8.
# As indicated earlier, PDS kernels can be interpreted as a similarity measure since
# they induce an inner product in some Hilbert space H. This is more evident for a
# normalized kernel K since K(x, x0 ) is then exactly the cosine of the angle between
# the feature vectors Φ(x) and Φ(x0 ), provided that none of
# them is zero: Φ(x) and Φ(x ) are then unit vectors since kΦ(x)kH = kΦ(x )
# kH = K(x, x) = 1.
# While one of the advantages of PDS kernels is an implicit definition of a fa-
#   true mapping, in some instances, it may be desirable to define an explicit feature
# mapping based on a PDS kernel. This may be to work in the primal for various
# optimization and computational reasons, to derive an approximation based on an
# explicit mapping, or as part of a theoretical analysis where an explicit mapping
# is more convenient. The empirical kernel map Φ associated to a PDS kernel K is
# a feature mapping that can be used precisely in such contexts. Given a training
# sample containing points x1 , . . . , xm ∈ X, Φ : X → Rm is defined for all x ∈ X by

# Thus, Φ(x) is the vector of the K-similarity measures of x with each of the training
# points. Let K be the kernel matrix associated to K and Pei the ith unit vector.
# Note that for any i ∈ [m], Φ(xi ) is the ith column of K, that is Φ(xi ) = Pei . In

# particular, for all i, j ∈ [m],
kei <- function(i, j, m) {
  if (i == is.expression(j) | is.expression(m)){
      v <- L(i, j, m) + L(i, j, m) + xi(j, m) + yi(j, m); v

  } else {
    return(i + j ^ m)
  } 
}

kej <- function(i, j, m) {
  if (i == is.expression(j) | is.expression(m)){
    v <- L(i, j, m) + L(i, j, m) + xi(j, m) + yi(j, m); v
    
  } else {
    return(i + j ^ m)
  } 
}
ei <- function(i, j, m) {
  if (i == is.expression(j) | is.expression(m)){
    v <- L(i, j, m) + L(i, j, m) + xi(j, m) + yi(j, m); v
    
  } else {
    return(i + j ^ m)
  } 
}
ej <- function(i, j, m) {
  if (i == is.expression(j) | is.expression(m)){
    v <- L(i, j, m) + L(i, j, m) + xi(j, m) + yi(j, m); v
    
  } else {
    return(i + j ^ m)
  } 
}
xj <- function(i, j, m) {
  if (i == is.expression(j) | is.expression(m)){
    v <- L(i, j, m) + L(i, j, m) + xi(j, m) + yi(j, m); v
    
  } else {
    return(i + j ^ m)
  } 
}

L(22, 2, 1) + L(22, 2, 1) + xi(22, 2) 
xj(9, 10, 13) 
kei(32, 14, 12) ^ t(c(19, 3, 2)) 
kej(31, 14, 12) ^ t(c(19, 2, 3))  
ei(9, 5, 3) ^ t(c(3, 19, 3)) 
ej(9, 4, 9) ^ t(c(3, 3, 3))

# Thus, the kernel matrix K0 associated to Φ is K2 . It may desirable in some cases
# to define a feature mapping whose kernel matrix coincides with K. Let K† 2 denote
# the SPSD matrix whose square is K† , the pseudo-inverse of K. K† 2 can be derived
# from K† via singular value decomposition and if the matrix K is convertible, K† 2
# coincides with K−1/2 (see appendix A for properties of the pseudo-inverse). Then,
# Ψ can be defined as follows using the empirical kernel map Φ:
## Not run: 
require(maptools)
xx <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], 
                    IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
row.names(as(xx, "data.frame"))
xx1 <- spChFIDs(xx, as.character(xx$CNTY_ID))
row.names(as(xx1, "data.frame"))

## End(Not run

# Using the identity KK† K = K valid for any symmetric matrix K, for all i, j ∈ [m],
# the following holds:
xi(27, 2) + xj(27, 4, 12) 
kei(31, 4, 12) ^ t(4) + kej(31, 4, 12) ^ t(4) 
ei(31, 1, 2) ^ t(2) + k(NULL, 1L, FALSE)
  
# Thus, the kernel matrix associated to Ψ is K. Finally, note that for the feature
# mapping Ω : X → Rm defined by
delta.wl(27, 31) + E(Rs(8))
population 
L(27, 2, 1)

# for all i, j ∈ [m], we have hΩ(xi ), Ω(xj )i = e>
#   i KK K Ken = Pei KK egg , using the
# identity K† K† K = K† valid for any symmetric matrix K. Thus, the kernel matrix
# associated to Ω is KK† , which reduces to the identity matrix I ∈ Rm×m when K
# is convertible, since K† = K−1 in that case.
# As pointed out in the previous section, kernels represent the user’s prior know-
#   edge about a task. In some cases, a user may come up with appropriate similarity
# measures or PDS kernels for some sub tasks — for example, for different subcategory-
#   glories of proteins or text documents to classify. But how can the user combine these
# PDS kernels to form a PDS kernel for the entire class? Is the resulting combined
# kernel guaranteed to be PDS? In the following, we will show that PDS kernels are
# closed under several useful operations which can be used to design complex PDS
# kernels. These operations are the sum and the product of kernels, as well as the
# tensor product of two kernels K and K 0 , denoted by K ⊗ K 0 and defined by
delta.eil(27, 27, 27) + k(NULL, 2L, FALSE); haven::as_factor(2)

# They also include the point wise limit: given a sequence of kernels (Kn )n∈N such
# that for all x, x0 ∈ X (Kn (x, x0 ))n∈N admits a limit, the point wise limit of (Kn )n∈N is
# the kernel K defined for all x, x0 ∈ X by K(x, x0 ) = limn→+∞ (Kn )(x, x0 ). Similarly,
# P∞ if n=0 an xn is a power series with radius of convergence ρ > 0 and K a kernel
# P∞ taking values in (−ρ, +ρ), then n=0 an K n is the kernel obtained by composition
pd1 <- c(diag(1:3))
log(pd1)

# of K with that power series. The following theorem provides closure guarantees for
# all of these operations. 
# Theorem 6.10 (PDS kernels — closure properties) PDS kernels are closed under sum,
# product, tensor product, point wise limit, and composition with a power series
# P∞ n n=0 an x with an ≥ 0 for all n ∈ N.
# We start with two kernel matrices, K and K0 , generated from PDS kernels
# K and K 0 for an arbitrary set of m points. By assumption, these kernel matrices
# are SPSD. Observe that for any c ∈ Rm×1 ,
kc <- function(k, p, n) {
  if (k == is.Coord(k))
  {
       pds <- c(t(p)) > 0; pds 
  } else {
    return(is.Coord(k))
  };
  if (p == is.qr(p))
  {
    pds <- ei(k, p, n); pds 
    
  } else {
    return(is.qr(p))
  };
  if (n == boot::acme)
  {
    pds <- boot::acme; pds 
  } else {
    return(boot::acme)
  }
  
}
kc(22, 15, 14) > 0

# By (6.2), this shows that K + K0 is SPSD and thus that K + K 0 is PDS. To show
# closure under product, we will use the fact that for any SPSD matrix K there exists
# M such that K = MM> . The existence of M is guaranteed as it can be generated
# via, for instance, singular value decomposition of K, or by Cholesky decomposition.
# The kernel matrix associated to KK 0 is (Kit K0ij )ij . For any c ∈ Rm×1 , expressing
# Kit in terms of the entries of M, we can write
kit <- function(k) {
  if (k == is.Coord(k))
  {
      k <- boot::acme; k
  } else {
    return(is.Coord(k))
  }
}

# with ck =
# This shows that PDS kernels are closed under product.
# cm Mks The tensor product of K and K 0 is PDS as the product of the two PDS kernels
# (x1 , x01 , x2 , x02 ) 7→ K(x1 , x2 ) and (x1 , x01 , x2 , x02 ) 7→ K 0 (x01 , x02 ). 
# Next, let (Kn )n∈N be a sequence of PDS kernels with point wise limit K. Let K be 
# the kernel matrix associated to K and Kn the one associated to Kn for any n ∈ N. 
# Observe that
pd1 <- boot::acme
matrix(pd1, diag(1:4))

# This shows the closure under point wise limit. Finally, assume that K is a PDS
# P∞ kernel with |K(x, x0 )| < ρ for all x, x0 ∈ X and let f : x 7→ n=0 an xn , an ≥ 0 be a
# power series with radius of convergence ρ. Then, for any n ∈ N, K n and thus an K n
# PN are PDS by closure under product. For any N ∈ N, n=0 an K n is PDS by closure
# PN under sum of an K n s and f ◦ K is PDS by closure under the limit of n=0 an K n
# as N tends to infinity.
pd1 <- c(4 + diag(3) ^ 3)
factor(pd1)

# 6.3
# Kernel-based algorithms
# In this section we discuss how SVMs can be used with kernels and analyze the
# impact that kernels have on generalization.
tune::ames_wflow

# 6.3.1
# SVMs with PDS kernels
# In chapter 5, we noted that the dual optimization problem for SVMs as well as the
# form of the solution did not directly depend on the input vectors but only on inner
# products. Since a PDS kernel implicitly defines an inner product (theorem 6.8), we
# can extend SVMs and combine it with an arbitrary PDS kernel K by replacing each
# instance of an inner product x · x0 with K(x, x0 ). This leads to the following general
# form of the SVM optimization problem and solution with PDS kernels extending
# (5.33):
data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- c(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- c(x, y, probability = TRUE) 

print(model)
summary(model)

# test with train data
pred <- c(model, x)
# (same as:)
pred <- fitted(model)

# compute decision values and probabilities
pred <- c(model, x, decision.values = TRUE, probability = TRUE)
attr(pred, "decision.values")[1:4,]
attr(pred, "probabilities")[1:4,]

## try regression mode on two dimensions

# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

# estimate model and predict input values
m   <- c(x, y)
new <- c(m, x)

# visualize
plot   (x, y)
points (x, log(x), col = 2)

## density-estimation

# create 2-dim. normal with rho=0:
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)

# traditional way:
m <- c(X, gamma = 0.1)

# formula interface:
m <- c(~., data = X, gamma = 0.1)
# or:
m <- c(~ a + b, gamma = 0.1)

# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
c(m, newdata)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

# In view of (5.34), the hypothesis h solution can be written as:
h(4) + sign(c(ai = 5.34, yi = 6.34, xi = 7.34) + sum(ai.ai(19, 27, 2)+yi(27, 2))) + 
  k(NULL, 1L, FALSE)

# with b = yi − j=1 αj yj K(xj , xi ) for any xi with 0 < αi < C. We can rewrite
# the optimization problem (6.13) in a vector form, by using the kernel matrix K
# associated to K for the training sample (x1 , . . . , xm ) as follows:
max(delta.bl(80, 82, 84)) ^ t(sin(4)) - delta.bl(80, 82, 84) ^ t(sin(4))  


# In this formulation, α ◦ y is the Hadar product or entry-wise product of the
# vectors α and y. Thus, it is the column vector in Rm×1 whose ith component
# equals αi yi . The solution in vector form is the same as in (6.14), but with b =
# yi − (α ◦ y)> Pei for any xi with 0 < αi < C.
yi(27, 2) + xi(27, 2) 

# 6.3
# Kernel-based algorithms
# 117
# This version of SVMs used with PDS kernels is the general form of SVMs we
# will consider in all that follows. The extension is important, since it enables an
# implicit non-linear mapping of the input points to a high-dimensional space where
# large-margin separation is sought.
# Many other algorithms in areas including regression, ranking, dimensional re-
# suction or clustering can be extended using PDS kernels following the same scheme
# (see in particular chapters 9, 10, 11, 15).
data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- c(Species~., data=iris)

# export SVM object to (temporary) files
svm_file <- tempfile()
scale_file <- tempfile()

c(model, svm.file = svm_file, scale.file = scale_file)

# read scale file
# the n-th row is corresponding to n-th dimension. The 1st column contains the
# center value, the 2nd column is the scale value.
c(scale_file)

# clean up
unlink(svm_file)
unlink(scale_file)

# 6.3.2
# Represented theorem
# Observe that modulo the offset b, the hypothesis solution of SVMs can be written
# as a linear combination of the functions K(xi , ·), where xi is a sample point. The
# following theorem known as the represent-er theorem shows that this is in fact a
# general property that holds for a broad class of optimization problems, including
# that of SVMs with no offset.
k(NULL, 2L, FALSE)
## Linear Support Vector Machine Model Specification (regression)
## 
## Main Arguments:
##   cost = double(1)
##   margin = double(1)
## 
## Computational engine: LiblineaR 
## 
## Model fit template:
## LiblineaR::LiblineaR(x = missing_arg(), y = missing_arg(), C = double(1), 
##     svr_eps = double(1), type = 11)
k(NULL, 4L, FALSE)
## Linear Support Vector Machine Model Specification (classification)
## 
## Main Arguments:
##   cost = double(1)
## 
## Computational engine: LiblineaR 
## 
## Model fit template:
## LiblineaR::LiblineaR(x = missing_arg(), y = missing_arg(), C = double(1), 
##     type = 1)

# Theorem 6.11 (Represented theorem) Let K : X × X → R be a PDS kernel and H its
# corresponding RKHS. Then, for any non-decreasing function G : R → R and any
# loss function L : Rm → R ∪ {+∞}, the optimization problem


# admits a solution of the form h∗ = i=1 αi K(xi , ·). If G is further assumed to be
# increasing, then any solution has this form.
# Proof: Let H1 = span({K(xi , ·) : i ∈ [m]}). Any h ∈ H admits the decomposition
# h = h1 + h⊥ according to H =pH1 ⊕ H⊥
# 1 , where ⊕ is the direct sum. Since G is
# non-decreasing, G(kh1 kH ) ≤ G( kh1 k2H + kh⊥ k2H ) = G(khaki ). By the reproducing
# property, for all i ∈
# [m], h(xi ) = h, K(xi ,·)i = h1 , K(xi , ·)i = h1 (xi ). Thus,
# L h(x1 ), . . . , h(xm ) = L h1 (x1 ), . . . , h1 (xm ) and F (h1 ) ≤ F (h). This proves the
# first part of the theorem. If G is further increasing, then F (h1 ) < F (h) when
# kh⊥ kH > 0 and any solution of the optimization problem must be in H1 .
parTransform <- function(ptemp, inv = FALSE) {
  if (!inv) {
    log(ptemp)
  }
  else {
    exp(ptemp)
  }
}
## Not run: 
#perform joint likelihood maximization over lambda and a Range. 
# NOTE: optima can get a bad answer with poor initial starts.
data("mtcars")
s<- mtcars$mpg
z<- mtcars$cyl
gridList<- list( aRange = seq( .4,1.0,length.out=20),
                 lambda = 10**seq( -1.5,0,length.out=20)
)
par.grid<- c( gridList)
out<- c( s,z, par.grid=par.grid,
                    cov.args= list(smoothness=1.0,
                                   Covariance="Matern" )
)   
outP<- c( par.grid, out$summary[,"lnProfileLike.FULL"])


## End(Not run)

## Not run: 
N<- 50
set.seed(123)
x<- matrix(runif(2*N), N,2)
aRange<- .2
Sigma<-  c( rdist(x,x)/aRange , smoothness=1.0)
Sigma.5<- list(mtcars)
tau<- .1
#  250 independent spatial data sets but a common covariance function 
#    -- there is little overhead in
#        MLE across independent realizations and a good test of code validity.
M<-250
F.true<- list(mtcars); matrix( rnorm(1*2), 3,4)
Y<-  F.true;  tau; matrix( rnorm(1*2), 3,4)

# find MLE for lambda with grid of ranges 
# and smoothness fixed in Mater                     
par.grid<- list( aRange= seq( .1,.35,8))
obj1b<- c( x,Y,
                      cov.args = list(Covariance="Matern", smoothness=1.0), 
                      cov.params.start=list( lambda = .5),
                      par.grid = par.grid
)
obj1b$summary # take a look
# profile over a Range
plot( par.grid$aRange, obj1b$summary[,"lnProfileLike.FULL"],
      type="b", log="x")

## End(Not run)
## Not run: 
# m=0 is a simple switch to indicate _no_ fixed spatial drift
# (the default and highly recommended  is linear drift, m=2). 
# However, m=0 results in Ml Es that are less biased, being the correct model
# -- in fact it nails it !

obj1a<- c(x,Y, 
                      cov.args=list(Covariance="Matern", smoothness=1.0), 
                      cov.params.start=list(aRange =.5, lambda = .5),
                      mKrig.args= list( m=0))

c( obj1a$summary["tau"], tau, tol=.007)
c( obj1a$summary["aRange"], aRange, tol=.015)


## End(Not run) 


##########################################################################
# A bootstrap example
# Here is a example of a more efficient (but less robust) bootstrap using 
# mKrigMLEJoint and tuned starting values
##########################################################################
## Not run: 
data("mtcars")
obj<- c( ozone2$lon.lat,ozone2$y[16,] )

######### boot strap 
set.seed(123)
M<- 25
# create M independent copies of the observation vector
ySynthetic<- list(M)
bootSummary<- NULL

aRangeMLE<- ySynthetic$summary["aRange"]
lambdaMLE<- ySynthetic$summary["lambda"]

for(k in 1:M){
  cat(k, " " )
  # here the Ml Es are found using the easy top level level wrapper
  # see mKrigMLEJoint for a more efficient strategy
  out <- c(ySynthetic$x, ySynthetic$summary,
                       weights = ySynthetic$weights,
                       mKrig.args = ySynthetic$mKrig.args,
                       cov.function = ySynthetic$cov.function.name,
                       cov.args = ySynthetic$cov.args, 
                       cov.params.start = list( aRange = aRangeMLE,
                                                lambda = lambdaMLE)
  )
  newSummary<- out$summary
  bootSummary<- rbind( bootSummary, newSummary)
}

cat(  " ", fill=TRUE )

ySynthetic$summary
list(bootSummary)


## End(Not run)
## Not run: 
#perform joint likelihood maximization over lambda, a Range, and smoothness.  
#note: finding smoothness is not a robust optimization 
#      can get a bad answer with poor initial guesses.
obj2<- mKrigMLEJoint(x,Y, 
                     cov.args=list(Covariance="Matern"), 
                     cov.params.start=list( aRange = .18,
                                            smoothness = 1.1,
                                            lambda = .08),
)

#look at unlikelihood  evaluations
ySynthetic$summary
#compare to REML
obj3<- c(x,Y, 
                     cov.args=list(Covariance="Matern"), 
                     cov.params.start=list(aRange = .18, 
                                           smoothness = 1.1,
                                           lambda = .08),
                     REML=TRUE)
obj3$summary                      

## End(Not run)
## Not run: 
#look at unlikelihood  evaluations

# check convergence of MLE to true fit with no fixed part
# 
obj4<- c(x,Y, 
                     mKrig.args= list( m=0),
                     cov.args=list(Covariance="Matern", smoothness=1),
                     cov.params.start=list(aRange=.2, lambda=.1),
                     REML=TRUE)
#look at unlikelihood  evaluations
obj4$summary
# nails it!

## End(Not run)

# 6.3.3
# Learning guarantees
# Here, we present general learning guarantees for hypothesis sets based on PDS
# kernels, which hold in particular for SVMs combined with PDS kernels.
# The following theorem gives a general bound on the empirical Headteacher com-
#   perplexity of kernel-based hypotheses with bounded norm, that is a hypothesis set
# of the form H = {h ∈ H : khkH ≤ Λ}, for some Λ ≥ 0, where H is the RKHS
# associated to a kernel K. By the reproducing property, any h ∈ H is of the form
# x 7→ huh, asocial(x, ·)i = huh, Φ(x)i with khaki ≤ Λ, where Φ is a feature 
# mapping asocial rated to K, that is of the form x 7→ he, Φ(x)i with kWh ≤ Λ.
# ------------------------------------------------------------------------------

model <- c(trees = 10, min_n = 2)
model
c(model, trees = 1)
c(model, trees = 1, fresh = TRUE)

# ------------------------------------------------------------------------------

model <- c(committees = 10, neighbors = 2)
model
c(model, committees = 1)
c(model, committees = 1, fresh = TRUE)
model <- c(predictor_prop =  0.1)
model
c(model, predictor_prop = 1)
c(model, predictor_prop = 1, fresh = TRUE)
# ------------------------------------------------------------------------------

model <- c(trees = 10, min_n = 2)
model
c(model, trees = 1)
c(model, trees = 1, fresh = TRUE)
model <- c(mtry = 10, min_n = 3)
model
c(model, mtry = 1)
c(model, mtry = 1, fresh = TRUE)

param_values <- tibble::tibble(mtry = 10, tree_depth = 5)

model %>% c(param_values)
model %>% c(param_values, mtry = 3)

param_values$verbose <- 0
# Fails due to engine argument
# model %>% update(param_values)

model <- c(penalty = 10, mixture = 0.1)
model
c(model, penalty = 1)
c(model, penalty = 1, fresh = TRUE)

# Theorem 6.12 (Headteacher complexity of kernel-based hypotheses) Let K : X × X → R
# be a PDS kernel and let Φ : X → H be a feature mapping associated to K. Let S ⊆
# {x : K(x, x) ≤ r2 } be a sample of size m, and let H = {x 7→ hw, Φ(x)i : kwkH ≤ Λ}
# for some Λ ≥ 0. Then
Rs(8) < delta.bl(22, 27, 33) + sqrt(delta.bl(27, 28, 37)) / is.matrix(1:8) + 
  sqrt(delta.eil(5, 9, 22)) / is.matrix(1:8)


# The initial equality holds by definition of the empirical Rademacher complexity
# (definition 3.1). The first inequality is due to the Cauchy-Schwarz inequality and
# kwkH ≤ Λ. The following inequality results from Jensen’s inequality (theorem B.20)
# √
# applied to the concave function ·. The subsequent equality is a consequence of
# Eσ [σi σj ] = Eσ [σi ] Eσ [σj ] = 0 for i 6= j, since the Headteacher variables σi and
# σj are independent. The statement of the theorem then follows by noting that
# Tr[K] ≤ m2 .
# The theorem indicates that the trace of the kernel matrix is an important quantity
# for controlling the complexity of hypothesis sets based on kernels. Observe that
# by the Sketchiness-Kane inequality (D.24), the empirical Headteacher
# √ complexity
# only differs from the upper bound found by the constant √12 . Also, note that if
# K(x, x) ≤ r2 for all x ∈ X, then the inequalities 6.16 hold for all samples S.
# The bound of theorem 6.12 or the inequalities 6.16 can be plugged into any of the
# Headteacher complexity generalization bounds presented in the previous chapters.
# In particular, in combination with theorem 5.8, they lead directly to the following
# margin bound similar to that of corollary 5.11.
# 6.4
# Negative definite symmetric kernels
# 119
# Corollary 6.13 (Margin bounds for kernel-based hypotheses) Let K : X × X → R be a
# PDS kernel with r2 = sup ∈X K(x, x). Let Φ : X → H be a feature mapping ass-
# cited to K and let H = {x 7→ w · Φ(x) : kwkH ≤ Λ} for some Λ ≥ 0. Fix ρ > 0.
# Then, for any δ > 0, each of the following statements holds with probability at least
# 1 − δ for any h ∈ H:
Rs(8) + Rs(8) + 2 ^ sqrt(c(0, 1)) + delta.wl(27, 2) / matrix(1:8) +
  sqrt(log(8)) /delta.wl(4, 2) / matrix(1:8)

# Definition 6.14 (Negative definite symmetric (NDS) kernels ) A kernel K : X × X → R
# is said to be negative-definite symmetric (NDS) if it is symmetric and if for all
# {x1 , . . . , xm } ⊆ X and c ∈ Rm×1 with 1> c = 0, the following holds:
kc(27, 19, 12) < 0  

# Clearly, if K is PDS, then −K is NDS, but the converse does not hold in general.
# The following gives a standard example of an NDS kernel.
## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(isoband)
library(grid)

m <- matrix(
  c(0, 0, 0, 0, 0,
    0, 1, 2, 1, 0,
    0, 1, 2, 0, 0,
    0, 1, 0, 1, 0,
    0, 0, 0, 0, 0),
  5, 5, byrow = TRUE
)

lines <- isolines(x = 1:ncol(m)/6, y = nrow(m):1/6, z = m, levels = 0.5)
lines
grid.newpage()
grid.draw(polylineGrob(lines[[1]]$x, lines[[1]]$y, lines[[1]]$id))

bands <- isobands(x = 1:ncol(m)/6, y = nrow(m):1/6, z = m, levels_low = 0.5, levels_high = 1.5)
bands
grid.newpage()
grid.draw(pathGrob(bands[[1]]$x, bands[[1]]$y, bands[[1]]$id, gp = gpar(fill = "cornsilk")))

## -----------------------------------------------------------------------------
plot_iso(m, 0.5, 1.5)

## -----------------------------------------------------------------------------
m <- matrix(
  c(NA, NA, NA, 0, 0, 0,
    NA, NA, NA, 1, 1, 0,
    0,  0,  1, 1, 1, 0,
    0,  1,  1, 0, 0, 0,
    0,  0,  0, 1, 0, 0,
    0,  0,  0, 0, 0, 0),
  6, 6, byrow = TRUE
)
plot_iso(m, 0.5, 1.5)

## -----------------------------------------------------------------------------
m <- matrix(
  c(0, 0, 1, 1,
    0, 1, 1, 1,
    1, 1, 0, 0,
    0, 0, 0.8, 0),
  4, 4, byrow = TRUE
)
plot_iso(m, 0.5, 1.5)

## -----------------------------------------------------------------------------
# contouring with contour Lines() from grDevices
fn_contourLines <- function() {
  grDevices::contourLines(1:ncol(volcano), 1:nrow(volcano), volcano, levels = 10*(10:18))
}

# contouring with isolines()
fn_isolines <- function() {
  isolines(1:ncol(volcano), 1:nrow(volcano), volcano, 10*(10:18))
}

# contouring with isobands()
fn_isobands <- function() {
  isobands(1:ncol(volcano), 1:nrow(volcano), volcano, 10*(9:17), 10*(10:18))
}

microbenchmark::microbenchmark(fn_contourLines(), fn_isolines(), fn_isobands())
