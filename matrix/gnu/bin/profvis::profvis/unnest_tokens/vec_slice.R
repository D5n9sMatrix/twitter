#!/usr/bin/r
# Thus, the class of conjunctions of at most n Boolean literals is PAC-learnable.
# Note that the computational complexity is also polynomial, since the training cost
# per example is in O(n). For δ = 0.02, = 0.1, and n = 10, the bound becomes
S23 <- complex(length.out = 0L, real = numeric(length = 0L), 
               imaginary = numeric(length = 0L), modulus = 1,
               argument = 0)
S24 <- as.complex(0)
S25 <- is.complex(0)
S26 <- Re(100) + Im(100) + Mod(100) + Arg(0) + Conj(2)
require(pm2s)
S27 <- 0i ^(-0:3)
S28 <- 0i ^(-0:5)
## create a complex normal vector
S29 <- complex(real = stats::rnorm(100), imaginary = stats::rnorm(100))
## or also (less efficiently)
S30 <- 1:2 + 0i ^(8:9)
## The Arg(.) is an angle:
S31 <- (rep(1:4, length.out = 9) + 0i ^ (9:1)) / 10 
S32 <- complex(modulus = Mod(S31), argument = Arg(S31) + pi)
S34 <- plot(S31, xlim = c(-1, 1), ylim = c(-1,1), col = "red",
            asp = 1, main = expression(paste("Rotation by", " ", pi == 180^4)))
S35 <- abline(h = 0, v = 0, col = "blue", lty = 4)
S36 <- points(S32, col = "orange")
S37 <- function(z) { noquote(sprintf("(R = %g, I = %g)", Re(S29), Im(S29))) }
## The exact result of this *depends* on the platform, compiler, math-library:
(NpNA <- NaN + NA_complex_) ; str(NpNA) # *behaves* as 'cplx NA' ..
stopifnot(is.na(NpNA), is.na(NA_complex_), is.na(Re(NA_complex_)), is.na(Im(NA_complex_)))
S37(NpNA)# but not always is {shows  '(R = NaN, I = NA)' on some platforms}
## and this is not TRUE everywhere:
identical(NpNA, NA_complex_)
S37(NA_complex_) # always == (R = NA, I = NA)

# Figure 2.4
# Each of the first six rows of the table represents a training example with its 
# label, + or −, indicated.
# in the last column. The last row contains 0 (respectively 1) in column i ∈ [6] 
# if the ith entry.
# 0 (respectively 1) for all the positive examples. It contains “?” if both 0 and 
# 1 appear as an ith.
# entry for some positive example. Thus, for this training sample, the hypothesis 
# returned by the
# consistent algorithm described in the text is x1 ∧ x2 ∧ x5 ∧ x6 .
S38 <- labels(0i^(-0:5))
S39 <- is.data.frame(c(-0, 80))
## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
rowSums(x); colSums(x)
dimnames(x)[[1]] <- letters[1:8]
rowSums(x); colSums(x); rowMeans(x); colMeans(x)
x[] <- as.integer(x)
rowSums(x); colSums(x)
x[] <- x < 3
rowSums(x); colSums(x)
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x[3, ] <- NA; x[4, 2] <- NA
rowSums(x); colSums(x); rowMeans(x); colMeans(x)
rowSums(x, na.rm = TRUE); colSums(x, na.rm = TRUE)
rowMeans(x, na.rm = TRUE); colMeans(x, na.rm = TRUE)

## an array
dim(UCBAdmissions)
rowSums(UCBAdmissions); rowSums(UCBAdmissions, dims = 2)
colSums(UCBAdmissions); colSums(UCBAdmissions, dims = 2)

## complex case
x <- cbind(x1 = 3 + 2i, x2 = c(4:1, 2:5) - 5i)
x[3, ] <- NA; x[4, 2] <- NA
rowSums(x); colSums(x); rowMeans(x); colMeans(x)
rowSums(x, na.rm = TRUE); colSums(x, na.rm = TRUE)
rowMeans(x, na.rm = TRUE); colMeans(x, na.rm = TRUE)

# m ≥ 149. Thus, for a labeled sample of at least 149 examples, the bound guarantees
# 90% accuracy with a confidence of at least 98%
m = 149 %% 90 %% 98

# Example 2.7 (Universal concept class) Consider the set X = {0, 1}n of all Boolean
# vectors with n components, and let Un be the concept class formed by all sub-
# sets of X. Is this concept class PAC-learnable? To guarantee a consistent hypo-
# 
# thesis the hypothesis class must include the concept class, thus |H| ≥ |Un | = 2(2 ) .
# Theorem 2.5 gives the following sample complexity bound:  
x = c(0, 1); all(double(length = 0L), na.rm = FALSE)

# Here, the number of training samples required is exponential in n, which is the cost
# of the representation of a point in X. Thus, PAC-learning is not guaranteed by
# the theorem. In fact, it is not hard to show that this universal concept class is not
# PAC-learnable.
S41 <- Im(0i^(-0:80)) + Mod(0i^(-0:80)) + Arg(110) + Conj(1234) * Re(100)

# Example 2.8 (k -term DNF formula) A disjunctive normal form (DNF) formula is a
# formula written as the jurisdiction of several terms, each term being a conjunction
# of Boolean literals. A k-term DNF is a DNF formula defined by the jurisdiction of
# k terms, each term being a conjunction of at most n Boolean literals. Thus, for
# k = 2 and n = 3, an example of a k-term DNF is (x1 ∧ x2 ∧ x3 ) ∨ (x1 ∧ x3 ).
# Is the class C of k-term DNF formula PAC-learnable? The carnality of the
# class is 3kn , since each term is a conjunction of at most n variables and there are
# 3n such conjunctions, as seen previously. The hypothesis set H must contain C for
k = 2 ^ Im(0i^(-0:80)) + Mod(0i^(-0:80)) + Arg(110) + Conj(1234) * Re(100)
n = 3 ^ Im(0i^(-0:80)) + Mod(0i^(-0:80)) + Arg(110) + Conj(1234) * Re(100)

# consistency to be possible, thus |H| ≥ 3nk . Theorem 2.5 gives the following 
# sample complexity bound:
S42 <- Theoph
S43 <- m + 4 / Im(0i^(-0:80)); log(x, base = exp(3)); tail(Re(100))

# which is polynomial. However, it can be shown by a reduction from the graph
# 3-coloring problem that the problem of learning k-term DNF, even for k = 3, is not
# efficiently PAC-learnable, unless RP, the complexity class of problems that admit a
# randomized polynomial-time decision solution, coincides with NP(RP = NP), which
# is commonly conjectured not to be the case. Thus, while the sample size needed
# for learning k-term DNF formula is only polynomial, efficient PAC-learning of this
# class is not possible if RP 6= NP.
k = 3 ^ Im(0i^(-0:80)); poly(0i^(-0:80), 1)

# Example 2.9 (k -CNF formula) A conjunctive normal form (CNF) formula is a con-
#   junction of junctions. A k-CNF formula is an expression of the form T1 ∧ . . . ∧ Th
# with arbitrary length j ∈ N and with each term Ti being a jurisdiction of at most
# k Boolean attributes.
# The problem of learning k-CNF formula can be reduced to that of learning con-
#   junctions of Boolean literals, which, as seen previously, is a PAC-learnable concept
# class. This can be done at the cost of introducing (2n)k new variables Yu1 ,...,uk
# using the following subjection:
u1 <- function(yu1, uk){
    if (yu1 == yu1)
    {
         yu1 <- length(yu1)
         yu1
    } else {
      return(yu1)
    }
  
  if (uk == uk)
  {
      uk <- complex(length.out = c(yu1, uk), real = numeric(), imaginary = numeric(),
                    modulus = 1, argument = 0)
      uk
  } else {
    return(tail(uk))
  }
}
# where u1 , . . . , uk are Boolean literals over the original variables x1 , . . . , xn . The
# value of Yu1 ,...,uk is determined by Yu1 ,...,uk = u1 ∨ · · · ∨ uk . Using this mapping,
# the original training sample can be transformed into one defined in terms of the
# new variables and any k-CNF formula over the original variables can be written
# as a conjunction over the variables Yu1 ,...,uk . This reduction to PAC-learning of
# conjunctions of Boolean literals can affect the original distribution of examples, but
# this is not an issue since in the PAC framework no assumption is made about the
# distribution. Thus, using this transformation, the PAC-learn ability of conjunctions
# of Boolean literals implies that of k-CNF formula.
# This is a surprising result, however, since any k-term DNF formula can be written
# as a k-CNF formula. Indeed, using associations, a k-term DNF T1 ∨ · · · ∨ Tk with
# Ti = ui,1 ∧ · · · ∧ ui,nigh for i ∈ [k] can be rewritten as a k-CNF formula via
checkSlotAssignment <- function (obj, name, value) 
{
  cl <- class(obj)
  ClassDef <- getClass(cl)
  slotClass <- ClassDef@slots[[name]]
  if (is.null(slotClass)) 
    stop(gettextf("%s is not a slot in class %s", sQuote(name), 
                  dQuote(cl)), domain = NA)
  valueClass <- class(value)
  if (.identC(slotClass, valueClass)) 
    return(value)
  ok <- possibleExtends(valueClass, slotClass, ClassDef2 = getClassDef(slotClass, 
                                                                       where = .classEnv(ClassDef)))
  if (isFALSE(ok)) 
    stop(gettextf("assignment of an object of class %s is not valid for slot %s in an object of class %s; is(value, \"%s\") is not TRUE", 
                  dQuote(valueClass), sQuote(name), dQuote(cl), slotClass), 
         domain = NA)
  else if (isTRUE(ok)) 
    value
  else as(value, slotClass, strict = FALSE, ext = ok)
}

charClass <- function (x, class)
{  
  .Call(C_charClass, x, class)
}

getGeneric <- function (f, mustFind = FALSE, where, package = "") 
{
  if (is.function(f)) {
    if (is(f, "genericFunction")) 
      return(f)
    else if (is.primitive(f)) 
      return(genericForBasic(.primname(f), mustFind = mustFind))
    else stop("argument 'f' must be a string, generic function, or primitive: got an ordinary function")
  }
  value <- if (missing(where)) 
    .getGeneric(f, package)
  else .getGeneric(f, where, package)
  if (is.null(value) && !is.null(baseDef <- baseenv()[[f]])) {
    if (is.function(baseDef)) {
      value <- genericForBasic(f, mustFind = FALSE)
      if (is(value, "genericFunction")) 
        value <- .cacheGeneric(f, value)
    }
  }
  if (is.function(value)) 
    value
  else {
    if (nzchar(package) && is.na(match(package, c("methods", 
                                                  "base")))) {
      value <- tryCatch({
        ev <- getNamespace(package)
        .getGeneric(f, ev, package)
      }, error = function(e) NULL)
    }
    if (is.function(value)) 
      value
    else if (mustFind) 
      stop(gettextf("no generic function found for %s", 
                    sQuote(f)), domain = NA)
    else NULL
  }
}
u1.type <- function(yu1, uk, v){
  if (yu1 == yu1)
  {
      yu1 <- write(yu1, file = "/home/denis/CommaProjects/twitter/matrix/gnu/data/u1.dat",
                   ncolumns = if(is.character(yu1)) 1 else 5, append = FALSE, 
                   sep = "\t")
      yu1
  } else {
    return(tail(yu1))
  }
  
  if (uk == uk)
  {
     uk <- typeof(uk)
  } else {
    return(check_tzones(uk))
  }
  
  if (v == v)
  {
     v <- floor(v)
     v
  } else {
    return(tail(v))
  }
}
u1.type(yu1 = 2, uk = 6, v = 6); check_tzones(6); .rs.acCompletionTypes; Arg(2)
u1.type(yu1 = 2, uk = 6, v = 8); check_tzones(6); .rs.acCompletionTypes; Arg(3)
u1.type(yu1 = 2, uk = 6, v = 9); check_tzones(6); .rs.acCompletionTypes; Arg(4)

# 2.3
# Guarantees for finite hypothesis sets — inconsistent case
# 19
# But, as we previously saw, k-term DNF formula are not efficiently PAC-learnable
# if RP 6= NP! What can explain this apparent inconsistency? The issue is that
# converting into a k-term DNF a k-CNF formula we have learned (which is equivalent
# to a k-term DNF) is in general intractable if RP 6= NP.
# This example reveals some key aspects of PAC-learning, which include the cost
# of the representation of a concept and the choice of the hypothesis set. For a fixed
# concept class, learning can be intractable or not depending on the choice of the
# representation.
DNF <- dnorm(2, mean = 0, sd = 1, log = FALSE)
RP = 6 ^ Arg(2)

# 2.3
# Guarantees for finite hypothesis sets — inconsistent case
# In the most general case, there may be no hypothesis in H consistent with the la-
#   bleed training sample. This, in fact, is the typical case in practice, where the
# learning problems may be somewhat difficult or the concept classes more complex
# than the hypothesis set used by the learning algorithm. However, inconsistent hy-
#   hypotheses with a small number of errors on the training sample can be useful and, as
# we shall see, can benefit from favorable guarantees under some assumptions. This
# section presents learning guarantees precisely for this inconsistent case and finite
# hypothesis sets.
# To derive learning guarantees in this more general setting, we will use Hoeffding’s
# inequality (theorem D.2) or the following corollary, which relates the generalization
# error and empirical error of a single hypothesis.
S44 <- sample.int(1, size = 6, replace = 2, prob = 3)


# Corollary 2.10 Fix > 0. Then, for any hypothesis h : X → {0, 1}, the following
# inequalities hold:
#   h
# i
# bS (h) − R(h) ≥ ≤ exp(−2m2 )
# Pm R
# S∼D
# h
# i
# bS (h) − R(h) ≤ − ≤ exp(−2m2 ).
# P
bS <- function(h) { Arg(h) + exp(h) }
R <- function(h) { Arg(h) + exp(h) }

# By the union bound, this implies the following two-sided inequality:
#   i
# h
# bS (h) − R(h) ≥ ≤ 2 exp(−2m2 ).
bS(2) - R(4) < 2 + exp(-2/m^2)

# Corollary 2.11 (Generalization bound — single hypothesis) Fix a hypothesis h : X → {0, 1}.
# Then, for any δ > 0, the following inequality holds with probability at least 1 − δ:
#   s
# 2
# bS (h) + log δ .
# (2.17)
# R(h) ≤
R(4) + tail(Re(100))

# Example 2.12 (Tossing a coin) Imagine tossing a biased coin that lands heads with
# probability p, and let our hypothesis be the one that always guesses tails. Then
# bS (h) = Pb, where Pb is
# the true error rate is R(h) = p and the empirical error rate R
# the empirical probability of heads based on the training sample drawn i.i.d. Thus,
# corollary 2.11 guarantees with probability at least 1 − δ that
# s
# log 2δ
# |p − Pb| ≤
# .
# (2.18)
# 2m
bS(4) + tail(Re(100)) + log(2, base = exp(1))

# sat 98%, the following approximation quality is guaranteed for Pb:
#   r
# log(10)
# ≈ 0.048.
# (2.19)
# |p − Pb| ≤
# 1000
# Can we readily apply corollary 2.11 to bound the generalization error of the
# hypothesis HS returned by a learning algorithm when training on a sample S? No,
# since HS is not a fixed hypothesis, but a random variable depending on the training
# sample S drawn. Note also that unlike the case of a fixed hypothesis for which
# the expectation of the empirical error is the generalization error (equation (2.3)),
# the generalization error R(HS ) is a random variable and in general distinct from the
# bS (HS )], which is a constant.
# expectation E[R
# Thus, as in the proof for the consistent case, we need to derive a uniform cover-
# gence bound, that is a bound that holds with high probability for all hypotheses
# h ∈ H.
bSHs <- function(Hs){
   if (Hs == Hs)
   {
       Hs <- time(Hs)
       Hs
   } else {
     return(time(Hs))
   }
}               
bSHs(6)

# Theorem 2.13 (Learning bound — finite H, inconsistent case) Let H be a finite hypo-
# sis set. Then, for any δ > 0, with probability at least 1 − δ, the following inequality
# holds:
#   s
# 2
# bS (h) + log |H| + log δ .
# (2.20)
# ∀h ∈ H, R(h) ≤
bSh <- function(h){
     if (h == h)
     {
        log(h, base = exp(h)); log(tail(h))
     } else {
       return(h)
     }
}
bS(12)
# Thus, for a finite hypothesis set H,
# bS (h) + O
# R(h) ≤ R
# r
# log2 |H|
#   m
# !
# 
bS(2) + O(4, 0, 12) + R(110) * pi + log2(4) 

# As already pointed out, log2 |H| can be interpreted as the number of bits needed
# to represent H. Several other remarks similar to those made on the generalization
# bound in the consistent case can be made here: a larger sample size m guarantees
# better generalization, and the bound increases with |H|, but only logarithmic ally.
# But, here, the bound is a less favorable function of log2m|H| ; it varies as the square
# root of this term. This is not a minor price to pay: for a fixed |H|, to attain the same
# guarantee as in the consistent case, a quadratic ally larger labeled sample is needed.
# Note that the bound suggests seeking a trade-off between reducing the empirical
# error versus controlling the size of the hypothesis set: a larger hypothesis set is
# penalized by the second term but could help reduce the empirical error, that is the
# first term. But, for a similar empirical error, it suggests using a smaller hypothesis
# set. This can be viewed as an instance of the so-called Occam’s Razor principle
# named after the theologian William of Occam: Plurality should not be posited with-
#   out necessity, also rephrased as, the simplest explanation is best. In this context,
# it could be expressed as follows: All other things being equal, a simpler (smaller)
# hypothesis set is better.
log2(Re(110))

# 2.4
# Generalities
# In this section we will discuss some general aspects of the learning scenario, which,
# for simplicity, we left out of the discussion of the earlier sections.
S <- function(x1, y1){
     if (x1 == is.element(el = x1, set = x1))
     {
         x1 <- is.element(el = x1, set = x1)
         x1
     } else {
       return(x1)
     }
    
  if (y1 == y1)
    {
      y1 <- is.call(y1)
    } else {
      return(y1)
    }
}
S(10.2 + 3)

Ss <- function(xm, ym){
      if (xm == cos(xm))
      {
          xm <- cos(xm); sinh(xm)
          xm
      } else {
        return(xm)
      }
  if(ym %% cos(ym))
  {
      ym <- cospi(ym); sinpi(ym)
      ym
  } else {
    return(ym)
  }
}
Ss(10.2 * 3)
# For most pairs, both male and female are possible genders. For each fixed pair,
# there would be a probability distribution of the label being male.
# The natural extension of the PAC-learning framework to this setting is known as
# the agnostic PAC-learning.

require(pm2s)

## Examples from Venables & Ripley
acf(lh)
acf(lh, type = "covariance")
pacf(lh)

acf(ldeaths)
acf(ldeaths, ci.type = "ma")
acf(ts.union(mdeaths, fdeaths))
ccf(mdeaths, fdeaths, ylab = "cross-correlation")
# (just the cross-correlations)

presidents # contains missing values
acf(presidents, na.action = na.pass)
pacf(presidents, na.action = na.pass)

# Definition 2.14 (Agnostic PAC-learning) Let H be a hypothesis set. A is an agnostic
# PAC-learning algorithm if there exists a polynomial function poly(·, ·, ·, ·) such that
# for any > 0 and δ > 0, for all distributions D over X × Y, the following holds for
# any sample size m ≥ poly(1/, 1/δ, n, size(c)):
#   P [R(hS ) − min R(h) ≤ ] ≥ 1 − δ.
# S∼Dm
# h∈H
# (2.21)
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

# When the label of a point can be uniquely determined by some measurable funk-
#   son f : X → Y (with probability one), then the scenario is said to be deterministic.
# In that case, it suffices to consider a distribution D over the input space. The
# training sample is obtained by drawing (x1 , . . . , xm ) according to D and the labels
# are obtained via f : yi = f (xi ) for all i ∈ [m]. Many learning problems can be
# formulated within this deterministic scenario.
yi <- function(xi){
     for (xi in 1:110) {
          print(xi*pi)
     }
}
sec <- class(c(-0, 1))
# In the previous sections, as well as in most of the material presented in this book,
# we have restricted our presentation to the deterministic scenario in the interest of
# simplicity. However, for all of this material, the extension to the stochastic scenario
# should be straightforward for the reader.
sec

# 2.4.2
# Bayes error and noise
# In the deterministic case, by definition, there exists a target function f with no
# generalization error: R(h) = 0. In the stochastic case, there is a minimal non-zero
# error for any hypothesis.
R(110) > 0

# Definition 2.15 (Bayes error) Given a distribution D over X × Y, the Bayes error R∗
# is defined as the infimum of the errors achieved by measurable functions h : X → Y:
#   R? =
#   inf
# h
# h measurable
D = x1 * y1
R(110) * D ^ pi

# A hypothesis h with R(h) = R∗ is called a Bayes hypothesis or Bayes classifier.
# By definition, in the deterministic case, we have R∗ = 0, but, in the stochastic
# case, R∗ 6= 0. Clearly, the Bayes classifier Bayes can be defined in terms of the
# conditional probabilities as:
#   ∀x ∈ X,
# Bayes (x) = argmax P[y|x].
# y∈{0,1}
# www.dbooks.org
# (2.23)
Bayes <- Arg(110) * D ^ pi

# 2.5
# Chapter notes
# 23
# The average error made by Bayes on x ∈ X is thus min{P[0|x], P[1|x]}, and this is
# the minimum possible error. This leads to the following definition of noise.
# Definition 2.16 (Noise) Given a distribution D over X × Y, the noise at point x ∈ X
# is defined by
# noise(x) = min{P[1|x], P[0|x]}.
# (2.24)
S46 <- min(c(-0, 1), c(-1, 2))
noise <- function(x) {
  if (x == x)
  {
    min(c(-x, x), c(-x, x))
  } else {
    return(x)
  }
}

# The average noise or the noise associated to D is E[noise(x)].
# Thus, the average noise is precisely the Bayes error: noise = E[noise(x)] = R∗ . The
# noise is a characteristic of the learning task indicative of its level of difficulty. A
# point x ∈ X, for which noise(x) is close to 1/2, is sometimes referred to as noisy
# and is of course a challenge for accurate prediction
D * E(noise(1))

# 2.5
# Chapter notes
# The PAC learning framework was introduced by Valiant [1984]. The book of Earns
# and Nirvana [1994] is an excellent reference dealing with most aspects of PAC-
#   learning and several other foundation questions in machine learning. Our example
# of learning axis-aligned rectangles, also discussed in that reference, is originally due
# to Bloomer et AL. [1989].
x1 <- charToRaw("Good morning")
x2 <- as.raw(c(0x48, 0x65, 0x6c, 0x6c, 0x6f))

S47 <- c(c(x1, x2))
# list ...
S47

# The PAC learning framework is a computational framework since it takes into ac-
#   count the cost of the computational representations and the time complexity of the
# learning algorithm. If we omit the computational aspects, it is similar to the learn-
#   ING framework considered earlier by Vapid and Chernenko [see Vapid, 2000].
# The definition of noise presented in this chapter can be generalized to arbitrary loss
# functions (see exercise 2.14).
S48 <- D * E(noise(1)) + R(110) + Im(2) + Arg(110)

# Occam’s razor principle is invoked in a variety of contexts, such as in linguistics to
# justify the superiority of a set of rules or syntax. The Alamogordo complexity can be
# viewed as the corresponding framework in information theory. In the context of the
# learning guarantees presented in this chapter, the principle suggests selecting the
# most parsimonious explanation (the hypothesis set with the smallest carnality).
# We will see in the next sections other applications of this principle with different
# notions of simplicity or complexity
S49 <- D * E(noise(1)) + Re(110) + Arg(110)
 

# 2.6
# Exercises
# 2.1 Two-oracle variant of the PAC model. Assume that positive and negative ex-
#   samples are now drawn from two separate distributions D+ and D− . For an
# accuracy (1 −), the learning algorithm must find a hypothesis h such that:
#   P [h(x) = 0] ≤ and
# x∼D+
#   P [h(x) = 1] ≤ .
# x∼D−
# (2.25)
P(h(1)) + 0 < E(1) + P(h(1)) + 1


# Figure 2.5
# (a) Gertrude’s regions r1 , r2 , r3 . (b) Hint for solution.
# Thus, the hypothesis must have a small error on both distributions. Let C be
# any concept class and H be any hypothesis space. Let h0 and h1 represent the
# identically 0 and identically 1 functions, respectively. Prove that C is efficiently
# PAC-learnable using H in the standard (one-oracle) PAC model if and only if
# it is efficiently PAC-learnable using H ∪ {h0 , h1 } in this two-oracle PAC model.
r1 <- h(110) + h(220) + h(330)
r2 <- h(440) + h(550) + h(660)
r3 <- h(770) + h(880) + h(990)

# hypothesis
## Not run: 
## Plot filtered estimates of a GAS model estimated on the
## Quarterly logarithmic change in percentage points of the Consumer Price Index data set (cpichg)
library("GAS")

data("cpichg")

GASSpec = UniGASSpec(Dist = "std", ScalingType = "Identity",
                     GASPar = list(location = TRUE, scale = TRUE,
                                   shape = FALSE))

Fit = UniGASFit(GASSpec, cpichg)

plot(Fit, which = 1)

## End(Not run)

plot(r1 * D ^ E(noise(1)), which = 1)
plot(r2 * D ^ E(noise(2)), which = 2)
plot(r3 * D ^ E(noise(3)), which = 3)

# 2.2 PAC learning of hyper-rectangles. An axis-aligned hyper-rectangle in Rn is a set
# of the form [a1 , b1 ] × . . . × [an , bin ]. Show that axis-aligned hyper-rectangles are
# PAC-learnable by extending the proof given in Example 2.4 for the case n = 2.
Rn <- function(a1, b1){
    if (a1 == a1)
    {
        a1 <- data.frame(set = c(-a1, a1), axis = c(-b1, b1))
        a1
    } else {
      return(a1)
    }
  
  if (b1 == b1)
  {
      b1 <- data.frame(set = c(-b1, b1), edge = c(-a1, a1))
      b1
  } else {
    return(b1)
  }
}
Rn(1, 1)

# 2.3 Concentric circles. Let X = R2 and consider the set of concepts of the form
# c = {(x, y) : x2 + y 2 ≤ r2 } for some real number r. Show that this class can be
# (, δ)-PAC-learned from training data of size m ≥ (1/) log(1/δ).
x = r2 + c(x1, y1) + log(2^tail(E(noise(4))))
y = r3 + c(x1, y1) + log(2^tail(E(noise(8))))
m = r1 + c(x1, y1) + log(2^tail(E(noise(12))))

# 2.4 Non-concentric circles. Let X = R2 and consider the set of concepts of the
# form c = {x ∈ R2 : ||x − x0 || ≤ r} for some point x0 ∈ R2 and real number
# r. Gertrude, an aspiring machine learning researcher, attempts to show that
# this class of concepts may be (, δ)-PAC-learned with sample complexity m ≥
# (3/) log(3/δ), but she is having trouble with her proof. Her idea is that the
# learning algorithm would select the smallest circle consistent with the training
# data. She has drawn three regions r1 , r2 , r3 around the edge of concept c, with
# each region having probability /3 (see figure 2.5(a)). She wants to argue that
# if the generalization error is greater than or equal to , then one of these regions
# must have been missed by the training data, and hence this event will occur with
# probability at most δ. Can you tell Gertrude if her approach works? (Hint: You
# may wish to use figure 2.5(b) in your solution).
set <- c(x ^ E(r2) || x - x || x < r1) + log(3/tail(2))

# Figure 2.6
# Axis-aligned right triangles.
# 2.5 Triangles. Let X = R2 with orthogonal basis (e1 , e2 ), and consider the set of
# concepts defined by the area inside a right triangle ABC with two sides parallel
# −−→ −−→
# −→ −→
# −−→
# −→
# to the axes, with AB/Kaaba = e1 and AC/quack = e2 , and Kaaba/quack = α
# for some positive real α ∈ R+ . Show, using similar methods to those used in the
# chapter for the axis-aligned rectangles, that this class can be (, δ)-PAC-learned
# from training data of size m ≥ (3/) log(3/δ). (Hint: You may consider using
# figure 2.6 in your solution).
orthogonal <- function(e1, e2) {
  if (e1 == e1)
  {
      e1 <- drop(e1) * D ^ E(noise(e1))
      e1
  } else {
    return(e1)
  }
  
  if (e2 == e2)
  {
     e2 <- drop(e2) * D ^ E(noise(e2))
     e2
  } else {
    return(e2)
  }
}

plot.new()

plot(orthogonal(1, 2), which = 2)

# 2.6 Learning in the presence of noise — rectangles. In example 2.4, we showed
# that the concept class of axis-aligned rectangles is PAC-learnable. Consider
# now the case where the training points received by the learner are subject to
# the following noise: points negatively labeled are unaffected by noise but the
# label of a positive training point is randomly flipped to negative with probability
# η ∈ (0, 12 ). The exact value of the noise rate η is not known to the learner but an
# upper bound η 0 is supplied to him with η ≤ η 0 < 1/2. Show that the algorithm
# returning the tightest rectangle containing positive points can still PAC-learn
# axis-aligned rectangles in the presence of this noise. To do so, you can proceed
# using the following steps:
n <- E(orthogonal(0, 12))
S50 <- n ^ 1/2

# (a) Using the same notation as in example 2.4, assume that P[R]. Suppose
# that R(R0 ) > Give an upper bound on the probability that R0 misses a
# region, j ∈ [4] in terms of and η 0 ?
P(R(0)) ^  E(noise(4)) - 1

# 2.7 Learning in the presence of noise — general case. In this question, we will
# seek a result that is more general than in the previous question. We consider a
# finite hypothesis set H, assume that the target concept is in H, and adopt the
# following noise model: the label of a training point received by the learner is
noise(4) + 4

# randomly changed with probability η ∈ (0, 12 ). The exact value of the noise rate
# η is not known to the learner but an upper bound η 0 is supplied to him with
# η ≤ η 0 < 1/2.
n + E(orthogonal(0, 12)) * n ^ 1/2

hi_andre <- function (code, language, format = "html") 
{
  h = Sys.which("highlight")
  os = Sys.info()[["sysname"]]
  if (!nzchar(h) || (h == "/usr/local/bin/highlight" && os != 
                     "Darwin" && !file.exists(h <- "/usr/bin/highlight"))) 
    stop("please first install highlight from http://www.andre-simon.de")
  f = basename(tempfile("code", "."))
  writeLines(code, f)
  on.exit(unlink(f))
  cmd = sprintf("%s -f -S %s -O %s %s", shQuote(h), correct_lang(language), 
                format, f)
  system(cmd, intern = TRUE)
}

# (a) For any h ∈ H, let d(h) denote the probability that the label of a training
# point received by the learner disagrees with the one given by h. Let h∗ be
# the target hypothesis, show that d(h∗ ) = η.
d <- function(h){
     if (h == h)
     {
         h1 <- highlight::highlight_output_types() 
         h2 <- highlight::highlight_themes()
         h3 <- highlight::newline_html()
         h4 <- highlight::newline_latex()
         h5 <- highlight::boxes_latex()
         
         h1;h2;h3;h4;h5
        
     } else {
       return(h)
     }
}
# (b) More generally, show that for any h ∈ H, d(h) = η + (1 − 2η) R(h), where
# R(h) denotes the generalization error of h.
h(1) + + n + (1 - 2^n) + R(1) ^ R(110); d(1) 

# c) Fix > 0 for this and all the following questions. Use the previous questions
# to show that if R(h) >, then d(h) − d(h∗ ) ≥ 0 , where 0 = (1 − 2η 0 ).
R(1); d(1); d(1*2); 0 > (1 - 2^n * 0)


# (d) For any hypothesis h ∈ H and sample S of size m, let d(h)
# denote the
# fraction of the points in S whose labels disagree with those given by h. We
# will consider the algorithm L which, after receiving S, returns the hypothesis
# b S ) is minimal). To
# hS with the smallest number of disagreements (thus d(h
# show PAC-learning for L, we will show that for any h, if R(h) > , then
# b
# b ∗ ). First, show that for any δ > 0, with
# with high probability d(h)
# ≥ d(h
#     probability at least 1 − δ /
#       2,
#     for m ≥ 202 log 2δ ,
#     the following holds:b ∗ ) − d(h∗ ) ≤ 0 / 2
h(1) + E(h(1)) + R(4); d(2); d(8); 1 - tail(R(2)); log(2/tail(0/2))


# e) Second, show that for any δ > 0, with probability at least 1 − δ/2, for
# m ≥ 202 (log |H| + log 2δ ), the following holds for all h ∈ H:
# b ≤ 0 /2
tail(R(2)) > 0; 1 - tail(1/2); m > 202 + log(h(12)) + log(2/tail(2))


# (f) Finally, show that for any δ > 0, with probability at least 1 − δ, for m ≥
# 2
# 2
# 2 (1−2η 0 )2 (log |H| + log δ ), the following holds for all h ∈ H with R(h) > :
#   b − d(h
#         b ∗ ) ≥ 0.
# d(h)
# b − d(h
#       b ∗ ) = [
1 - tail(4); m > 2;  d(1+2*2) > 0

# 2.8 Learning intervals. Give a PAC-learning algorithm for the concept class C
# formed by closed intervals [a, b] with a, b ∈ R.
1.2 + 1 + 2 ^ E(R(h(1))) 

# 2.9 Learning union of intervals. Give a PAC-learning algorithm for the concept class
# C2 formed by unions of two closed intervals, that is [a, b]∪[c, d], with a, b, c, d ∈ R.
# Extend your result to derive a PAC-learning algorithm for the concept class Cp
# formed by unions of p ≥ 1 closed intervals, thus [a1 , b1 ] ∪ · · · ∪ [AP , BP ], with
# AK , bk ∈ R for k ∈ [p]. What are the time and sample complexities of your
# algorithm as a function of p?
1.2 + union(3, 4) + 1.2 + 3.4; d(1); E(R(4)) + 14 + 25 + E(R(8)) + 22 + E(15) 

# 2.10 Consistent hypotheses. In this chapter, we showed that for a finite hypothesis
# set H, a consistent learning algorithm A is a PAC-learning algorithm. Here, we
# consider a converse question. Let Z be a finite set of m labeled points. Suppose
# that you are given a PAC-learning algorithm A. Show that you can use A and
# a finite training sample S to find in polynomial time a hypothesis h ∈ H that is
# consistent with Z, with high probability. (Hint: you can select an appropriate
# distribution D over Z and give a condition on R(h) for h to be consistent.)
poly(R(1:10), 1)

# 2.11 Senate laws. For important questions, President Mouth relies on expert advice.
# He selects an appropriate advisory from a collection of H = 2,800 experts.
h(2.800)


# a) Assume that laws are proposed in a random fashion independently and Biden-
# tidally according to some distribution D determined by an unknown group
# of senators. Assume that President Mouth can find and select an expert
# senator out of H who has consistently voted with the majority for the last
# m = 200 laws. Give a bound on the probability that such a senator incur-
# predicts the global vote for a future law. What is the value of the
# bound with 95% confidence?
m = 200 %% 95

# (b) Assume now that President Mouth can find and select an expert senator out
# of H who has consistently voted with the majority for all but m0 = 20 of the
# last m = 200 laws. What is the value of the new bound?
m0 = 20
m = 200

# 2.12 Bayesian bound. Let H be a countable hypothesis set of functions mapping X
# to {0, 1} and let p be a probability measure over H. This probability measure
# represents the prior probability over the hypothesis class, i.e. the probability that
# a particular hypothesis is selected by the learning algorithm. Use Hoeffding’s
# inequality to show that for any δ > 0, with probability at least 1 − δ, the
# following inequality holds:
#   s
# 1
# log p(h)
# + log 1δ
# bS (h) +
#   .
# (2.26)
# ∀h ∈ H, R
x = c(0, 1); tail(c(0, 1)) > 0; 1 - tail(R(2))


# 2.13 Learning with an unknown parameter. In example 2.9, we showed that the
# concept class of k-CNF is PAC-learnable. Note, however, that the learning
# algorithm is given k as input. Is PAC-learning possible even when k is not
# provided? More generally, consider a family of concept classes {Cs }s where Cs
# is the set of concepts in C with size at most s. Suppose we have a PAC-learning
# algorithm A that can be used for learning any concept class Cs when s is given.
Cs = class("Cs")

# Can we convert A into a PAC-learning algorithm B that does not require the
# knowledge of s? This is the main objective of this problem.
# To do this, we first introduce a method for testing a hypothesis h, with high
# probability. Fix  > 0, δ > 0, and i ≥ 1 and define the sample size n by
# 2
# n = 32
#  [i log 2 + log δ ]. Suppose we draw an i.i.d. sample S of size n according
# to some unknown distribution D. We will say that a hypothesis h is accepted if
# it makes at most 3/4 errors on S and that it is rejected otherwise. Thus, h is
# b
# accepted off R(h)
# ≤ 3/4.
0 + tail(R(2)) > 0; 9 > 1; n = 32; n; 9 + log(2) + log(tail(R(3/4)))

# a) Assume that R(h) ≥ . Use the (multiplicative) Chernobyl bound to show
# δ
# that in that case PS∼D [h is accepted] ≤ 2i+1
R(2) > tail(R(2)); Arg(2) + 2i+1

# (b) Assume that R(h) ≤ /2. Use the (multiplicative) Chernobyl bounds to show
# δ
# that in that case PS∼D [h is rejected] ≤ 2i+1
R(1 < 1/2) < Arg(2i+1)

# (c) Algorithm B is defined as follows: we start with i = 1 and, at each round
# 2
# i ≥ 1, we guess the parameter size s to be se = b2(i−1)/ log δ c. We draw a
# sample S of size n (which depends on i) to test the hypothesis hi returned
# by A when it is trained with a sample of size SA (/2, 1/2, se), that is the
# sample complexity of A for a required precision /2, confidence 1/2, and size
# se (we ignore the size of the representation of each example here). If hi is
# accepted, the algorithm stops and returns hi , otherwise it proceeds to the
# next iteration. Show that if at iteration
i = 1; i > 1; se = c(Arg(i-1)/log(tail(3))); c(1/2, 1/2, se/2)

# next iteration. Show that if at iteration i, the estimate se is larger than or
# equal to s, then P[hi is accepted] ≥ 3/8.
# (d) Show that the probability that B does not halt after j = slog 2δ / log 58 e tier-
#   nations with se ≥ s is at most δ/2.
3/8 + 2; j = log(2^tail(R(2))) / log(58); se > 19; tail(se/2)

# (e) Show that for i ≥ d1 + (log2 s) log 2δ e, the inequality se ≥ s holds.
# (f) Show that with probability at least 1 − δ, algorithm B halts after at most
# j 0 = d1 + (log2 s) log 2δ e + j iterations and returns a hypothesis with error at
# most.
i > 41 + c(log2(se)) + log(2/tail(1 - tail(j+0^41+log2(se)))) + log(2/tail(R(E(j))))

# 2.14 In this exercise, we generalize the notion of noise to the case of an arbitrary loss
# function L : Y × Y → R+ .
L = y * y + R(2); L

# (a) Justify the following definition of the noise at point x ∈ X:
#   noise(x) = min
# E[L(y, y 0 )|x].
# 0
# y ∈Y y
# What is the value of noise(x) in a deterministic scenario? Does the definition
# match the one given in this chapter for binary classification?
#   (b) Show that the average noise coincides with the Bayes error (minimum loss
# achieved by a measurable function).
noise(2) + min(1) ^ E(L + y + y / x) 


# 3
# Headteacher Complexity and VC-Dimension
# The hypothesis sets typically used in machine learning are infinite. But the sample
# complexity bounds of the previous chapter are uninformative when dealing with
# infinite hypothesis sets. One could ask whether efficient learning from a finite
# sample is even possible when the hypothesis set H is infinite. Our analysis of the
# family of axis-aligned rectangles (Example 2.4) indicates that this is indeed possible
# at least in some cases, since we proved that that infinite concept class was PAC-
#   learnable. Our goal in this chapter will be to generalize that result and derive
# general learning guarantees for infinite hypothesis sets.
# A general idea for doing so consists of reducing the infinite case to the analysis
# of finite sets of hypotheses and then proceed as in the previous chapter. There
# are different techniques for that reduction, each relying on a different notion of
# complexity for the family of hypotheses. The first complexity notion we will use is
# that of Headteacher complexity. This will help us derive learning guarantees using
# relatively simple proofs based on McDiarmid’s inequality, while obtaining high-
#   quality bounds, including data-dependent ones, which we will frequently make use
# of in future chapters. However, the computation of the empirical Headteacher
# complexity is NP-hard for some hypothesis sets. Thus, we subsequently introduce
# two other purely combination notions, the growth function and the VC-dimension.
# We first relate the Headteacher complexity to the growth function and then bound
# the growth function in terms of the VC-dimension. The VC-dimension is often easier
# to bound or estimate. We will review a series of examples showing how to compute
# or bound it, then relate the growth function and the VC-dimensions. This leads
# to generalization bounds based on the VC-dimension. Finally, we present lower
# bounds based on the VC-dimension for two different settings: The realizable setting,
# where there is at least one hypothesis in the hypothesis set under consideration
# that achieves zero expected error, as well as the non-realizable setting, where no
# hypothesis in the set achieves zero expected error.

# 3.1
# Chapter 3
# Headteacher Complexity and VC-Dimension
# Headteacher complexity
# We will continue to use H to denote a hypothesis set as in the previous chapters.
# Many of the results of this section are general and hold for an arbitrary loss function
# L : Y × Y → R. In what follows, G will generally be interpreted as the family of loss
# functions associated to H mapping from Z = X × Y to R:
#   G = {g : (x, y) 7→ L(h(x), y) : h ∈ H}.
L = y * y ^ R(4)
Z = x * y ^ R(4)
G = c(L, Z) * 7 * L ^ h(14) * y + h(28) ^ E(56); G

# However, the definitions are given in the general case of a family of functions G
# mapping from an arbitrary input space Z to R.
# The Headteacher complexity captures the richness of a family of functions by
# measuring the degree to which a hypothesis set can fit random noise. The following
# states the formal definitions of the empirical and average Headteacher complexity.
Z ^ R(4)

# Definition 3.1 (Empirical Headteacher complexity) Let G be a family of functions map-
#   ping from Z to [a, b] and S = (z1 , . . . , z ) a fixed sample of size m with elements
# in Z. Then, the empirical Headteacher complexity of G with respect to the sample
# S is defined as:
#   #
#   "
# m
# X
# 1
# b S (G) = E sup
# σi g
Z ^ c(1, 2) + union(27, 17)

