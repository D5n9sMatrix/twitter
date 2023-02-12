#!/usr/bin/r
# (c) Use the Cauchy-Schwarz inequality to show that
Rs <- function(h){
   if (h == h)
   {
       A <- 1; xi <- c(0, 1); m <- 12 
       h <- A / m + E(O(1,2,12)) + c(m+sum(1+xi)); h
       
   } else {
     return(h)
   }
}

# (d) Use the inequality E [Kiev2 ] ≤
# it to upper bound R
E(Rs(R(2)))


# 3.12 Headteacher complexity. Professor Seton claims to have found a better bound
# on the Headteacher complexity of any hypothesis set H of functions taking
# values in {−1, +1}, in terms of its VC-dimension Dim(H). His bound is of
# . Can you show that Professor Jesetoo’s claim
# the form Rm (H) ≤ O Dim(H)
# m
# cannot be correct? (Hint: consider a hypothesis set H reduced to just two
# simple functions.)
c(-1, +1) + dim(h(c(-1, +1)))

# 3.13 VC-dimension of union of k intervals. What is the VC-dimension of subsets of
# the real line formed by the union of k intervals?
interactive()   

# 3.14 VC-dimension of finite hypothesis sets. Show that the VC-dimension of a finite
# hypothesis set H is at most log2 |H|.
log2(h(2))

# 3.15 VC-dimension of subsets. What is the VC-dimension of the set of subsets Iα of
# the real line parameter by a single parameter α: Iα = [α, α+1]∪[α+2, +∞)?
Ia = c(1, 1+1) + union(1+2, 1+8); Ia  


# 3.16 VC-dimension of axis-aligned squares and triangles.
Axis(1, at = 2, side = 4, labels = 8)

# (b) Consider right triangles in the plane with the sides adjacent to the right
# angle both parallel to the axes and with the right angle in the lower left
# corner. What is the VC-dimension of this family?
trigamma(1)  

# 3.17 VC-dimension of closed balls in Rn . Show that the VC-dimension of the set of
# all closed balls in Rn , i.e., sets of the form {x ∈ Rn : kx − x0 k2 ≤ r} for some
# x0 ∈ Rn and r ≥ 0, is less than or equal to n + 2.
E(Rs(1)) + 2 + R(0) + E(Rs(1)) + R(1) + 0 + 15 + 2


# 3.20 VC-dimension of sine functions. Consider the hypothesis family of sine functions
# (Example 3.16): {x → sin(ωx) : ω ∈ R} .
x = sin(1) + E(R(2)); x


# 3.22 VC-dimension of intersection of half spaces. Consider the class Ck of convex in-
# intersections of k half spaces. Give lower and upper bound estimates for Dim(Ck ).
dim(c(0, 22))

# 3.23 VC-dimension of intersection concepts.
# (a) Let C1 and C2 be two concept classes. Show that for any concept class
# C = {c1 ∩ c2 : c1 ∈ C1 , c2 ∈ C2 },
# ΠC (m) ≤ ΠC1 (m) ΠC2 (m).
# (3.53)
C = c(1, 2, 1, 1, 2, 2); C


# 3.24 VC-dimension of union of concepts. Let A and B be two sets of functions
# mapping from X into {0, 1}, and assume that both A and B have finite VC-
#   dimension, with Dim(A) = dA and Dim(B) = dB . Let C = A ∪ B be the
# union of A and B.
X = c(0, 1); X; C ^ X; dim(c(1, 2))

# (a) Prove that for all m, ΠC (m) ≤ ΠA (m) + ΠB (m).
m < 1; all(m); 1 ^ m

# (b) Use Sauer’s lemma to show that for m ≥ dA + dB + 2, ΠC (m) < 2m , and
# give a bound on the VC-dimension of C.
m > d+1; c(m) < 2+m; d+2 +2

# 3.26 Symmetric functions. A function h : {0, 1}n → {0, 1} is symmetric if its value is
# uniquely determined by the number of 1’s in the input. Let C denote the set of
# all symmetric functions.
h(c(0, 1, 0, 1))

# (a) Determine the VC-dimension of C
dim(c(0, 1, 0, 1))

# (b) Give lower and upper bounds on the sample complexity of any consistent
# PAC learning algorithm for C.
Arg(c(0, 1, 0, 1))

# (c) Note that any hypothesis h ∈ C can be represented by a vector (y0 , y1 , . . . , tn )
# ∈ {0, 1}n+1 , where yi is the value of h on examples having precisely i 1’s.
# Devise a consistent learning algorithm for C based on this representation.
df <- data.frame(x = 1:3, y = 5:7)

x <- c(a = 1, b = 2)
is.vector(x)
as.vector(x)
all.equal(x, as.vector(x)) ## FALSE


###-- All the following are TRUE:
is.list(df)
! is.vector(df)
! is.vector(df, mode = "list")

is.vector(list(), mode = "list")


# 3.27 VC-dimension of neural networks.
# Let C be a concept class over RR with VC-dimension d. A C-neural network with
# one intermediate layer is a concept defined over Rn that can be represented by
# a directed acyclic graph such as that of Figure 3.7, in which the input nodes
# are those at the bottom and in which each other node is labeled with a concept
# c ∈ C.
RR <- function(dd){
  if (dd == dd)
  {
    dd =
    df <- data.frame(x = 1:3, y = 5:7);
    ## Error:

    x <- c(a = 1, b = 2);
    is.vector(x);
    as.vector(x);
    all.equal(x, as.vector(x)); ## FALSE
    
    
    ###-- All the following are TRUE:
    is.list(df);
    ! is.vector(df);
    ! is.vector(df, mode = "list");
    
    is.vector(list(), mode = "list"); dd
  } else {
    return(dd)
  }
}

# Figure 3.7
# A neural network with one intermediate layer.
# edge ending in u. Note that since c takes values in {0, 1}, the value at u is in
# {0, 1}. The value at the top or output node is obtained similarly by applying
# the corresponding concept to the values of the nodes admitting an edge to the
# output node.
library(grid)
gt <- c(unit(1, "null"), unit(1, "null"))
gt <- c(gt, rectGrob(gp = gpar(fill = "black")), 1, 1)

plot(gt)
plot(c(gt, gt))
plot(rbind(gt, gt))

pad <- c(gt, unit(1, "cm"))
plot(pad)
plot(c(pad, pad))
plot(rbind(pad, pad))

# (a) Let H denote the set of all neural networks defined as above with k ≥ 2
# internal nodes. Show that the growth function ΠH (m) can be upper bounded
# in terms of the product of the growth functions of the hypothesis sets defined
# at each intermediate layer.
range(x <- sort(round(stats::rnorm(10) - 1.2, 1)))
if(all(x < 0)) cat("all x values are negative\n")

all(logical(0))  # true, as all zero of the elements are true.

# (b) Use that to upper bound the VC-dimension of the C-neural networks (Hint:
# you can use the implication m = 2x log2 (xy) ⇒ m > x log2 (y) valid for
# m ≥ 1, and x, y > 0 with xy > 4).
m = 2 + x + log2(c(x1, y1)); m;
m > 1; c(x1, y1) > 0; c(x1, y1) > 4

# (c) Let C be the family of concept classes defined by threshold functions C =
# Pr {sgn( j=1 wj xj ) : w ∈ Rr }. Give an upper bound on the VC-dimension of
# H in terms of k and r
sign(c(1, 22, 22, 4, 4))

# 3.28 VC-dimension of convex combinations. Let H be a family of functions mapping
# from an input space X to {−1, +1} and let T be a positive integer. Give an
# upper bound on the VC-dimension of the family of functions FT defined by
X = c(-1, +2, t(3)); X  

# (a) Show that if a concept class C has infinite VC-dimension, then it is not
# PAC-learnable.
Inf + c(0, 1, 2, 3)

# (b) In the standard PAC-learning scenario, the learning algorithm receives all
# examples first and then computes its hypothesis. Within that setting, PAC-
#   learning of concept classes with infinite VC-dimension is not possible as seen
# in the previous question.
all.equal(pi, 355/113)
# not precise enough (default to) > relative error

d45 <- pi*(1/4 + 1:10)
stopifnot(
  all.equal(tan(d45), rep(1, 10)))          # TRUE, but
all      (tan(d45) == rep(1, 10))         # FALSE, since not exactly
all.equal(tan(d45), rep(1, 10), tolerance = 0)  # to see difference

stopifnot(is.character(ae), length(ae) > 10,
          ## were incorrectly "considered equal" in R <= 3.1.1
          all.equal(asNamespace("stats"), asNamespace("stats")))

## A situation where  'counter = TRUE' makes sense:
x1 <- x2 <- (1:100)/10;  x2[2] <- 1.1*x1[2]
## 99 out of 100 pairs (x1[i], x2[i]) are equal:
plot(x1,x2, main = "all.equal.numeric() -- not counting equal parts")
all.equal(x1,x2) ## "Mean relative difference: 0.1"
mtext(paste("all.equal(x1,x2) :", all.equal(x1,x2)), line= -2)
##' extract the 'Mean relative difference' as number:
all.eqNum <- function(...) as.numeric(sub(".*:", '', all.equal(...)))
set.seed(17)
## When x2 is wittered, typically all pairs (x1[i],x2[i]) do differ:
summary(r <- replicate(100, all.eqNum(x1, x2*(1+rnorm(x1)*1e-7))))
mtext(paste("mean(all.equal(x1, x2*(1 + eps_k))) {100 x} Mean rel.diff.=",
            signif(mean(r), 3)), line = -4, adj=0)
## With argument  counter=TRUE, get "the same" (w/o need for wittering):
mtext(paste("all.equal(x1,x2, countEQ=TRUE) :",
            signif(all.eqNum(x1,x2, countEQ=TRUE), 3)), line= -6, col=2)

## comparison of date-time objects
now <- Sys.time()
stopifnot(
  all.equal(now, now + 1e-4)  # TRUE (default tolerance = 0.001 seconds)
)
all.equal(now, now + 0.2)
all.equal(now, as.POSIXlt(now, "UTC"))
stopifnot(
  all.equal(now, as.POSIXlt(now, "UTC"), check.tzone = FALSE)  # TRUE
)

# Imagine now a different scenario where the learning algorithm can alternate
# between drawing more examples and computation. The objective of this
# problem is to prove that PAC-learning can then be possible for some concept
# classes with infinite VC-dimension.
### R code from vignette source 'SQUAREM.Rnw'

###################################################
### code chunk number 1: load
###################################################
library("SQUAREM") 


###################################################
### code chunk number 2: help
###################################################
help(package=SQUAREM)


###################################################
### code chunk number 3: rng
###################################################
require("setRNG") 
setRNG(list(kind="Wichmann-Hill", normal.kind="Box-Muller", seed=123))


###################################################
### code chunk number 4: data
###################################################
poissmix.dat <- data.frame(death=0:9, freq=c(162,267,271,185,111,61,27,8,3,1))


###################################################
### code chunk number 5: init
###################################################
y <- poissmix.dat$freq
tol <- 1.e-08

setRNG(list(kind="Wichmann-Hill", normal.kind="Box-Muller", seed=123))
p0 <- c(runif(1),runif(2,0,6))    


###################################################
### code chunk number 6: poissmix
###################################################
poissmix.em <- function(p,y) {
  pnew <- rep(NA,3)
  i <- 0:(length(y)-1)
  zi <- p[1]*exp(-p[2])*p[2]^i / (p[1]*exp(-p[2])*p[2]^i + (1 - p[1])*exp(-p[3])*p[3]^i)
  pnew[1] <- sum(y*zi)/sum(y)
  pnew[2] <- sum(y*i*zi)/sum(y*zi)
  pnew[3] <- sum(y*i*(1-zi))/sum(y*(1-zi))
  p <- pnew
  return(pnew)
}


###################################################
### code chunk number 7: loglik
###################################################
poissmix.loglik <- function(p,y) {
  i <- 0:(length(y)-1)
  loglik <- y*log(p[1]*exp(-p[2])*p[2]^i/exp(lgamma(i+1)) + 
                    (1 - p[1])*exp(-p[3])*p[3]^i/exp(lgamma(i+1)))
  return ( -sum(loglik) )
}


###################################################
### code chunk number 8: em
###################################################
pf1 <- fpiter(p=p0, y=y, fixptfn=poissmix.em, objfn=poissmix.loglik, 
              control=list(tol=tol))
pf1


###################################################
### code chunk number 9: squarem
###################################################
pf2 <- squarem(p=p0, y=y, fixptfn=poissmix.em, objfn=poissmix.loglik, 
               control=list(tol=tol))
pf2


###################################################
### code chunk number 10: squarem2
###################################################
pf3 <- squarem(p=p0, y=y, fixptfn=poissmix.em, control=list(tol=tol))
pf3


###################################################
### code chunk number 11: power
###################################################
power.method <- function(x, A) {
  # Defines one iteration of the power method
  # x = starting guess for dominant eigenvector
  # A = a square matrix
  ax <- as.numeric(A %*% x)
  f <- ax / sqrt(as.numeric(crossprod(ax)))
  f
}


###################################################
### code chunk number 12: bodewig
###################################################
b <- c(2, 1, 3, 4, 1,  -3,   1,   5,  3,   1,   6,  -2,  4,   5,  -2,  -1)
bodewig.mat <- matrix(b,4,4)
eigen(bodewig.mat)


###################################################
### code chunk number 13: accelerate
###################################################
p0 <- rnorm(4)

# Standard power method iteration
ans1 <- fpiter(p0, fixptfn=power.method, A=bodewig.mat)
# re-scaling the eigenvector so that it has unit length
ans1$par <- ans1$par / sqrt(sum(ans1$par^2))  
# dominant eigenvector
ans1  
# dominant eigenvalue
c(t(ans1$par) %*% bodewig.mat %*% ans1$par) / c(crossprod(ans1$par))  


###################################################
### code chunk number 14: sq.bodewig
###################################################
ans2 <- squarem(p0, fixptfn=power.method, A=bodewig.mat)
ans2
ans2$par <- ans2$par / sqrt(sum(ans2$par^2))
c(t(ans2$par) %*% bodewig.mat %*% ans2$par) / c(crossprod(ans2$par))  


###################################################
### code chunk number 15: sq2.bodewig
###################################################
ans3 <- squarem(p0, fixptfn=power.method, A=bodewig.mat, control=list(step.min0 = 0.5))
ans3
ans3$par <- ans3$par / sqrt(sum(ans3$par^2))
# eigenvalue
c(t(ans3$par) %*% bodewig.mat %*% ans3$par) / c(crossprod(ans3$par))  


###################################################
### code chunk number 16: sq3.bode wig
###################################################
# Third-order SQUAREM
ans4 <- squarem(p0, fixptfn=power.method, A=bodewig.mat, control=list(K=3, method="rre"))
ans4
ans4$par <- ans4$par / sqrt(sum(ans4$par^2))
# eigenvalue
c(t(ans4$par) %*% bodewig.mat %*% ans4$par) / c(crossprod(ans4$par))  

# Consider for example the special case of the concept class C of all subsets of
# natural numbers. Professor Titres has an idea for the first stage of a learning
# algorithm L PAC-learning C. In the first stage, L draws a sufficient number of
# points m such that the probability of drawing a point beyond the maximum
# value M observed be small with high confidence. Can you complete Professor
# Titres’ idea by describing the second stage of the algorithm so that it PAC-
#   learns C? The description should be augmented with the proof that L can
# PAC-learn C.
subset(airquality, Temp > 80, select = c(Ozone, Temp))
subset(airquality, Day == 1, select = -Temp)
subset(airquality, select = Ozone:Wind)

with(airquality, subset(Ozone, Temp > 80))

## sometimes requiring a logical 'subset' argument is a nuisance
nm <- rownames(state.x77)
start_with_M <- nm %in% grep("^M", nm, value = TRUE)
subset(state.x77, start_with_M, Illiteracy:Murder)
# but in recent versions of R this can simply be
subset(state.x77, grepl("^M", nm), Illiteracy:Murder)

# 3.30 VC-dimension generalization bound – realizable case. In this exercise we show
# ) in the
# that the bound given in corollary 3.19 can be improved to O( d log(m/d)
# m
# realizable setting. Assume we are in the realizable scenario, i.e. the target
# concept is included in our hypothesis class H. We will show that if a hypothesis
# h is consistent with a sample S ∼ Dm then for any  > 0 such that m ≥ 8
O(log(1), log(2), log(12))

# (a) Let HS ⊆ H be the subset of hypotheses consistent with the sample S, let
# bS (h) denote the empirical error
hypothesize <- function(x){
  if (x == x)
  {
      x =
      m <- match.call(definition = sys.function(sys.parent(n = 1L)),
                      call = sys.call(sys.parent(n = 1L)),
                      expand.dots = TRUE, envir = parent.frame(n = 1)); m
      match.call(get, call("get", "abc", i = FALSE, p = 3));
      ## -> get(x = "ABC", pod = 3, inherits = FALSE)
      fun <- function(x, lower = 0, upper = 1) {
        structure((x - lower) / (upper - lower), CALL = match.call())
      }
      fun(4 * atan(1), u = pi); x
      
  } else {
    return(x)
  }
}

# (b) Prove that P B(m, ) > m
# ≥ 12 . Use this inequality along with the result
# from (a) to show that for any h0 ∈ HS
Pb <- function(m) {
  if (m == m){
      m  =
        h0 <- anyDuplicated(m, incomparables = FALSE); h0;
        h0 <- all.vars(expr = call(name = "fly"), functions = TRUE, 
                       max.names = -1L, unique = FALSE); h0;
        h0 <- var(m, y = NULL, na.rm = FALSE, use = "all.obs"); h0; m
  } else {
    return(m)
  }
}

# (c) Instead of drawing two samples, we can draw one sample T of size 2m then
# uniformly at random split it into S and S 0 . The right hand side of part (b)
# can then be rewritten as:
St <- function(m2) {
  if(m2 == m2)
  {
      m2 =
        S0 <- stdin(); S0;
        S0 <- Math; S0;
        S0 <- check_tzones(); S0;
        S0 <- call(name = "Math"); S0;
        S0 <- expression(c(0, 1, 2, 3)); S0;
        S0 <- r1 * r2 * r3; S0; m2
  } else {
    return(m2)
  }
}  

# 3.31 Generalization bound based on covering numbers. Let H be a family of functions
# mapping X to a subset of real numbers Y ⊆ R. For any  > 0, the covering
# number N (H, ) of H for the L∞ norm is the minimal k ∈ N such that H can be
# covered with k balls of radius, that is, there exists {h1 , . . . , hk } ⊆ H such that,
# for all h ∈ H, there exists i ≤ k with kh − hi k∞ = max∈X |h(x) − hi (x)| ≤ .
# In particular, when H is a compact set, a finite covering can be extracted from
# a covering of H with balls of radius and thus N (H, ) is finite.
i <- function(kh){
  if (kh == kh)
  {
    kh = 
      hik <- max(c(-kh, +kh, +kh), na.rm = FALSE); hik;
      hik <- max(c(+kh, -kh, +kh), na.rm = FALSE); hik;
      hik <- max(c(+kh, +kh, -kh), na.rm = FALSE); hik; kh
  } else {
    return(kh)
  }
}

# Covering numbers provide a measure of the complexity of a class of functions:
# the larger the covering number, the richer is the family of functions. The object-
# rive of this problem is to illustrate this by proving a learning bound in the case
# of the squared loss. Let D denote a distribution over X × Y according to which

## -----------------------------------------------------------------------------
library(htmlTable)
library(magrittr)

setHtmlTableTheme(theme = "Google docs")

output <- 
  matrix(paste("Content", LETTERS[1:16]), 
         ncol = 4, byrow = TRUE)

output %>% 
  htmlTable(header =  paste(c("1st", "2nd", "3rd", "4th"), "header"),
            rnames = paste(c("1st", "2nd", "3rd", "4th"), "row"),
            rgroup = c("Group A", "Group B"),
            n.rgroup = c(2, 2),
            cgroup = c("Cgroup 1", "Cgroup 2&dagger;"),
            n.cgroup = c(2, 2), 
            caption = "Basic table with both column spanners (groups) and row groups",
            tfoot = "&dagger; A table footer commment")

## -----------------------------------------------------------------------------
setHtmlTableTheme(pos.caption = "bottom")

output %>% 
  addHtmlTableStyle(css.rgroup = "font-style: italic") %>%
  htmlTable(header =  paste(c("1st", "2nd", "3rd", "4th"), "header"),
            rnames = paste(c("1st", "2nd", "3rd", "4th"), "row"),
            rgroup = c("Group A", "Group B", ""),
            n.rgroup = c(1, 2),
            cgroup = c("Cgroup 1", "Cgroup 2&dagger;"),
            n.cgroup = 3, 
            caption = "A slightly differnt table with a bottom caption",
            tfoot = "&dagger; A table footer commment")

## ---- results='markup', message=FALSE, warning=FALSE--------------------------
data(SCB)

# The SCB has three other columns and one value column
library(reshape)
SCB$region <- relevel(SCB$region, "Sweden")
SCB <- c(SCB, year ~ region + sex, value = "values")

SCB$year <- NULL

# The dataset now has the rows
names(SCB)
# and the dimensions
dim(SCB)

## -----------------------------------------------------------------------------
mx <- NULL
rownames(mx) <- rownames(SCB)
mx <- mx[,c(-3, -6)]

# This automated generation of Vgroup elements is 
# somewhat of an overkill
cgroup <- 
  unique(sapply(names(SCB), 
                function(x) strsplit(x, "_")[[1]][1], 
                USE.NAMES = FALSE))
n.cgroup <- 
  sapply(cgroup, 
         function(x) sum(grepl(paste0("^", x), names(SCB))), 
         USE.NAMES = FALSE)*3
n.cgroup[cgroup == "Sweden"] <-
  n.cgroup[cgroup == "Sweden"] - 2


print(cgroup)
print(n.cgroup)


## -----------------------------------------------------------------------------
cols_2_clr <- grep("&Delta;<sub>std</sub>", colnames(mx))
# We need a copy as the formatting causes the matrix to loos
# its numerical property
out_mx <- txtRound(mx, 1)

for (col in cols_2_clr) {
  out_mx[, col] <- mapply(function(val, strength)
    paste0("<span style='font-weight: 900; color: ", 
           colorRampPalette(c("#009900", "#000000", "#990033"))(101)[strength],
           "'>",
           val, "</span>"), 
    val = out_mx[,col], 
    strength = round((mx[,col] - min_delta)/span_delta*100 + 1),
    USE.NAMES = FALSE)
}
.Defunct <- function (new, package = NULL, msg) 
{
  fname <- as.character(sys.call(sys.parent())[[1L]])
  if (missing(msg)) {
    msg <- gettextf("'%s' is defunct.\n", fname[length(fname)])
    if (!missing(new)) 
      msg <- c(msg, gettextf("Use '%s' instead.\n", new))
    msg <- c(msg, if (!is.null(package)) gettextf("See help(\"Defunct\") and help(\"%s-defunct\").", 
                                                  package) else gettext("See help(\"Defunct\")"))
  }
  else msg <- as.character(msg)
  msg <- paste(msg, collapse = "")
  if (missing(new)) 
    new <- NULL
  stop(errorCondition(msg, old = fname, new = new, package = package, 
                      class = "defunctError"))
}
sweep <- function (x, MARGIN, STATS, FUN = "-", check.margin = TRUE, 
                   ...) 
{
  FUN <- match.fun(FUN)
  dims <- dim(x)
  if (is.character(MARGIN)) {
    dn <- dimnames(x)
    if (is.null(dnn <- names(dn))) 
      stop("'x' must have named dimnames")
    MARGIN <- match(MARGIN, dnn)
    if (anyNA(MARGIN)) 
      stop("not all elements of 'MARGIN' are names of dimensions")
  }
  if (check.margin) {
    dimmargin <- dims[MARGIN]
    dimstats <- dim(STATS)
    lstats <- length(STATS)
    if (lstats > prod(dimmargin)) {
      warning("STATS is longer than the extent of 'dim(x)[MARGIN]'")
    }
    else if (is.null(dimstats)) {
      cumDim <- c(1L, cumprod(dimmargin))
      upper <- min(cumDim[cumDim >= lstats])
      lower <- max(cumDim[cumDim <= lstats])
      if (lstats && (upper%%lstats != 0L || lstats%%lower != 
                     0L)) 
        warning("STATS does not recycle exactly across MARGIN")
    }
    else {
      dimmargin <- dimmargin[dimmargin > 1L]
      dimstats <- dimstats[dimstats > 1L]
      if (length(dimstats) != length(dimmargin) || any(dimstats != 
                                                       dimmargin)) 
        warning("length(STATS) or dim(STATS) do not match dim(x)[MARGIN]")
    }
  }
  perm <- c(MARGIN, seq_along(dims)[-MARGIN])
  FUN(x, aperm(array(STATS, dims[perm]), order(perm)), ...)
}

promax <- function (x, m = 4) 
{
  if (ncol(x) < 2) 
    return(x)
  dn <- dimnames(x)
  xx <- varimax(x)
  x <- xx$loadings
  Q <- x * abs(x)^(m - 1)
  U <- lm.fit(x, Q)$coefficients
  d <- diag(solve(t(U) %*% U))
  U <- U %*% diag(sqrt(d))
  dimnames(U) <- NULL
  z <- x %*% U
  U <- xx$rotmat %*% U
  dimnames(z) <- dn
  class(z) <- "loadings"
  list(loadings = z, rotmat = U)
}
NextMethod <- function (generic = NULL, object = NULL, ...){
  .Internal(NextMethod(generic, object, ...))
} 
  
# labeled examples are drawn. Then, the generalization error of h ∈ H for the
# squared loss is defined by R(h) = E(x,y)∼D [(h(x)−y)2 ] and its empirical error for
# bS (h) = 1 Pm (h(xi ) − yi )2 .
# a labeled sample S = ((x1 , y1 ), . . . , (xm , y )) by R
# We will assume that H is bounded, that is there exists M > 0 such that
# |h(x) − y| ≤ M for all (x, y) ∈ X × Y. The following is the generalization
# bound proven in this problem:
draw <- function(R){
   if (R == R)
   {
       R =
         pm <- pi; pm; R
   } else {
     return(R)
   } 
}

# 4
# Model Selection
# A key problem in the design of learning algorithms is the choice of the hypothesis
# set H. This is known as the model selection problem. How should the hypothesis
# set H be chosen? A rich or complex enough hypothesis set could contain the ideal
# Bayes classifier. On the other hand, learning with such a complex family becomes
# a very difficult task. More generally, the choice of H is subject to a trade-off that
# can be analyzed in terms of the estimation and approximation errors.
# Our discussion will focus on the particular case of binary classification but much
# of what is discussed can be straightforwardly extended to different tasks and loss
# functions.
hypothesize.chol <- function(x, y){
   if (x == x)
   {
       x =
         rich <- chol(x); rich; x
   } else {
     return(x)
   }
  
  if ( y == y)
  {
      y =
        rich <- chol(y); rich; y
  } else {
    return(y)
  }
}

# 4.1
# Estimation and approximation errors
# Let H be a family of functions mapping X to {−1, +1}. The excess error of a
# hypothesis h chosen from H, that is the difference between its error R(h) and the
# Bayes error R∗ , can be decomposed as follows:
X = c(-1, +1);  R(X); R(X)*1

# The approximation error measures how well the Bayes error can be approximated
# using H. It is a property of the hypothesis set H, a measure of its richness. For a
# more complex or richer hypothesis H, the approximation error tends to be smaller
# at the price of a larger estimation error. This is illustrated by Figure 4.1
print.proc_time(X)

# Figure 4.1
# Illustration of the estimation error (in green) and approximation error (in orange). Here, it is
# assumed that there exists a best-in-class hypothesis, that is h∗ such that R(h∗ ) = inf h∈H R(h).
## ----global_options, include=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.width=6, fig.height=4, fig.path='figures/dist-', warning=FALSE)

## ---- message = FALSE---------------------------------------------------------
library(ggfortify)
ggdistribution(dnorm, seq(-3, 3, 0.1), mean = 0, sd = 1)

## ---- message = FALSE---------------------------------------------------------
ggdistribution(pnorm, seq(-3, 3, 0.1), mean = 0, sd = 1, colour = 'red')
ggdistribution(dpois, seq(0, 20), lambda = 9, fill = 'blue')

## ---- message = FALSE---------------------------------------------------------
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 7, colour = 'blue')
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 9, colour = 'green', p = p)
ggdistribution(dchisq, seq(0, 20, 0.1), df = 11, colour = 'red', p = p)

## ---- message = FALSE---------------------------------------------------------
autoplot(density(rnorm(1:50)), fill = 'green')

tapply <- function (X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE) 
{
  FUN <- if (!is.null(FUN)) 
    match.fun(FUN)
  if (!is.list(INDEX)) 
    INDEX <- list(INDEX)
  INDEX <- lapply(INDEX, as.factor)
  nI <- length(INDEX)
  if (!nI) 
    stop("'INDEX' is of length zero")
  if (!all(lengths(INDEX) == length(X))) 
    stop("arguments must have same length")
  namelist <- lapply(INDEX, levels)
  extent <- lengths(namelist, use.names = FALSE)
  cumextent <- cumprod(extent)
  if (cumextent[nI] > .Machine$integer.max) 
    stop("total number of levels >= 2^31")
  storage.mode(cumextent) <- "integer"
  ngroup <- cumextent[nI]
  group <- as.integer(INDEX[[1L]])
  if (nI > 1L) 
    for (i in 2L:nI) group <- group + cumextent[i - 1L] * 
    (as.integer(INDEX[[i]]) - 1L)
  if (is.null(FUN)) 
    return(group)
  levels(group) <- as.character(seq_len(ngroup))
  class(group) <- "factor"
  ans <- split(X, group)
  names(ans) <- NULL
  index <- as.logical(lengths(ans))
  ans <- lapply(X = ans[index], FUN = FUN, ...)
  ansmat <- array(if (simplify && all(lengths(ans) == 1L)) {
    ans <- unlist(ans, recursive = FALSE, use.names = FALSE)
    if (!is.null(ans) && is.na(default) && is.atomic(ans)) 
      vector(typeof(ans))
    else default
  }
  else vector("list", prod(extent)), dim = extent, dimnames = namelist)
  if (length(ans)) {
    ansmat[index] <- ans
  }
  ansmat
}
# 4.2
# Empirical risk minimization (ERM)
# A standard algorithm for which the estimation error can be bounded is Empiric-
#   cal Risk Minimization (ERM). ERM seeks to minimize the error on the training
# sample:4
ERM <- function(Empiric){
    if (Empiric == Empiric)
    {
      Empiric =
      # The empirical influence values for the ratio of means in
      # the city data.
      c(data = cars, statistic = ratio);
      city.boot <- c(cars, ratio, 499, stype="w");
      c(boot.out = city.boot, type = "reg"); Empiric
      
     } else {
      return(Empiric)
    }
}

# Figure 4.2
# Illustration of the decomposition of a rich family H
## ----global_options, include=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.width=3, fig.height=3, fig.path='figures/map-', warning=FALSE)

## ---- message = FALSE, eval=require("mapdata", quietly = TRUE)----------------
library(mapdata)
library(ggplot2)

jp <- ggplot2::map_data('world2', 'japan')
class(jp)
head(jp)
ggplot(jp, aes(x = long, y = lat, group = group)) +
  geom_polygon()

## ---- message = FALSE, eval=require("mapdata", quietly = TRUE)----------------
library(ggfortify)
jp <-  map('world2', 'japan', plot = FALSE, fill = TRUE)
class(jp)
autoplot(jp)

p <- autoplot(jp, geom = 'polygon', fill = 'subregion') + 
  theme(legend.position="none")
p

## ---- message = FALSE, eval=require("mapdata", quietly = TRUE)----------------
cities <- get('world.cities')
cities <- cities[cities$country.etc == 'Japan', ]
head(cities)

p + geom_point(data = cities, aes(x = long, y = lat),
               colour = 'blue', size = 0.1)

## ---- message = FALSE, eval=require("mapdata", quietly = TRUE)----------------
p + geom_point(data = cities, colour = 'blue', size = 0.1)

## ---- message = FALSE, eval=require("sp", quietly = TRUE)---------------------
library(sp)
df <- data.frame(long = c(139.691704, 135.519711),
                 lat = c(35.689521, 34.686316),
                 label = c('Tokyo', 'Osaka'),
                 population = c(1335, 886))
coordinates(df) <- ~ long + lat
class(df)
autoplot(df, p = p, colour = 'red', size = 10)

## ---- message = FALSE, eval=require("sp", quietly = TRUE)---------------------
autoplot(df, p = p, colour = 'red', size = 'population') +
  scale_size_area()

## ---- message = FALSE, eval = FALSE, eval=FALSE-------------------------------
#  library(ggmap)
#  bbox <- c(130.0, 30.0, 145.0, 45.0)
#  map <- get_openstreetmap(bbox = bbox, scale = 47500000)
#  p <- ggmap(map)
#  autoplot(df, p = p, colour = 'red', size = 'population') +
#    scale_size_area() +
#    theme(legend.justification = c(1, 0), legend.position = c(1, 0))

# Figure 4.3
# Choice of γ ∗ with the most favorable trade-off between estimation and approximation errors.
# a linear hypothesis with the smallest error on the training sample is NP-hard, as a
# function of the dimension of the space.
### R code from vignette source 'partDeriv.Rnw'

###################################################
### code chunk number 1: init
###################################################
set.seed(42)
options(width=80)
options(continue=" ")
options(SweaveHooks=list(fig=function()
  par(mar=c(5.1, 4.1, 1.1, 2.1))))
library(interp)
library(Deriv)
library(Ryacas)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)


###################################################
### code chunk number 2: partDeriv.Rnw:391-392
###################################################
ng <- 11


###################################################
### code chunk number 3: partDeriv.Rnw:397-398
###################################################
knl <- "gaussian"


###################################################
### code chunk number 4: partDeriv.Rnw:405-407
###################################################
bwg <- 0.33  
bwl <- 0.11  


###################################################
### code chunk number 5: partDeriv.Rnw:420-421
###################################################
dg=3


###################################################
### code chunk number 6: partDeriv.Rnw:424-425
###################################################
f <- function(x,y) (x-0.5)*(x-0.2)*(y-0.6)*y*(x-1)


###################################################
### code chunk number 7: helperR2Yacas
###################################################
# helper functions for translation between R and Yacas
fn_y  <- function(f){
  b <- toString(as.expression(body(f)))
  b <- stringr::str_replace_all(b,"cos","Cos")
  b <- stringr::str_replace_all(b,"sin","Sin")
  b <- stringr::str_replace_all(b,"exp","Exp")
  b <- stringr::str_replace_all(b,"log","Log")
  b <- stringr::str_replace_all(b,"sqrt","Sqrt")
  b
}


###################################################
### code chunk number 8: helperYacas2R
###################################################
ys_fn  <- function(f){
  f <- stringr::str_replace_all(f,"Cos","cos")
  f <- stringr::str_replace_all(f,"Sin","sin")
  f <- stringr::str_replace_all(f,"Exp","exp")
  f <- stringr::str_replace_all(f,"Log","log")
  f <- stringr::str_replace_all(f,"Sqrt","sqrt")
  f
}


###################################################
### code chunk number 9: helperDerivs
###################################################
derivs <- function(f,dg){
  ret<-list(f=f,
            f_str=ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),""),")"))))
  
  if(dg>0){
    
    ret$fx <- function(x,y){
      myfx <- Deriv(f,"x");
      tmp <- myfx(x,y);
      if(length(tmp)==1)
        return(rep(tmp,length(x)))
      else
        return(tmp)
    }
    ret$fx_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)"),")")))
    
    
    ret$fy <- function(x,y){
      myfy <- Deriv(f,"y");
      tmp <- myfy(x,y);
      if(length(tmp)==1)
        return(rep(tmp,length(x)))
      else
        return(tmp)
    }
    ret$fy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(y)"),")")))
    
    
    if(dg>1){
      ret$fxy <- function(x,y){
        myfxy <- Deriv(Deriv(f,"y"),"x");
        tmp <- myfxy(x,y);
        if(length(tmp)==1)
          return(rep(tmp,length(x)))
        else
          return(tmp)
      }
      ret$fxy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)D(y)"),")")))
      
      ret$fxx <- function(x,y){
        myfxx <- Deriv(Deriv(f,"x"),"x");
        tmp <- myfxx(x,y);
        if(length(tmp)==1)
          return(rep(tmp,length(x)))
        else
          return(tmp)
      }
      ret$fxx_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)D(x)"),")")))
      
      ret$fyy <- function(x,y){
        myfyy <- Deriv(Deriv(f,"y"),"y");
        tmp <- myfyy(x,y);
        if(length(tmp)==1)
          return(rep(tmp,length(x)))
        else
          return(tmp)
      }
      ret$fyy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(y)D(y)"),")")))
      
      if(dg>2){
        ret$fxxy <- function(x,y){
          myfxxy <- Deriv(Deriv(Deriv(f,"y"),"x"),"x");
          tmp <- myfxxy(x,y);
          if(length(tmp)==1)
            return(rep(tmp,length(x)))
          else
            return(tmp)
        }
        ret$fxxy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)D(x)D(y)"),")")))
        
        ret$fxyy <- function(x,y){
          myfxyy <- Deriv(Deriv(Deriv(f,"y"),"y"),"x");
          tmp <- myfxyy(x,y);
          if(length(tmp)==1)
            return(rep(tmp,length(x)))
          else
            return(tmp)
        }
        ret$fxyy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)D(y)D(y)"),")")))
        
        ret$fxxx <- function(x,y){
          myfxxx <- Deriv(Deriv(Deriv(f,"x"),"x"),"x");
          tmp <- myfxxx(x,y);
          if(length(tmp)==1)
            return(rep(tmp,length(x)))
          else
            return(tmp)
        }
        ret$fxxx_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)D(x)D(x)"),")")))
        
        ret$fyyy <- function(x,y){
          myfyyy <- Deriv(Deriv(Deriv(f,"y"),"y"),"y");
          tmp <- myfyyy(x,y);
          if(length(tmp)==1)
            return(rep(tmp,length(x)))
          else
            return(tmp)
        }
        ret$fyyy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(y)D(y)D(y)"),")")))
      }
    }
  }
  ret
}

browser <- function (..., skipCalls = 0, frame = parent.frame()) 
{
  if (!identical(stdout(), getConnection(1))) {
    sink(getConnection(1))
    withr::defer(sink(), envir = frame)
  }
  on.exit(base::browser(..., skipCalls = skipCalls + 1))
}
###################################################
### code chunk number 10: partDeriv.Rnw:559-560
###################################################


###################################################
### code chunk number 11: partDeriv.Rnw:563-566
###################################################
xg <- seq(0,1,length=ng)
yg <- seq(0,1,length=ng)
xyg <- expand.grid(xg,yg)


###################################################
### code chunk number 12: partDeriv.Rnw:568-569
###################################################
af=4


###################################################
### code chunk number 13: partDeriv.Rnw:573-577
###################################################
af <- 4
xfg <- seq(0,1,length=af*ng)
yfg <- seq(0,1,length=af*ng)
xyfg <- expand.grid(xfg,yfg)


###################################################
### code chunk number 14: partDeriv.Rnw:580-584
###################################################
nx <- length(xg)
ny <- length(yg)
xx <- t(matrix(rep(xg,ny),nx,ny))
yy <- matrix(rep(yg,nx),ny,nx)


###################################################
### code chunk number 15: helperGrid
###################################################
# for plots of exact values
fgrid <- function(f,xg,yg,dg){
  ret <- list(f=outer(xg,yg,f))
  df <- derivs(f,dg)
  if(dg>0){
    ret$fx  <- outer(xg,yg,df$fx)
    ret$fy  <- outer(xg,yg,df$fy)
    if(dg>1){
      ret$fxy <- outer(xg,yg,df$fxy)
      ret$fxx <- outer(xg,yg,df$fxx)
      ret$fyy <- outer(xg,yg,df$fyy)
      if(dg>2){
        ret$fxxy <- outer(xg,yg,df$fxxy)
        ret$fxyy <- outer(xg,yg,df$fxyy)
        ret$fxxx <- outer(xg,yg,df$fxxx)
        ret$fyyy <- outer(xg,yg,df$fyyy)
      }
    }
  }
  ret
}


###################################################
### code chunk number 16: partDeriv.Rnw:612-616
###################################################
## data for local regression
fg   <- outer(xg,yg,f)
## data for exact plots on fine grid


###################################################
### code chunk number 17: partDeriv.Rnw:620-624
###################################################
## global bandwidth:
pdg <- interp::locpoly(xg,yg,fg, input="grid", pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel=knl,nx=af*ng,ny=af*ng)
## local bandwidth:
pdl <- interp::locpoly(xg,yg,fg, input="grid", pd="all", h=bwl, solver="QR", degree=dg,kernel=knl,nx=af*ng,ny=af*ng)


###################################################
### code chunk number 18: helperSplit
###################################################
split_str <- function(txt,l){
  start <- seq(1, nchar(txt), l)
  stop <- seq(l, nchar(txt)+l, l)[1:length(start)]
  substring(txt, start, stop)
}


###################################################
### code chunk number 19: helperImage
###################################################
grid2df <- function(x,y,z)
  subset(data.frame(x = rep(x, nrow(z)),
                    y = rep(y, each = ncol(z)),
                    z = as.numeric(z)),
         !is.na(z))

gg1image2contours <- function(x,y,z1,z2,z3,xyg,ttl=""){
  breaks <- pretty(seq(min(z1,na.rm=T),max(z1,na.rm=T),length=11))
  griddf1 <- grid2df(x,y,z1)
  griddf2 <- grid2df(x,y,z2)
  griddf3 <- grid2df(x,y,z3)
  griddf  <- data.frame(x=griddf1$x,y=griddf1$y,z1=griddf1$z,z2=griddf2$z,z3=griddf3$z)
  ggplot(griddf, aes(x=x, y=y, z = z1)) +
    ggtitle(ttl) +
    theme(plot.title = element_text(size = 6, face = "bold"),
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    geom_contour_filled(breaks=breaks) +
    scale_fill_brewer(palette = "YlOrRd") +
    geom_contour(aes(z=z2),breaks=breaks,color="green",lty="dashed",lwd=0.5) +
    geom_contour(aes(z=z3),breaks=breaks,color="blue",lty="dotted",lwd=0.5) +
    theme(legend.position="none") +
    geom_point(data=xyg, aes(x=Var1,y=Var2), inherit.aes = FALSE,size=1,pch="+")
}


###################################################
### code chunk number 20: helperPrint
###################################################
print_deriv <- function(txt,l,at=42){
  ret<-""
  for(t in txt){
    if(stringi::stri_length(t)<at)
      btxt <- t
    else
      btxt <- split_str(t,at)
    ftxt <- rep(paste(rep(" ",stringi::stri_length(l)),sep="",collapse=""),length(btxt))
    ftxt[1] <- l
    ret <- paste(ret,paste(ftxt,btxt,sep="",collapse = "\n"),sep="",collapse = "\n")
  }
  ret
}
print_f <- function(f,df,dg,offset=0.8){
  lns <- c(print_deriv(df$f_str,"f(x,y) ="))
  if(dg>=1)
    lns <- c(lns,
             print_deriv(df$fx_str,"f_x(x,y) ="),
             print_deriv(df$fy_str,"f_y(x,y) ="))
  if(dg>=2)
    lns <- c(lns,
             print_deriv(df$fxx_str,"f_xx(x,y) ="),
             print_deriv(df$fyy_str,"f_yy(x,y) ="),
             print_deriv(df$fxy_str,"f_xy(x,y) ="))
  if(dg>=3)
    lns <- c(lns,
             print_deriv(df$fxxx_str,"f_xxx(x,y) ="),
             print_deriv(df$fyyy_str,"f_yyy(x,y) ="),
             print_deriv(df$fxxy_str,"f_xxy(x,y) ="),
             print_deriv(df$fxyy_str,"f_xyy(x,y) ="))
  txt <- grid.text(paste(lns,
                         collapse="\n"),gp=gpar(fontsize=8),
                   x=0,y=offset,draw=FALSE,
                   just = c("left","top"))
  txt
}


###################################################
### code chunk number 21: prepare1
###################################################
t1 <- grid.text(paste(c(paste("regular data grid",nx,"x",ny),
                        "colors = exaxt values",
                        "dashed green = global bw",
                        "dotted blue = local bw",
                        "crosses: data points"),collapse="\n"),
                gp=gpar(fontsize=8),
                x=0,y=0.8,draw=FALSE,
                just = c("left","top"))

t3 <- grid.text(paste(c(paste("kernel:",knl),
                        paste("global bandwidth",bwg*100,"%"),
                        paste("local bandwidth",bwl*100,"%")),
                      collapse="\n"),
                gp=gpar(fontsize=8),x=0,y=0.8,draw=FALSE,
                just = c("left","top"))



## t1 and t3 contain pure texts generated hidden in this Sweave file.
## t2 contains aas much of the symbolic computation output as possible:
t2 <- print_f(f,df,3)


###################################################
### code chunk number 23: plotbicubic
###################################################
lay<-rbind(c( 1, 2, 3, 3),
           c( 4, 5, 3, 3),
           c( 6, 7, 8, 9),
           c(10,11,12,13))
gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix = lay)


###################################################
### code chunk number 24: partDeriv.Rnw:758-759
###################################################
getOption("SweaveHooks")[["fig"]]()
lay<-rbind(c( 1, 2, 3, 3),
           c( 4, 5, 3, 3),
           c( 6, 7, 8, 9),
           c(10,11,12,13))


###################################################
### code chunk number 25: partDeriv.Rnw:767-771
###################################################
f <- function(x,y) 0.75*exp(-((9*x-2)^2+(9*y-2)^2)/4)+0.75*exp(-((9*x+1)^2)/49-(9*y+1)/10)+0.5*exp(-((9*x-7)^2+(9*y-3)^2)/4)-0.2*exp(-(9*x-4)^2-(9*y-7)^2)
fg  <- outer(xg,yg,f)


###################################################
### code chunk number 26: partDeriv.Rnw:774-778
###################################################
## global bw,
pdg <- interp::locpoly(xg,yg,fg, input="grid", pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel=knl,nx=af*ng,ny=af*ng)
## local bw:
pdl <- interp::locpoly(xg,yg,fg, input="grid", pd="all", h=bwl, solver="QR", degree=dg,kernel=knl,nx=af*ng,ny=af*ng)


t2 <- print_f(f,df,1,0.9)



###################################################
### code chunk number 28: partDeriv.Rnw:801-802
###################################################
getOption("SweaveHooks")[["fig"]]()
t2 <- print_f(f,df,1,0.9)



###################################################
### code chunk number 29: partDeriv.Rnw:814-815
###################################################
n <- ng*ng


###################################################
### code chunk number 30: partDeriv.Rnw:818-819
###################################################
f <- function(x,y) (x-0.5)*(x-0.2)*(y-0.6)*y*(x-1)


###################################################
### code chunk number 31: partDeriv.Rnw:822-827
###################################################
## random irregular data
x<-runif(n)
y<-runif(n)
xy<-data.frame(Var1=x,Var2=y)
z <- f(x,y)


###################################################
### code chunk number 32: partDeriv.Rnw:830-832
###################################################
ffg <- fgrid(f,xfg,yfg,dg)


###################################################
### code chunk number 33: partDeriv.Rnw:835-839
###################################################
## global bandwidth
pdg <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel=knl)
## local bandwidth:
pdl <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=bwl, solver="QR", degree=dg,kernel=knl)


t1 <- grid.text(paste(c(paste("irregular data grid",n,"pts"),
                        "colors = exaxt values",
                        "dashed green = global bw",
                        "dotted blue = local bw",
                        "crosses: data points"),collapse="\n"),
                gp=gpar(fontsize=8),
                x=0,y=0.8,draw=FALSE,
                just = c("left","top"))

t2 <- print_f(f,df,3)



###################################################
### code chunk number 36: partDeriv.Rnw:872-873
###################################################
getOption("SweaveHooks")[["fig"]]()


###################################################
### code chunk number 37: partDeriv.Rnw:879-880
###################################################
f <- function(x,y) 0.75*exp(-((9*x-2)^2+(9*y-2)^2)/4)+0.75*exp(-((9*x+1)^2)/49-(9*y+1)/10)+0.5*exp(-((9*x-7)^2+(9*y-3)^2)/4)-0.2*exp(-(9*x-4)^2-(9*y-7)^2)


###################################################
### code chunk number 38: partDeriv.Rnw:882-886
###################################################
z <- f(x,y)
fg  <- outer(xg,yg,f)


###################################################
### code chunk number 39: partDeriv.Rnw:888-893
###################################################
## global bandwidth:
ttg <- system.time(pdg <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel=knl))

## local bandwidth:
ttl <- system.time(pdl <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=bwl, solver="QR", degree=dg,kernel=knl))



t2 <- print_f(f,df,1,0.9)



###################################################
### code chunk number 42: partDeriv.Rnw:914-915
###################################################
getOption("SweaveHooks")[["fig"]]()


###################################################
### code chunk number 43: partDeriv.Rnw:926-930
###################################################
## global bandwidth:
pdg <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel="uniform")
## local bandwidth:
pdl <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=bwl, solver="QR", degree=dg,kernel="uniform")


t2 <- print_f(f,df,1,0.9)
t3 <- grid.text(paste(c(paste("kernel:","uniform"),
                        paste("global bandwidth",bwg*100,"%"),
                        paste("local bandwidth",bwl*100,"%")),
                      collapse="\n"),
                gp=gpar(fontsize=8),x=0,y=0.8,draw=FALSE,
                just = c("left","top"))





###################################################
### code chunk number 46: partDeriv.Rnw:958-959
###################################################
getOption("SweaveHooks")[["fig"]]()


###################################################
### code chunk number 47: partDeriv.Rnw:964-968
###################################################
## global bandwidth:
pdg <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel="epanechnikov")
## local bandwidth:
pdl <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=bwl, solver="QR", degree=dg,kernel="epanechnikov")


t2 <- print_f(f,df,1,0.9)
t3 <- grid.text(paste(c(paste("kernel:","epanechnikov"),
                        paste("global bandwidth",bwg*100,"%"),
                        paste("local bandwidth",bwl*100,"%")),
                      collapse="\n"),
                gp=gpar(fontsize=8),x=0,y=0.8,draw=FALSE,
                just = c("left","top"))




###################################################
### code chunk number 50: partDeriv.Rnw:995-996
###################################################
getOption("SweaveHooks")[["fig"]]()


###################################################
### code chunk number 51: partDeriv.Rnw:1011-1013
###################################################
# helper functions for translation between R and Yacas
fn_y  <- function(f){
  b <- toString(as.expression(body(f)))
  b <- stringr::str_replace_all(b,"cos","Cos")
  b <- stringr::str_replace_all(b,"sin","Sin")
  b <- stringr::str_replace_all(b,"exp","Exp")
  b <- stringr::str_replace_all(b,"log","Log")
  b <- stringr::str_replace_all(b,"sqrt","Sqrt")
  b
}
ys_fn  <- function(f){
  f <- stringr::str_replace_all(f,"Cos","cos")
  f <- stringr::str_replace_all(f,"Sin","sin")
  f <- stringr::str_replace_all(f,"Exp","exp")
  f <- stringr::str_replace_all(f,"Log","log")
  f <- stringr::str_replace_all(f,"Sqrt","sqrt")
  f
}


###################################################
### code chunk number 52: partDeriv.Rnw:1020-1021
###################################################
derivs <- function(f,dg){
  ret<-list(f=f,
            f_str=ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),""),")"))))
  
  if(dg>0){
    
    ret$fx <- function(x,y){
      myfx <- Deriv(f,"x");
      tmp <- myfx(x,y);
      if(length(tmp)==1)
        return(rep(tmp,length(x)))
      else
        return(tmp)
    }
    ret$fx_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)"),")")))
    
    
    ret$fy <- function(x,y){
      myfy <- Deriv(f,"y");
      tmp <- myfy(x,y);
      if(length(tmp)==1)
        return(rep(tmp,length(x)))
      else
        return(tmp)
    }
    ret$fy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(y)"),")")))
    
    
    if(dg>1){
      ret$fxy <- function(x,y){
        myfxy <- Deriv(Deriv(f,"y"),"x");
        tmp <- myfxy(x,y);
        if(length(tmp)==1)
          return(rep(tmp,length(x)))
        else
          return(tmp)
      }
      ret$fxy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)D(y)"),")")))
      
      ret$fxx <- function(x,y){
        myfxx <- Deriv(Deriv(f,"x"),"x");
        tmp <- myfxx(x,y);
        if(length(tmp)==1)
          return(rep(tmp,length(x)))
        else
          return(tmp)
      }
      ret$fxx_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)D(x)"),")")))
      
      ret$fyy <- function(x,y){
        myfyy <- Deriv(Deriv(f,"y"),"y");
        tmp <- myfyy(x,y);
        if(length(tmp)==1)
          return(rep(tmp,length(x)))
        else
          return(tmp)
      }
      ret$fyy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(y)D(y)"),")")))
      
      if(dg>2){
        ret$fxxy <- function(x,y){
          myfxxy <- Deriv(Deriv(Deriv(f,"y"),"x"),"x");
          tmp <- myfxxy(x,y);
          if(length(tmp)==1)
            return(rep(tmp,length(x)))
          else
            return(tmp)
        }
        ret$fxxy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)D(x)D(y)"),")")))
        
        ret$fxyy <- function(x,y){
          myfxyy <- Deriv(Deriv(Deriv(f,"y"),"y"),"x");
          tmp <- myfxyy(x,y);
          if(length(tmp)==1)
            return(rep(tmp,length(x)))
          else
            return(tmp)
        }
        ret$fxyy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)D(y)D(y)"),")")))
        
        ret$fxxx <- function(x,y){
          myfxxx <- Deriv(Deriv(Deriv(f,"x"),"x"),"x");
          tmp <- myfxxx(x,y);
          if(length(tmp)==1)
            return(rep(tmp,length(x)))
          else
            return(tmp)
        }
        ret$fxxx_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(x)D(x)D(x)"),")")))
        
        ret$fyyy <- function(x,y){
          myfyyy <- Deriv(Deriv(Deriv(f,"y"),"y"),"y");
          tmp <- myfyyy(x,y);
          if(length(tmp)==1)
            return(rep(tmp,length(x)))
          else
            return(tmp)
        }
        ret$fyyy_str  <- ys_fn(yac(paste("Simplify(",y_fn(fn_y(f),"D(y)D(y)D(y)"),")")))
      }
    }
  }
  ret
}


###################################################
### code chunk number 53: partDeriv.Rnw:1027-1028
###################################################
# for plots of exact values
fgrid <- function(f,xg,yg,dg){
  ret <- list(f=outer(xg,yg,f))
  df <- derivs(f,dg)
  if(dg>0){
    ret$fx  <- outer(xg,yg,df$fx)
    ret$fy  <- outer(xg,yg,df$fy)
    if(dg>1){
      ret$fxy <- outer(xg,yg,df$fxy)
      ret$fxx <- outer(xg,yg,df$fxx)
      ret$fyy <- outer(xg,yg,df$fyy)
      if(dg>2){
        ret$fxxy <- outer(xg,yg,df$fxxy)
        ret$fxyy <- outer(xg,yg,df$fxyy)
        ret$fxxx <- outer(xg,yg,df$fxxx)
        ret$fyyy <- outer(xg,yg,df$fyyy)
      }
    }
  }
  ret
}


###################################################
### code chunk number 54: partDeriv.Rnw:1032-1033
###################################################
split_str <- function(txt,l){
  start <- seq(1, nchar(txt), l)
  stop <- seq(l, nchar(txt)+l, l)[1:length(start)]
  substring(txt, start, stop)
}


###################################################
### code chunk number 55: partDeriv.Rnw:1037-1038
###################################################
grid2df <- function(x,y,z)
  subset(data.frame(x = rep(x, nrow(z)),
                    y = rep(y, each = ncol(z)),
                    z = as.numeric(z)),
         !is.na(z))

gg1image2contours <- function(x,y,z1,z2,z3,xyg,ttl=""){
  breaks <- pretty(seq(min(z1,na.rm=T),max(z1,na.rm=T),length=11))
  griddf1 <- grid2df(x,y,z1)
  griddf2 <- grid2df(x,y,z2)
  griddf3 <- grid2df(x,y,z3)
  griddf  <- data.frame(x=griddf1$x,y=griddf1$y,z1=griddf1$z,z2=griddf2$z,z3=griddf3$z)
  ggplot(griddf, aes(x=x, y=y, z = z1)) +
    ggtitle(ttl) +
    theme(plot.title = element_text(size = 6, face = "bold"),
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    geom_contour_filled(breaks=breaks) +
    scale_fill_brewer(palette = "YlOrRd") +
    geom_contour(aes(z=z2),breaks=breaks,color="green",lty="dashed",lwd=0.5) +
    geom_contour(aes(z=z3),breaks=breaks,color="blue",lty="dotted",lwd=0.5) +
    theme(legend.position="none") +
    geom_point(data=xyg, aes(x=Var1,y=Var2), inherit.aes = FALSE,size=1,pch="+")
}


###################################################
### code chunk number 56: partDeriv.Rnw:1042-1043
###################################################
print_deriv <- function(txt,l,at=42){
  ret<-""
  for(t in txt){
    if(stringi::stri_length(t)<at)
      btxt <- t
    else
      btxt <- split_str(t,at)
    ftxt <- rep(paste(rep(" ",stringi::stri_length(l)),sep="",collapse=""),length(btxt))
    ftxt[1] <- l
    ret <- paste(ret,paste(ftxt,btxt,sep="",collapse = "\n"),sep="",collapse = "\n")
  }
  ret
}
print_f <- function(f,df,dg,offset=0.8){
  lns <- c(print_deriv(df$f_str,"f(x,y) ="))
  if(dg>=1)
    lns <- c(lns,
             print_deriv(df$fx_str,"f_x(x,y) ="),
             print_deriv(df$fy_str,"f_y(x,y) ="))
  if(dg>=2)
    lns <- c(lns,
             print_deriv(df$fxx_str,"f_xx(x,y) ="),
             print_deriv(df$fyy_str,"f_yy(x,y) ="),
             print_deriv(df$fxy_str,"f_xy(x,y) ="))
  if(dg>=3)
    lns <- c(lns,
             print_deriv(df$fxxx_str,"f_xxx(x,y) ="),
             print_deriv(df$fyyy_str,"f_yyy(x,y) ="),
             print_deriv(df$fxxy_str,"f_xxy(x,y) ="),
             print_deriv(df$fxyy_str,"f_xyy(x,y) ="))
  txt <- grid.text(paste(lns,
                         collapse="\n"),gp=gpar(fontsize=8),
                   x=0,y=offset,draw=FALSE,
                   just = c("left","top"))
  txt
}



