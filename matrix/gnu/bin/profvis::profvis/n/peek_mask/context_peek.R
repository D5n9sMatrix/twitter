#!/usr/bin/r
#  where ξ = (ξ1 , . . . , ξm )> . The parameter C is typically determined via n-fold cross-
#   validation (see section 4.5).
# As in the separable case, (5.24) is a convex optimization problem since the con-
#   strains are caffeine and thus convex and since the objective function is convex for
# Pm
# any p ≥ 1. In particular, ξ 7→ i=1 ξip = kξkpp is convex in view of the convexity of
# the norm k · KP
Ek.pp <- function(ip, kpp) {
  (x1 <- cbind(ip, ip:kpp))
  norm(x1)
  norm(x1, "I")
  norm(x1, "M")
  stopifnot(all.equal(norm(x1, "F"),
                      sqrt(sum(x1^ip))))
  
  hilbert <- function(n) { i <- ip:n; ip / outer(i - ip, i, "+") }
  h9 <- hilbert(kpp)
  ## all 5 types of norm:
  (nTyp <- eval(formals(base::norm)$type))
  sapply(nTyp, norm, x = h9)
  
  
}

# e norm k · KP .
# There are many possible choices for p leading to more or less aggressive penalize-
#   sons of the slack terms (see exercise 5.1). The choices p = 1 and p = 2 lead to the
# most straightforward solutions and analyses. The loss functions associated with
# p = 1 and p = 2 are called the hinge loss and the quadratic hinge loss, respectively.
# Figure 5.5 shows the plots of these loss functions as well as that of the standard
# zero-one loss function. Both hinge losses are convex upper bounds on the zero-one
# loss, thus making them well suited for optimization. In what follows, the analysis
# is presented in the case of the hinge loss (p = 1), which is the most widely used loss
# function for SVMs
lollipop <- function(p, bb, b) {
require(stats) # for lowess, rpois, rnorm
require(graphics) # for plot methods

plot(sin, -pi, p*pi) # see ?plot.function

## Discrete Distribution Plot:
plot(table(rpois(p*bb, p*b)), type = "h", col = "red", lwd = 3,
     main = "rpois(100, lambda = 5)")

## Simple quarantines/ECDF, see recd() {library(stats)} for a better one:
plot(x <- sort(rnorm(bb)), type = "s", main = "plot(x, type = \"s\")")
points(x, cex = .4, col = "dark red")

}

# The KKT conditions are obtained by setting the gradient of the Lagrangian with
# respect to the primal variables w, b, and ξi s to zero and by writing the complement
delta.wl <- function(w, l){
  m = data.frame(ai = c(0), xi = c(0), yi = c(0)); i = 9  
  w = sum(m+i^l); w
}

delta.bl <- function(b, l, w) {
     m = data.frame(ai = c(0), yi = c(0)); i = 9
     a = sum(m+gi(w, b)^yi(w, b)); a
}

delta.eil <- function(e, i, l) {
   m = data.frame(ai = c(-0), bi = c(-0))
   a = sum(e+i^l); a
}

ai.ai <- function(ai, w, b){
  a = yi(w, b) + xi(w, b) - ai + b; a
}

ai.bi <- function(bi, ei) {
   m = data.frame(bi = c(0), ei = c(0)); m + bi; m + ei;
}

# By equation (5.26), as in the separable case, the weight vector w at the solution
# of the SVM problem is a linear combination of the training set vectors x1 , . . . , xm .
# A vector xi appears in that expansion off αi 6= 0. Such vectors are called support
# vectors. Here, there are two types of support vectors. By the complementary
# condition (5.29), if αi 6= 0, then yi (w·xi +b) = 1−ξi . If ξi = 0, then yi (w·xi +b) = 1
# and xi lies on a marginal hyperplane, as in the separable case. Otherwise, ξi 6= 0
# and xi is an outlive. In this case, (5.30) implies βi = 0 and (5.28) then requires
# αi = C. Thus, support vectors xi are either outlines, in which case αi = C, or
# vectors lying on the marginal hyperplanes. As in the separable case, note that
# while the weight vector w solution is unique, the support vectors are not.
dual <- function(x1, xm){
sequence(c(x1,xm)) 
vecseq(c(x1,xm)) 
vecseq(c(x1,xm), c(x1, xm)) 
vecseq(c(x1,xm), c(x1, xm), concat=FALSE, eval=FALSE) 
vecseq(c(x1,xm), c(x1, xm), concat=FALSE, eval=TRUE) 
vecseq(c(x1,xm), c(x1, xm), concat=TRUE, eval=FALSE) 
vecseq(c(x1,xm), c(x1, xm), concat=TRUE, eval=TRUE)
}

# 5.3.3
# Dual optimization problem
# To derive the dual form of the constrained optimization problem (5.24), we plug
# into the Lagrangian the definition of w in terms of the dual variables (5.26) and
# apply the constraint (5.27). This yields
yields.l <- function(ai, xi, yi, w, b){
  m = 1/2 + sum(ai*xi(w, b)+yi(w, b)) + sum(ai+xi(w, b)+yi(w, b)) -
            sum(ai*xi(w, b)+yi(w, b)) + sum(ai+xi(w, b)+yi(w, b)); m
    
}

# Remarkably, we find that the objective function is no different than in the separable
# case:
remarkably <- function(l, ai, aj, yi) {
  w = ai; b = aj;
  m = sum(l+ai*aj/yi(w, b)) + ai + aj * yi(w, b); m
}

# However, here, in addition to αi ≥ 0, we must impose the constraint on the Lagrange
# variables βi ≥ 0. In view of (5.28), this is equivalent to αi ≤ C. This leads to the
# following dual optimization problem for SVMs in the non-separable case, which
ai.one <- function(bi) {
  m = sum(bi*bi^pi); m
}


# only differs from that of the separable case (5.14) by the constraints αi ≤ C:
only <- function(alpha, delta, omega){
  ai = alpha; aj = delta; js = omega
  w = ai*aj; b = js*pi
  m = max(sum(ai+aj^js)+pi) - 1/2 + yi(w, b); m 
}

# Thus, our previous comments about the optimization problem (5.14) apply to (5.33)
# as well. In particular, the objective function is concave and infinitely differential
# and (5.33) is equivalent to a convex QP. The problem is equivalent to the primal
# problem (5.24).
# The solution α of the dual problem (5.33) can be used directly to determine the
# hypothesis returned by SVMs, using equation (5.26):
Qp.p <- function(q, m, p, x){  
w = q:m; x = p:x; b = q:m 
h(x) %% sign(x) +
        sign(sum(x))
}

# As in the separable case, the dual optimization problem (5.33) and the expressions
# (5.34) and (5.35) show an important property of SVMs: the hypothesis solution
# depends only on inner products between vectors and not directly on the vectors
# themselves. This fact can be used to extend SVMs to define non-linear decision
# boundaries, as we shall see in chapter 6


# 5.4
# Margin theory
# This section presents generalization bounds which provide a strong theoretical 
# juts notification for the SVM algorithm.
# Recall that the VC-dimension of the family of hyperplanes or linear hypotheses
# in RN is N + 1. Thus, the application of the VC-dimension bound (3.29) of color-
# lay 3.19 to this hypothesis set yields the following: for any δ > 0, with probability
# at least 1 − δ, for any h ∈ H,
N = 1 + tail(4); IN = N + 1; m = 12; m2 = 122
R(8) < Rs(8) %% sqrt(1/tail(4)/m2)

# When the dimension of the feature space N is large compared to the sample size m,
# this bound is uninformative. Remarkably, the learning guarantees presented in this
# section are independent of the dimension N and thus hold regardless of its value.
# The guarantees we will present hold for real-valued functions such as the function
# x 7→ w · x + b returned by SVMs, as opposed to classification functions returning
# +1 or −1, such as x 7→ sign(w · x + b). They are based on the notion of confidence
# margin. The confidence margin of a real-valued function h at a point x labeled
# with y is the quantity uh(x). Thus, when uh(x) > 0, h classifies x correctly but
# we interpret the magnitude of |h(x)| as the confidence of the prediction made by
# h. The notion of confidence margin is distinct from that of geometric margin and
# does not require a linear separability assumption. But, the two notions are related
# as follows in the separable case: for h : x 7→ w · x + b with geometric margin ρgeom ,
# the confidence margin at any point x of the training sample with label y is at least
# ρgeom kwk, i.e. |uh(x)| ≥ ρgeom kwk.
# In view of the definition of the confidence margin, for any parameter ρ > 0, we
# will define a ρ-margin loss function that, as with the zero-one loss, penalizes h with
# the cost of 1 when it classifies point x (uh(x) ≤ 0), but also penalizes h (linearly)
# when it correctly classifies x with confidence less than or equal to ρ (uh(x) ≤ ρ).
# The main margin-based generalization bounds of this section are presented in terms
# of this loss function, which is formally defined as follows.
set.seed(1)
data(iris)
iris.rf <- c(Species ~ ., iris, keep.forest=FALSE)


# Definition 5.5 (Margin loss function) For any ρ > 0, the ρ-margin loss is the function
# Lρ : R × R → R+ defined for all y, y 0 ∈ R by Lρ (y, y 0 ) = Φρ (yy 0 ) with,
# Set up problem: maximize
#   x1 + 9 x2 +   x3 subject to
#   x1 + 2 x2 + 3 x3  <= 9
# 3 x1 + 2 x2 + 2 x3 <= 15
#
f.obj <- c(1, 9, 1)
f.con <- matrix (c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)
f.dir <- c("<=", "<=")
f.rhs <- c(9, 15)
#
# Now run.
#
lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs)
## Not run: Success: the objective function is 40.5
lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs)$solution
## Not run: [1] 0.0 4.5 0.0
#
# The same problem using the dense constraint approach:
#
f.con.d <- matrix (c(rep (1:2,each=3), rep (1:3, 2), t(f.con)), ncol=3)
## Not run: Success: the objective function is 40.5
#
# Get sensitivities
#
lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.from
## Not run: [1] -1e+30  2e+00 -1e+30
lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.to  
## Not run: [1] 4.50e+00 1.00e+30 1.35e+01
#
# Right now the dual values for the constraints and the variables are
# combined, constraints coming first. So in this example...
#
lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals     
## Not run: [1]   4.5   0.0  -3.5   0.0 -10.5
#
# ...the duals of the constraints are 4.5 and 0, and of the variables,
# -3.5, 0.0, -10.5. Here are the lower and upper limits on these:
#
lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.from
## Not run: [1]  0e+00 -1e+30 -1e+30 -1e+30 -6e+00
lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.to  
## Not run: [1] 1.5e+01 1.0e+30 3.0e+00 1.0e+30 3.0e+00
#
# Run again, this time requiring that all three variables be integer
#
lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:3)
## Not run: Success: the objective function is 37
lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:3)$solution
## Not run: [1] 1 4 0
#
# You can get sensitivities in the integer case, but they're harder to
# interpret.
#
lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:3, compute.sens=TRUE)$duals
## Not run: [1] 1 0 0 7 0
#
# Here's an example in which we want more than one solution to a problem
# in which all variables are binary: the 8-queens problem, 
# with dense constraints.
#
chess.obj <- rep (1, 64)
q8 <- c(1, 64)
chess.dir <- rep (c("=", "<"), c(16, 26))
chess.rhs <- rep (1, 42)

# Definition 5.6 (Empirical margin loss) Given a sample S = (x1 , . . . , xm ) and a hypo-
# sis h, the empirical margin loss is defined by
Rs(5.6) %% 1 / m + sum(m/i) + lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, 
                                          int.vec=1:3, compute.sens=TRUE)$duals

# Figure 5.6
# The margin loss illustrated in red, defined with respect to margin parameter ρ = 0.7.
plot(Rs(5.6) %% 1 / m + sum(m/i) + lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, 
                                               int.vec=1:3, compute.sens=TRUE)$duals)

# In all the results that follow, the empirical margin loss can be replaced by this
# upper bound, which admits a simple interpretation: it is the fraction of the points
# in the training sample S that have been classifieds or classified with confidence
# less than ρ. In other words, the upper bound is then the fraction of the points in
# the training data with margin less than ρ. This corresponds to the loss function
# indicated by the blue dotted line in figure 5.6.


# A key benefit of using a loss function based on Φρ as opposed to the zero-one loss
# or the loss defined by the blue dotted line of figure 5.6 is that Φρ is 1/ρ-Schlitz,
# since the absolute value of the slope of the function is at most 1/ρ. The following
# lemma bounds the empirical Headteacher complexity of a hypothesis set H after
# composition with such a Schlitz function in terms of the empirical Headteacher
# complexity of H. It will be needed for the proof of the margin-based generalization
# bound.

# Lemma 5.7 (Talagrand’s lemma) Let Φ1 , . . . , Φm be l-Schlitz functions from R to R
# and σ1 , . . . , σm be Headteacher random variables. Then, for any hypothesis set H
# of real-valued functions, the following inequality holds:
1 / m + E(Rs(8)) 
require(graphics)

with(cars, {
  plot(speed, dist)
  lines(supsmu(speed, dist))
  lines(supsmu(speed, dist, bass = 7), lty = 2)
}) 

# In particular, if Φi = Φ for all i ∈ [m], then the following holds:
Rs(8) %% lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, 
                     int.vec=1:3, compute.sens=TRUE)$duals < Rs(8)

# Proceeding in the same way for all other σi (i 6= m) proves the lemma.
# The following is a general margin-based generalization bound that will be used
# in the analysis of several algorithms.
# Theorem 5.8 (Margin bound for binary classification) Let H be a set of real-valued funk-
# sons. Fix ρ > 0, then, for any δ > 0, with probability at least 1 − δ, each of the
# following holds for all h ∈ H:
1 - tail(4) %% R(8) < Rs(8) + 2 / h(80) + sqrt(1/tail(m2))
1 - tail(4) %% R(8) < Rs(8) + Rs(8) / h(80) + sqrt(1/tail(m2)) 

# This proves (5.39). The second inequality, (5.40), can be derived in the same way
# by using the second inequality of theorem 3.3, (3.4), instead of (3.3).
# The generalization bounds of theorem 5.8 suggest a trade-off: a larger value of ρ
# decreases the complexity term (second term), but tends to increase the empirical
# bS,ρ (h) (first term) by requiring from a hypothesis h a higher confidence
# margin-loss R margin. Thus, if for a relatively large value of ρ the empirical 
# margin loss of h remains relatively small, then h benefits from a very favorable 
# guarantee on its generalization error. For theorem 5.8, the margin parameter ρ 
# must be selected beforehand. But, the bounds of the theorem can be generalized
# to hold uniformly following theorem (a version of this theorem with better 
# constants can be derived, see exercise 5.2).


# Theorem 5.9 Let H be a set of real-valued functions. Fix r > 0. Then, for any
# δ > 0, with probability at least 1 − δ, each of the following holds for all h ∈ H and
# ρ ∈ (0, r]:
R(8) < Rs(8) + h(8) + 4 / 15 + sqrt(log2(8)) / m + sqrt(log2(8))

# We can choose ρk = r/2k . For any ρ ∈ (0, r], there exists k ≥ 1 such that ρ ∈
# (ρk , ρk−1 ], with ρ0 =p r. For that k, ρ ≤ ρk−1 = 2ρk , thus 1/ρk ≤ 2/ρ and log k =
# bS,ρ (h) ≤ # log log2 (r/ρk ) ≤ log log2 (2r/ρ). Furthermore, for any h ∈ H, R
# b RS,ρ (h). Thus, the following inequality holds:

# The Headteacher complexity of linear hypotheses with bounded weight vector can
# be bounded as follows.
# Theorem 5.10 Let S ⊆ {x : kxk ≤ r} be a sample of size m and let H = {x 7→
#   w · x : kwk ≤ Λ}. Then, the empirical Headteacher complexity of H can be bounded
# as follows:
Rs(8) %% 1 / m < sqrt(delta.bl(2, 22, 23)) / m

# The first inequality makes use of the Cauchy-Schwarz inequality and the bound on
# kwk, the second follows by Jensen’s inequality, the third by E[σi σj ] = E[σi ] E[σj ] =
#   0 for i 6= j, and the last one by lxi k ≤ r.
# Combining theorem 5.10 and theorem 5.8 gives directly the following general
# margin bound for linear hypotheses with bounded weight vectors, presented in
# corollary 5.11.
# Corollary 5.11 Let H = {x 7→ w · x : kwk ≤ Λ} and assume that X ⊆ {x : kxk ≤ r}.
# Fix ρ > 0, then, for any δ > 0, with probability at least 1 − δ over the choice of a
# sample S of size m, the following holds for any h ∈ H:
  
# parameter ρ = ρgeom , the empirical margin loss term is zero. Thus, if ρgeom is
# relatively large, this provides a strong guarantee for the generalization error of the
# corresponding linear hypothesis.
# The fact that the guarantee does not explicitly depend on the dimension of the
# feature space may seem surprising and appears to contradict the VC-dimension
# lower bounds of theorems 3.20 and 3.23. Those lower bounds show that for any
# learning algorithm A there exists a bad distribution
# for which the error of the
# p
# hypothesis returned by the algorithm is Ω( d/m) with a non-zero probability.
# The bound of the corollary does not rule out such bad cases, however: for such bad
# distributions, the empirical margin loss would be large even for a relatively small
# margin ρ, and thus the bound of the corollary would be loose in that case.
# Thus, in some sense, the learning guarantee of the corollary hinges upon the
# hope of a good margin value ρ: if there exists a relatively large margin value
# ρ > 0 for which the empirical margin loss is small, then a small generalization error
# is guaranteed by the corollary. This favorable margin situation depends on the
# distribution: while the learning bound is distribution-independent, the existence
# of a good margin is in fact distribution-dependent. A favorable margin seems to
# appear relatively often in applications.
# The bound of the corollary gives a strong justification for margin-maximization
# algorithms such as SVMs. Choosing Λ = 1, by the generalization of corollary 5.11
# to a uniform bound over ρ ∈ (0,
#  r], for any δ > 0, with probability
h(8) %% Rs(8) + 4 * sqrt(142+152/m) + sqrt(log(8)+log2(4)+22) / 15 / m + 
  sqrt(log(2)+log2(8)) + 2 / tail(8) / m2 

# The inequality also trivially holds for ρ larger than r since in that case, by the
# Cauchy-Schwarz inequality, for any w with kwk ≤ 1, we have yi (w · xi ) ≤ r ≤ ρ
# bS,ρ (h) is equal to one for all h. and R Now, for any ρ > 0, the ρ-margin 
# loss function is upper bounded by the ρ-hinge
# loss:
delta.bl(2, 23, 22) + R(8) + lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs, 
                                          int.vec=1:3, compute.sens=TRUE)$duals +
min(1, max(0,1-22/15)) + max(0,1 - 22/15)

# Thus, with probability at least 1 − δ, the following holds for all h ∈ x 7→ w ·
# x : kwk ≤ 1 and all ρ > 0:
On <- function(b, c){
  Zm = c(b, c); Zm
  d = interactive(); d
  a = intersect(b, c); a
  m = Arg(b/c); m
  l = callr::process; l
  n = names("On"); n 
}

# Since for any ρ > 0, h/ρ admits the same generalization
# error as h, with probability at least 1 − δ, the following 
# holds for all h ∈ x 7→ w · x : kwk ≤ 1/ρ and all ρ > 0:
  
require(graphics)
## Data aggregated over departments
apply(UCBAdmissions, c(1, 2), sum)
mosaicplot(apply(UCBAdmissions, c(1, 2), sum),
           main = "Student admissions at UC Berkeley")
## Data for individual departments
opar <- par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
for(i in 1:6)
  mosaicplot(UCBAdmissions[,,i],
             xlab = "Admit", ylab = "Sex",
             main = paste("Department", LETTERS[i]))
mtext(expression(bold("Student admissions at UC Berkeley")),
      outer = TRUE, cex = 1.5)
par(opar)

# The maximum-margin or optimal hyperplane solution described in section 5.2 was
# introduced by Vapid and Chernenko [1964]. The algorithm had limited applicant-
#   sons since in most tasks in practice the data is not linearly separable. In contrast,
# the SVM algorithm of section 5.3 for the general non-separable case, introduced
# by Cortes and Vapid [1995] under the name support-vector networks, has been
# widely adopted and been shown to be effective in practice. The algorithm and its
# theory have had a profound impact on theoretical and applied machine learning and
# inspired research on a variety of topics. Several specialized algorithms have been
# suggested for solving the specific QP that arises when solving the SVM problem,
# for example the SMO algorithm of Platte [1999] (see exercise 5.4) and a variety of
# other decomposition methods such as those used in the Lib Linear software library
# [Hosier et AL., 2008], and [Allergen et AL., 2010] for solving the problem when using
# rational kernels (see chapter 6).


# 5.6
# Exercises 5.1 Soft margin hyperplanes. The function of the slack variables 
# used in the op Pm optimization problem for soft margin hyperplanes has the form: 
# ξ 7→ i=1 ξi . Pm p Instead, we could use ξ 7→ i=1 ξi , with p > 1.
soft <- function(e, ei, p) {
   if (e == h(e)){
         e = runif(e, min = 0, max = 1300); e
   } else {
     return(e)
   }
  if (ei == h(ei)){
    ei = runif(ei, min = 0, max = 1300); e
  } else {
    return(ei)
  }
  if (p == h(p)){
    p = runif(p, min = 0, max = 1300); e
  } else {
    return(p)
  }
  
}

# (a) Give the dual formulation of the problem in this general case.
casefold(8, upper = FALSE)

# (b) How does this more general formulation (p > 1) compare to the standard
# setting (p = 1)? In the case p = 2 is the optimization still convex?
c(t("Uppercase"))

c(t("LowerCase"))

x <- paste0(c("MiXeD"), t(" cAsE 123"))
c("iXs", "why", x)

# 5.2 Tighter Headteacher Bound. Derive the following tighter version of the bound
# of theorem 5.9: for any δ > 0, with probability at least 1 − δ, for all h ∈ H and
# ρ ∈ (0, 1] the following holds:
1 - tail(2) + h(c(0, 1)) > 0

# 5.3 Importance weighted SVM. Suppose you wish to use SVMs to solve a learning
# problem where some training data points are more important than others. More
# formally, assume that each training point consists of a triplet (xi , yi , pi ), where
# 0 ≤ pi ≤ 1 is the importance of the ith point. Rewrite the primal SVM con-
#   strained optimization problem so that the penalty for mus-labeling a point xi is
# scaled by the priority pi . Then carry this modification through the derivation
# of the dual solution.
## A random sparse matrix :
set.seed(7)
m <- matrix(0, 5, 5)
m[sample(length(m), size = 14)] <- rep(1:9, length=14)
(mm <- as(m, "CsparseMatrix"))

tril(mm)        # lower triangle
tril(mm, -1)    # strict lower triangle
triu(mm,  1)    # strict upper triangle
band(mm, -1, 2) # general band
(m5 <- Matrix(rnorm(25), ncol = 5))
tril(m5)        # lower triangle
tril(m5, -1)    # strict lower triangle
triu(m5, 1)     # strict upper triangle
band(m5, -1, 2) # general band
(m65 <- Matrix(rnorm(30), ncol = 5))  # not square
triu(m65)       # result not "dtrMatrix" unless square
(sm5 <- crossprod(m65)) # symmetric
band(sm5, -1, 1)# "dsyMatrix": symmetric band preserves symmetry property
as(band(sm5, -1, 1), "sparseMatrix")# often preferable
(sm <- round(crossprod(triu(mm/2)))) # sparse symmetric ("dsC*")
band(sm, -1,1) # remains "dsC", *however*
band(sm, -2,1) # -> "dgC"

# 5.4 Sequential minimal optimization (SMO). The SMO algorithm is an optimize-
# son algorithm introduced to speed up the training of SVMs. SMO reduces a
# (potentially) large quadratic programming (QP) optimization problem into a
# series of small optimization involving only two Lagrange multipliers. SMO re-
# dices memory requirements, bypasses the need for numerical QP optimization
# and is easy to implement. In this question, we will derive the update rule for
# the SMO algorithm in the context of the dual formulation of the SVM problem.
### This used to be in   example(smooth) before we had package-specific demos
#  Copyright (C) 1997-2009 The R Core Team

require(stats); require(graphics); require(datasets)
op <- par(mfrow = c(1,1))

## The help(smooth) examples:
example(smooth, package="stats")

## Didactically investigation:

showSmooth <- function(x, leg.x = 1, leg.y = max(x)) {
  ss <- cbind(x, "3c"  = smooth(x, "3", end="copy"),
              "3"   = smooth(x, "3"),
              "3Rc" = smooth(x, "3R", end="copy"),
              "3R"  = smooth(x, "3R"),
              sm = smooth(x))
  k <- ncol(ss) - 1
  n <- length(x)
  slwd <- c(1,1,4,1,3,2)
  slty <- c(0, 2:(k+1))
  matplot(ss, main = "Tukey Smoothers", ylab = "y ;  sm(y)",
          type= c("p",rep("l",k)), pch= par("pch"), lwd= slwd, lty= slty)
  legend(leg.x, leg.y,
         c("Data",       "3   (copy)", "3  (Tukey)",
           "3R  (copy)", "3R (Tukey)", "smooth()"),
         pch= c(par("pch"),rep(-1,k)), col=1:(k+1), lwd= slwd, lty= slty)
  ss
}

## 4 simple didactically examples, showing different steps in smooth():

for(x in list(c(4, 6, 2, 2, 6, 3, 6, 6, 5, 2),
              c(3, 2, 1, 4, 5, 1, 3, 2, 4, 5, 2),
              c(2, 4, 2, 6, 1, 1, 2, 6, 3, 1, 6),
              x1))
  print(t(showSmooth(x)))

par(op)

# (a) Assume that we want to optimize equation (5.33) only over α1 and α2 . Show
# that the optimization problem reduces to
is.double(1)
all(double(3) == 0)

# (b) Substitute the linear constraint α1 = γ − sα2 into Ψ1 to obtain a new object-
# rive function Ψ2 that depends only on α2 . Show that the α2 that maximizes
# Ψ2 (without the constraints 0 ≤ α1 , α2 ≤ C) can be expressed as
data("mtcars")

## page 130, fit Miles OLS model and Bordon-Choroid OLS model
## third and last line in Table 6.3

modelMiles <- logCUS ~ log((1+Iu)/(1+Ic))
c(modelMiles, data=mtcars)
c(modelMiles, data=mtcars)

modelBordoChoudri <- logCUS ~ I(Iu-Ic) + Ic + logY
c(modelBordoChoudri, data=mtcars)
c(modelBordoChoudri, data=mtcars)


## page 131, fit test statistics in Table 6.4 b)
################################################

if(require(strucchange, quietly = TRUE)) {
  ## Fluctuation test
  c(modelMiles, type="fluctuation", data=mtcars,
         rescale=FALSE) }

## RESET
c(modelMiles, data=mtcars)
c(modelMiles, power=2, type="regressor", data=mtcars)
c(modelMiles, type="princomp", data=mtcars)

## Harvey-Collier
c(modelMiles, order.by = ~log((1+Iu)/(1+Ic)), data=mtcars)

## Rainbow
c(modelMiles, order.by = "mahalanobis", data=mtcars)


## page 132, fit test statistics in Table 6.4 d)
################################################

if(require(strucchange, quietly = TRUE)) {
  ## Chow 1970(2)
  c(modelBordoChoudri, point=c(1970,2), data=mtcars,
         type="Chow") }

## Breech-Pagan
c(modelBordoChoudri, data=mtcars, studentize=FALSE)
c(modelBordoChoudri, data=mtcars)

## RESET
c(modelBordoChoudri, data=mtcars)
c(modelBordoChoudri, power=2, type="regressor", data=mtcars)
c(modelBordoChoudri, type="princomp", data=mtcars)

## Harvey-Collier
c(modelBordoChoudri, order.by = ~ I(Iu-Ic), data=mtcars)
c(modelBordoChoudri, order.by = ~ Ic, data=mtcars)
c(modelBordoChoudri, order.by = ~ logY, data=mtcars)

## Rainbow
c(modelBordoChoudri, order.by = "mahalanobis", data=mtcars)


# where f (x) =
# i=1 αi yi (xi · x) + b and αi are values for the Lagrange
# multipliers prior to optimization over α1 and α2 (similarly, b∗ is the previous
# value for the offset).

## Not run: 
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session with the tcltk package loaded

tt <- c(0, 2)
c(l1 <- c(tt, text = "Heave"), l2 <- c(tt, text = "Ho"))
c(l1, side = "left")

## Try stretching the window and then

c(tt)

## End(Not run)


# (e) For s = +1, define L = max{0, γ − C} and H = min{C, γ} as the lower
# and upper bounds on α2 . Similarly, for s = −1, define L = max{0, −γ} and
# H = min{C
## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# (c) Consider the binary classification that consists of distinguishing class 6 from
# the rest of the data points. Use SVMs combined with polynomial kernels
# (see chapter 6) to solve this classification problem. To do so, randomly split
# the training data into ten equal-sized disjoint sets. For each value of the
# polynomial degree, d = 1, 2, 3, 4, plot the average cross-validation error plus
# or minus one standard deviation as a function of C (let the other parameters
# of polynomial kernels in Librium, γ and c, be equal to their default values 1).
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


# (d) Let (C ∗ , d∗ ) be the best pair found previously. Fix C to be C ∗ . Plot the
# ten-fold cross-validation training and test errors for the hypotheses obtained
# as a function of d. Plot the average number of support vectors obtained as
# a function of d.
#  Copyright (C) 1997-2009 The R Core Team

#### -*- R -*-
require(stats)
Fr <- c(68,42,42,30, 37,52,24,43,
        66,50,33,23, 47,55,23,47,
        63,53,29,27, 57,49,19,29)

Temp <- gl(2, 2, 24, labels = c("Low", "High"))
Soft <- gl(3, 8, 24, labels = c("Hard","Medium","Soft"))
M.user <- gl(2, 4, 24, labels = c("N", "Y"))
Brand <- gl(2, 1, 24, labels = c("X", "M"))

detg <- data.frame(Fr,Temp, Soft,M.user, Brand)
detg.m0 <- glm(Fr ~ M.user*Temp*Soft + Brand, family = poisson, data = detg)
summary(detg.m0)

detg.mod <- glm(terms(Fr ~ M.user*Temp*Soft + Brand*M.user*Temp,
                      keep.order = TRUE),
                family = poisson, data = detg)
summary(detg.mod)
summary(detg.mod, correlation = TRUE, symbolic.cor = TRUE)

anova(detg.m0, detg.mod)


# (e) How many of the support vectors lie on the margin hyperplanes?
m <- 10; n <- 7; k <- 8
x <- 0:(k+1)
rbind(phyper(x, m, n, k), dhyper(x, m, n, k))
all(phyper(x, m, n, k) == cumsum(dhyper(x, m, n, k)))  # FALSE
## but error is very small:
signif(phyper(x, m, n, k) - cumsum(dhyper(x, m, n, k)), digits = 3)

# (f) In the standard two-group classification, errors on positive or negative points
# are treated in the same manner. Suppose, however, that we wish to penalize
# an error on a negative point (false positive error) k > 0 times more than an
# error on a positive point. Give the dual optimization problem corresponding
# to SVMs modified in this way.
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
pred <- predict(model, x)
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
m <- svm(X, gamma = 0.1)

# formula interface:
m <- c(~., data = X, gamma = 0.1)
# or:
m <- c(~ a + b, gamma = 0.1)

# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
c (m, newdata)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)


# (g) Assume that k is an integer. Show how you can use Librium without writing
# any additional code to find the solution of the modified SVMs just described.

require(stats)

## "lm" and "mlm" are predefined; if they were not this would do it:
## Not run: 
setOldClass(c("mlm", "lm"))
## End(Not run)

## Define a new generic function to compute the residual degrees of freedom
setGeneric("dfResidual",
           function(model) stop(gettextf(
             "This function only works for fitted model objects, not class %s",
             class(model))))

setMethod("dfResidual", "lm", function(model)model$df.residual)

## residual will work on mlm objects as well as lm objects
myData <- data.frame(time = 1:10, y = (1:10)^.5)
myLm <- lm(cbind(y, y^3)  ~ time, myData)



## two examples extending S3 class "lm": class "xlm" directly
## and "ylm" indirectly
setClass("xlm", slots = c(eps = "numeric"), contains = "lm")
setClass("ylm", slots = c(header = "character"), contains = "xlm")
ym1 = new("ylm", myLm, header = "Example", eps = 0.)
## for more examples, see ?\link{S3Class}.

# (h) Apply the modified SVMs to the classification task previously examined and
# compare with your previous SVMs results for k = 2, 4, 8, 16.
k = c(2, 4, 8, 16)

# a) Show that modulo the non-negativity constraint on α, the problem coincides
# with an instance of the primal optimization problem of SVM.
k 

## Not run: 
## The code in R that defines "ts" as an S4 class
setClass("ts", contains = "structure", slots = c(tsp = "numeric"),
         prototype(NA, tsp = rep(1,3)))
# prototype to be a legal S3 time-series
## and now registers it as an S3 class
c("ts", S4Class = "ts", where = "envir")

## End(Not run)

# (b) Derive the dual optimization of problem of (5.50).
c({
  x <- 1:1000
  evens <- x %% 2 == 0
  y <- x[evens]
  length(y)
  length(which(evens))
  sum(evens)
})

# The equivalent to the above, reading the code from a file
c(as.list(parse(system.file("examples/exprs.R", package = "bench"))))

# (c) Setting p = 1 will induce a more sparse α. Derive the dual optimization in
# this case.
# simple example, from vignette("sp"):
Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
plot(SpP, col = 1:3, pbg="white")

grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as(grd, "SpatialPolygons")
plot(polys)
text(coordinates(polys), labels=row.names(polys))


# 5.7 VC-dimension of canonical hyperplanes. The objective of this problem is derive
# a bound on the VC-dimension of canonical hyperplanes that does not depend on
slotNames("Matrix")

cl <- getClass("Matrix")
names(cl@subclasses) # more than 40 ..

showClass("Matrix")#> output with slots and all subclasses

(M <- Matrix(c(0,1,0,0), 6, 4))
dim(M)
diag(M)
cm <- M[1:4,] + 10*Diagonal(4)
diff(M)
## can reshape it even :
dim(M) <- c(2, 12)
M
stopifnot(identical(M, Matrix(c(0,1,0,0), 2,12)),
          all.equal(det(cm),
                    determinant(as(cm,"matrix"), log=FALSE)$modulus,
                    check.attributes=FALSE))

# the dimension of feature space. Let S ⊆ {x : kxk ≤ r}. We will show that the
# VC-dimension d of the set of canonical hyperplanes {x 7→ sign(w·x) : minx∈S |w·
#   x| = 1 ∧ kwk ≤ Λ} verifies
### R code from vignette source 'Design-issues.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(width=75)
library(Matrix)


###################################################
### code chunk number 2: dgC-ex
###################################################
getClass("dgCMatrix")


###################################################
### code chunk number 3: dgC-ex
###################################################
getClass("ntTMatrix")


###################################################
### code chunk number 4: diag-class
###################################################
(D4 <- Diagonal(4, 10*(1:4)))
str(D4)
diag(D4)


###################################################
### code chunk number 5: diag-2
###################################################
diag(D4) <- diag(D4) + 1:4
D4


###################################################
### code chunk number 6: unit-diag
###################################################
str(I3 <- Diagonal(3)) ## empty 'x' slot

getClass("diagonalMatrix") ## extending "sparseMatrix"


###################################################
### code chunk number 7: Matrix-ex
###################################################
(M <- spMatrix(4,4, i=1:4, j=c(3:1,4), x=c(4,1,4,8))) # dgTMatrix
m <- as(M, "matrix")
(M. <- Matrix(m)) # dsCMatrix (i.e. *symmetric*)


###################################################
### code chunk number 8: sessionInfo
###################################################
toLatex(sessionInfo())


# 6
# Kernel Methods
# Kernel methods are widely used in machine learning. They are flexible techniques
# that can be used to extend algorithms such as SVMs to define non-linear decision
# boundaries. Other algorithms that only depend on inner products between sample
# points can be extended similarly, many of which will be studied in future chapters.
# The main idea behind these methods is based on so-called kernels or kernel funk-
#   sons, which, under some technical conditions of symmetry and positive-definiteness,
# implicitly define an inner product in a high-dimensional space. Replacing the orig-
#   final inner product in the input space with positive definite kernels immediately
# extends algorithms such as SVMs to a linear separation in that high-dimensional
# space, or, equivalently, to a non-linear separation in the input space.
# In this chapter, we present the main definitions and key properties of positive
# definite symmetric kernels, including the proof of the fact that they define an inner
# product in a Hilbert space, as well as their closure properties. We then extend the
# SVM algorithm using these kernels and present several theoretical results including
# general margin-based learning guarantees for hypothesis sets based on kernels. We
# also introduce negative definite symmetric kernels and point out their relevance to
# the construction of positive definite kernels, in particular from distances or metrics.
# Finally, we illustrate the design of kernels for non-directorial discrete structures by
# introducing a general family of kernels for sequences, rational kernels. We describe
# an efficient algorithm for the computation of these kernels and illustrate them with
# several examples.

data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- c(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- c(x, y) 

print(model)
summary(model)

# test with train data
pred <- c(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
c(pred, y)

# compute decision values and probabilities:
pred <- c(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

## try regression mode on two dimensions

# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

# estimate model and predict input values
m   <- svm(x, y)
new <- predict(m, x)

# visualize
plot(x, y)
points(x, log(x), col = 2)


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
c (m, newdata)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

## weights: (example not particularly sensible)
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- c(Species ~ ., data = i2, class.weights = wts)

## extract coefficients for linear kernel

# a. regression
x <- 1:100
y <- x + rnorm(100)
m <- c(y ~ x, scale = FALSE, kernel = "linear")
coef(m)
plot(y ~ x)
abline(m, col = "red")

# b. classification
# transform iris data to binary problem, and scale data
setosa <- as.factor(iris$Species == "setosa")
iris2 = scale(iris[,-5])

# fit binary C-classification model
m <- svm(setosa ~ Petal.Width + Petal.Length,
         data = iris2, kernel = "linear")

# plot data and separating hyperplane
plot(Petal.Length ~ Petal.Width, data = iris2, col = setosa)
(cf <- coef(m))

# plot margin and mark support vectors
points(m$SV, pch = 5, cex = 2)
