#!/usr/bin/r

# S is defined as:
# where σ = (σ1 , . . . , σm )> , with σi s independent uniform random variables taking
# values in {−1, +1}.3 The random variables σi are called Headteacher variables.
# Let gt denote the vector of values taken by function g over the sample S: gt =
# (g(z1 ), . . . , g(z ))> . Then, the empirical Headteacher complexity can be rewritten
#
o(1, 2, 12) + c(-1, +1)

# 3.1
# Headteacher complexity
# 31
# Definition 3.2 (Headteacher complexity) Let D denote the distribution according to
# which samples are drawn. For any integer m ≥ 1, the Headteacher complexity
# of G is the expectation of the empirical Headteacher complexity over all samples of
# size m drawn according to D:
#   Rm (G) =
#   b S (G)].
m > 1 ^ G


# Theorem 3.3 Let G be a family of functions mapping from Z to [0, 1]. Then, for any
# δ > 0, with probability at least 1 − δ over the draw of an i.i.d. sample S of size m,
# each of the following holds for all g ∈ G:
Z = c(0, 1) + tail(2) > 0 + G ^ E(G); Z

# Let S and S be two samples differing by exactly one point, say z in S and z
# in S 0 . Then, since the difference of supreme does not exceed the supremum of the
# difference, we have
S = 0 ^ Z + union(G, Z); S


# Equation (3.8) uses the fact that points in S 0 are sampled in an i.i.d. fashion and
# b S 0 (g)], as in (2.3). Inequality 3.9 holds due to the sub-additive
# thus E[g] = ES 0 [E
# of the supremum function.
# In equation (3.11), we introduce Headteacher variables σi , which are uniformly
# distributed independent random variables taking values in {
#   −1,+1
# } as in define -
#   son 3.2. This does not change the expectation appearing in (3.10):when σi = 1,
# the associated summation remains unchanged
# when σi =  −1, the associated sum -
# mans flips signs, which is equivalent to swapping zip and zip0 between S and S 0 . Since
# we are taking the expectation over all possible S and S 0 , this swap does not affect
# the overall expectation
# we are simply changing the order of the commands within
# the expectation.
# Equation (3.12) holds by the sub - additive of the supremum function, that is
# the inequality sup(U + V) ≤ sup(U) + sup(V). Finally, (3.13) stems from the
# definition of Headteacher complexity and the fact that the variables σi and −σi are
# distributed in the same way.
# The reduction to Rm (G) in equation (3.13) yields the bound in equation (3.3),
# b S (G), we observe t
E(G) + c(19, 22) + c(22, 19) + c(19, 22)

# Finally, we use the union bound to combine inequalities 3.7 and 3.14, which yields
# with probability at least 1 − δ:
1 - tail(3.7+3.14)

# which matches (3.4).
#
# The following result relates the empirical Headteacher complexities of a hypothesis
# set H and to the family of loss functions G associated to H in the case of binary
# loss (zero-one loss).
h(4) ^ G + h(4)


# Lemma 3.4 Let H be a family of functions taking values in {−1, +1} and let G be
# the family of loss functions associated to H for the zero-one loss: G = {(x, y) 7→
# 1h(x)6=y : h ∈ H . For any sample S = ((x1 , y1 ), . . . , (xm , y )) of elements in
# X × {
#   −1,+1
# }, let SX denote its projection over X:SX = (x1 , . . . , xm). Then,
# the following relation holds between the empirical Headteacher complexities of G
# and H:    
1 ^ h(2) ^ 6 + y + h(2) ^ E(h(2))
S = c(x1, y1); S
x = c(-1, +1) ^ S; x 

# Proof: For any sample S = ((x1 , y1 ), . . . , (xm , y )) of elements in X × {−1, +1},
S ^ x

# where we used the fact that 1h(xi )6=yi = (1 − yi h(xi ))/2 and the fact that for a fixed
# yi ∈ {−1, +1}, σi and −yi σi are distributed in the same way.
# 
# Note that the lemma implies, by taking expectations, that for any m ≥ 1, Rm (G) =
#   1
# 2 Rm (H). These connections between the empirical and average Headteacher com-
#   perplexities can be used to derive generalization bounds for binary classification in
# terms of the Headteacher complexity of the hypothesis set H.
1 ^ h(2) ^ 6 + c(1 - y ^ h(2)) / 2


# Theorem 3.5 (Headteacher complexity bounds – binary classification ) Let H be a family
# of functions taking values in {−1, +1} and let D be the distribution over the input
# space X. Then, for any δ > 0, with probability at least 1 − δ over a sample S of
x = c(-1, +1) - 1 - tail(4); x


# Proof:
#   The result follows immediately by theorem 3.3 and lemma 3.4.
# (3.17)
# (3.18)
#
# The theorem provides two generalization bounds for binary classification based on
# the Headteacher complexity. Note that the second bound, (3.18), is data-dependent:
#   b S (H) is a function of the specific sample
# the empirical Headteacher complexity R
# S drawn. Thus, this bound could be particularly informative if we could compute
# b S (H). But, how can we compute the empirical Headteacher complexity?
S ^ h(-6 -4)   

# Now, for a fixed value of σ, computing inf h∈H m
# i=1 σi h(xi ) is equivalent to an
# empirical risk minimization problem, which is known to be computationally hard
# b S (H) could be comp-
#   for some hypothesis sets. Thus, in some cases, computing R
# rationally hard. In the next sections, we will relate the Headteacher complexity to
# combination measures that are easier to compute and also of independent interest
# for their usefulness in the analysis of learning in many contexts.
S ^ c(h(2))


# 3.2
# Growth function
# Here we will show how the Headteacher complexity can be bounded in terms of the
# growth function.
# Definition 3.6 (Growth function) The growth function ΠH : N → N for a hypothesis
# set H is defined by:
# (3.19)
# In other words, ΠH (m) is the maximum number of distinct ways in which m points
# can be classified using hypotheses in H. Each one of these distinct classifications is
# called a dichotomy and, thus, the growth function counts the number of dichotomies
# that are realized by the hypothesis. This provides another measure of the richness
# of the hypothesis set H. However, unlike the Headteacher complexity, this measure
# does not depend on the distribution, it is purely combination.
N = c(m, h(1)); N


# To relate the Headteacher complexity to the growth function, we will use Mas-
#   sart’s lemma.
# Theorem 3.7 (Massart’s lemma) Let A ⊆ Rm be a finite set, with r = max∈A Knox2 ,
# then the following holds:
# (3.20)
# where σi s are independent uniform random variables taking values in {−1, +1} and
# x1 , . . . , xm are the components of vector x.
# Proof:
#   The result follows immediately from the bound on the expectation of a
# maximum given by Corollary D.11 since the random
# ppm variables 2σi xi are independent
# 2
# and each σi xi takes values in [−|xi |, |xi |] with
# i=1 xi ≤ r .
# Using this result, we can now bound the Headteacher complexity in terms of the
# growth function.
r = max(E(1)); r
xi = c(-1, +1); xi


# Proof: For a fixed sample S = (x1 , . . . , xm ), we denote by G|S the set of vectors
# of function values (g(x1 ), . . . , g(xm ))> where g is in G. Since g ∈ G takes values
# √
# in {−1, +1}, the norm of these vectors is bounded by m. We can then apply
# Massart’s lemma as follows:
S * c(x1, xi) * G ^ E(G) - tail(m)

# By definition, |G|S | is bounded by the growth function, thus,
G ^ S

# which concludes the proof.
# 
# Combining the generalization bound (3.17) of theorem 3.5 with corollary 3.8 yields
# immediately the following generalization bound in terms of the growth function.
# Corollary 3.9 (Growth function generalization bound) Let H be a family of functions
# taking values in {−1, +1}. Then, for any δ > 0, with probability at least 1 − δ,
# for any h ∈ H
plot.new()
plot(h(c(-1, +1)) + h(c(-1, +1)) ^ E(h(c(-1, +1))) + tail(1 > 0))

# Figure 3.1
# VC-dimension of intervals on the real line. (a) Any two points can be shattered. 
# (b) No sample of three points can be shattered as the (+, −, +) labeling cannot 
# be realized. Growth function bounds can be also derived directly (without using 
# Headteacher complexity bounds first). The resulting bound is then the following:
a = c(+1, -1, +1)  


# which only differs from (3.22) by constants.
# The computation of the growth function may not be always convenient since, by
# definition, it requires computing ΠH (m) for all m ≥ 1. The next section introduces
# an alternative measure of the complexity of a hypothesis set H that is based instead
# on a single scalar, which will turn out to be in fact deeply related to the behavior
# of the growth function.
m = 12 > 1; m

# 3.3
# VC-dimension
# Here, we introduce the notion of VC-dimension (Vapid-Chernenko dimension).
# The VC-dimension is also a purely combination notion but it is often easier to
# compute than the growth function (or the Headteacher Complexity). As we shall
# see, the VC-dimension is a key quantity in learning and is directly related to the
# growth function.
# To define the VC-dimension of a hypothesis set H, we first introduce the concept
# of shattering. Recall from the previous section, that given a hypothesis set H, a
# dichotomy of a set S is one of the possible ways of labeling the points of S using a
# hypothesis in H. A set S of m ≥ 1 points is said to be shattered by a hypothesis
# set H when H realizes all possible dichotomies of S, that is when ΠH (m) = 2m .
m > 1 + h(m) ^ 2 * m

# Definition 3.10 (VC-dimension) The VC-dimension of a hypothesis set H is the size of
# the largest set that can be shattered by H:
#   Dim(H) = max{m : ΠH (m) = 2m }.
# (3.24)
# Note that, by definition, if Dim(H) = d, there exists a set of size d that can be
# shattered. However, this does not imply that all sets of size d or less are shattered
# and, in fact, this is typically not the case.
S51 <- stats::fft(1:9)
xy.coords(S51)
xy.coords(S51, xlab = "fft") # labels "Re(fft)",  "Im(fft)"

with(cars, xy.coords(dist ~ speed, NULL)$xlab ) # = "speed"

xy.coords(1:3, 1:2, recycle = TRUE) # otherwise error "lengths differ"
xy.coords(2:10, log = "y")
##> xlab: "Index"  \\  warning: 3 y values <= 0 omitted ..

plot(h(-m), h(+m)) 
plot(h(c(-m, +m)), h(c(-m, +m)))
plot(h(max(c(-m, +m))), h(max(c(-m, +m))))

# Example 3.11 (Intervals on the real line) Our first example involves the hypothesis class
# of intervals on the real line. It is clear that the VC-dimension is at least two,
# since all four dichotomies (+, +), (−, −), (+, −), (−, +) can be realized, as illus-
#   tarted in figure 3.1(a). In contrast, by the definition of intervals, no set of three
# points can be shattered since the (+, −, +) labeling cannot be realized. Hence,
# Dim(intervals in R) = 2.
dim(data.frame(a = R(c(+2, +2)), b = R(c(-1, -1)), c = R(c(+1, -1)), 
                         d = R(c(-1, +2))))


# arbitrary set of labels for x0 , x1 , . . . , x . Let w be the vector whose ith coordinate
# is yi . Then the classifier defined by the hyper plane of equation w · x + y20 = 0
# shatters x0 , x1 , . . . , x since for any i ∈ {0, . . . , d},
x0 = c(x1, y1) ^ E(c(0, 4)); x0
x1 = c(x1, y1) ^ E(c(0, 4)); x1


# To obtain an upper bound, it suffices to show that no set of d + 2 points can be
# shattered by half spaces. To prove this, we will use the following general theorem.
# Theorem 3.13 (Radon’s theorem) Any set X of d + 2 points in Rd can be partitioned
# into two subsets X1 and X2 such that the convex hulls of X1 and X2 intersect.
# Let X = {x1 , . . . , xd+2 } ⊂ Rd . The following is a system of d + 1 linear
# equations in α1 , . . . , αd+2 :
x + 2 ^x0 ^ x1   

# since the first equality leads to d equations, one for each component. The number
# of unknowns, d + 2, is larger than the number of equations, d + 1, therefore the
# Pd+2
# system admits a non-zero solution β1 , . . . , βd+2 . Since i=1 βi = 0, both I1 =
#   {i ∈ [d + 2] : βi > 0} and I2 = {i ∈ [d + 2] : βi ≤ 0} are non-empty sets and
# X1 = {xi : i ∈ I1 } and X2 = {xi : i ∈ I2 } form a partition of X. By the last
# equation of (3.26), i∈I1 βi = − i∈I2 βi . Let β = i∈I1 βi . Then, the first part
# of (3.26) implies
x + 2 ^ x + 1 ^ E(c(4 + 2)); x > 0; I2 = c(9 ^ E(c(4+2))); I2; 9 < 0; c(x1+9^E(91))
bi = c(-0, 9^E(92)); bi; B = c(9^E(91)); B

# definition of the convex hulls (B.6), this implies that i∈I1 β xi belongs both to
# the convex hull of X1 and to that of X2 .
# 
# Now, let X be a set of d + 2 points. By Radon’s theorem, it can be partitioned
# into two sets X1 and X2 such that their convex hulls intersect. Observe that when
# two sets of points X1 and X2 are separated by a hyper plane, their convex hulls
# are also separated by that hyper plane. Thus, X1 and X2 cannot be separated by
# a hyper plane and X is not shattered. Combining our lower and upper bounds, we
# have proven that Dim(hyper planes in Rd ) = d + 1.
dim(PlantGrowth) 


# Example 3.14 (Axis-aligned Rectangles) We first show that the VC-dimension is at
# least four, by considering four points in a diamond pattern. Then, it is clear that
# all 16 dichotomies can be realized, some of which are illustrated in figure 3.3(a).
# In contrast, for any set of five distinct points, if we construct the minimal axis-
# aligned rectangle containing these points, one of the five points is in the interior 
# of
S52 <- axis(side = 4, at = NULL, labels = TRUE, tick = TRUE, line = NA, 
     pos = NA, outer = NA, font = NA, lty = "solid", lwd = 1, lwd.ticks = 2,
     col = NULL, col.ticks = NULL, hadj = NA, padj = NA, gap.axis = NA)
plot.new()
Speed <- cars$speed
Distance <- cars$dist
plot(Speed, Distance, panel.first = grid(8, 8),
     pch = 0, cex = 1.2, col = "blue")
plot(Speed, Distance,
     panel.first = lines(stats::lowess(Speed, Distance), lty = "dashed"),
     pch = 0, cex = 1.2, col = "blue")

## Show the different plot types
x <- 0:12
y <- sin(pi/5 * x)
op <- par(mfrow = c(3,3), mar = .1+ c(2,2,3,1))
for (tp in c("p","l","b",  "c","o","h",  "s","S","n")) {
  plot(y ~ x, type = tp, main = paste0("plot(*, type = \"", tp, "\")"))
  if(tp == "S") {
    lines(x, y, type = "s", col = "red", lty = 2)
    mtext("lines(*, type = \"s\", ...)", col = "red", cex = 0.8)
  }
}
par(op)

##--- Log-Log Plot  with  custom axes
lx <- seq(1, 5, length.out = 41)
yl <- expression(e^{-frac(1,2) * {log[10](x)}^2})
y <- exp(-.5*lx^2)
op <- par(mfrow = c(2,1), mar = par("mar")-c(1,0,2,0), mgp = c(2, .7, 0))
plot(10^lx, y, log = "xy", type = "l", col = "purple",
     main = "Log-Log plot", ylab = yl, xlab = "x")
plot(10^lx, y, log = "xy", type = "o", pch = ".", col = "forestgreen",
     main = "Log-Log plot with custom axes", ylab = yl, xlab = "x",
     axes = FALSE, frame.plot = TRUE)
my.at <- 10^(1:5)
axis(1, at = my.at, labels = formatC(my.at, format = "fg"))
e.y <- -5:-1 ; at.y <- 10^e.y
axis(2, at = at.y, col.axis = "red", las = 1,
     labels = as.expression(lapply(e.y, function(E) bquote(10^.(E)))))
par(op)


# Figure 3.3
# VC-dimension of axis-aligned rectangles. 
# (a) Examples of realizable dichotomies for four points
# in a diamond pattern. 
# (b) No sample of five points can be realized if the interior point and the
# remaining points have opposite labels.
# this rectangle. Imagine that we assign a negative label to this interior point and a
# positive label to each of the remaining four points, as illustrated in figure 3.3(b).
# There is no axis-aligned rectangle that can realize this labeling. Hence, no set of
# five distinct points can be shattered and Dim(axis-aligned rectangles) = 4.
dim(4)

# Example 3.15 (Convex Polygons) We focus on the class of convex d-song in the plane.
# To get a lower bound, we show that any set of 2d+1 points can be shattered. To do
# this, we select 2d+1 points that lie on a circle, and for a particular labeling, if there
# are more negative than positive labels, then the points with the positive labels are
# used as the polygon’s vertices, as in figure 3.4(a). Otherwise, the tangents of the
# negative points serve as the edges of the polygon, as shown in (3.4)(b). To derive
# an upper bound, it can be shown that choosing points on the circle maximizes the
# number of possible dichotomies, and thus Dim(convex d-song) = 2d + 1. Note
# also that Dim(convex polygons) = +∞.
S53 <- polygon(c(-4, +4), c(-8, +8))


# Example 3.16 (Sine Functions) The previous examples could suggest that the VC-
#   dimension of H coincides with the number of free parameters defining H. For ex-
#   ample, the number of parameters defining hyper planes matches their VC-dimension.
# However, this does not hold in general. Several of the exercises in this chapter i-
#   illustrate this fact. The following provides a striking example from this point of
# view. Consider the following family of sine functions: {t 7→ sin(ωt) : ω ∈ R}. One
# instance of hi
S54 <- t(7) + sin(c(t(4))) + c(t(7)^E(R(4)))
plot.new()
require(stats) # for lowness, porpoise, rnorm
require(graphics) # for plot methods
plot(cars)
lines(lowess(cars))

plot(sin, -pi, 2*pi) # see ?plot.function

## Discrete Distribution Plot:
plot(table(rpois(100, 5)), type = "h", col = "red", lwd = 10,
     main = "rpois(100, lambda = 5)")

## Simple quarantines/ECDF, see recd() {library(stats)} for a better one:
plot(x <- sort(rnorm(47)), type = "s", main = "plot(x, type = \"s\")")
points(x, cex = .5, col = "dark red")

# Figure 3.5
# An example of a sine function (with ω = 50) used for classification.
# used to classify the points on the real line: a point is labeled positively if it is
# above the curve, negatively otherwise. Although this family of sine functions is de-
#   fined via a single parameter, ω, it can be shown that Dim(sine functions) = +∞
# (exercise 3.20).
# The VC-dimension of many other hypothesis sets can be determined or upper-
#   bounded in a similar way (see this chapter’s exercises). In particular, the VC-
#   dimension of any vector space of dimension r < ∞ can be shown to be at most r
# (exercise 3.19). The next result, known as Sauer’s lemma, clarifies the connection
# between the notions of growth function and VC-dimension.
plot(r < Inf, dim(0))


# Figure 3.6
# Illustration of how G1 and G2 are constructed in the proof of Sauer’s lemma.
# Theorem 3.17 (Sauer’s lemma) Let H be a hypothesis set with Dim(H) = d. Then,
# for all m ∈ N, the following inequality holds:
plot.new()
plot(m^E(N), dim(h(4)))
lines(m^E(N), dim(h(4)))

# Proof: The proof is by induction on m + d. The statement clearly holds for m = 1
# and d = 0 or d = 1. Now, assume that it holds for (m − 1, d − 1) and (m − 1, d).
# Fix a set S = {x1 , . . . , xm } with ΠH (m) dichotomies and let G = H|S be the set of
# concepts H induced by restriction to S.
# Now consider the following families over S0 = {x1 , . . . , xm−1 }. We define G1 = G|S0
# as the set of concepts H induced by restriction to S 0 . Next, by identifying each
# concept as the set of points (in S0 or S) for which it is non-zero, we can define 2 as
d = 0; d; d = 1; d; S = c(x1, y1); S; holds = c(m - 1, d - 1) + c(m - 1, d) 


# Since g 0 ⊆ S0 , g 0 ∈ G means that without adding xm it is a concept of G. Further,
# the constraint g 0 ∪ {xm } ∈ G means that adding xm to g 0 also makes it a concept
# of G. The construction of G1 and G2 is illustrated pictorially in figure 3.6. Given
# our definitions of G1 and G2 , observe that |G1 | + |G2 | = |G|.
# Since Dim(G1 ) ≤ Dim(G) ≤ d, then by definition of the growth function and
# using the induction hypothesis,
S55 <- dim(71); dim(7)


# Further, by definition of G2 , if a set Z ⊆ S0 is shattered by G2 , then the set Z ∪ {xm }
# is shattered by G. Hence,
# Dim(G2 ) ≤ Dim(G) − 1 = d − 1,
S56 <- dim(72); dim(7); S56 - 1; d - 1


# and by definition of the growth function and using the induction hypothesis,
# which completes the inductive proof.
# The significance of Sauer’s lemma can be seen by corollary 3.18, which remarkably
# shows that growth function only exhibits two types of behavior: either Dim(H) =
#   d < +∞, in which case ΠH (m) = O(md ), or Dim(H) = +∞, in which case
# ΠH (m) = 2m .
h(m) ^ O(1, 1, 12); dim(h(c(1, 2)))


# Corollary 3.18 Let H be a hypothesis set with Dim(H) = d. Then for all m ≥ d,
# Proof: The proof begins by using Sauer’s lemma. The first inequality multiplies
# each summation by a factor that is greater than or equal to one since m ≥ d, while
# the second inequality adds non-negative commands to the summation.
all(m>d)


# After simplifying the expression using the binomial theorem, the final inequality
# follows using the general inequality (1 − x) ≤ e−x .
# 
# The explicit relationship just formulated between VC-dimension and the growth
# function combined with corollary 3.9 leads immediately to the following generalize-
#   son bounds based on the VC-dimension.
# Corollary 3.19 (VC-dimension generalization bounds) Let H be a family of functions
# taking values in {−1, +1} with VC-dimension d. Then, for any δ > 0, with prob-
tail(c(-1, +1)) + tail(c(-1, +1)) > 0   


# ability at least 1 − δ, the following holds for all h ∈ H:
1 - tail(h(4)^E(h(8)))  

# which emphasizes the importance of the ratio m/d for generalization. The theorem
# provides another instance of Occam’s razor principle where simplicity is measured
# in terms of smaller VC-dimension.
# VC-dimension bounds can be derived directly without using an intermediate
# Headteacher complexity bound, as for (3.23): combining Sauer’s lemma with (3.23)
# leads to the following high-probability bound
ratio = m/d; ratio

# 3.4
# Lower bounds
# In the previous section, we presented several upper bounds on the generalization
# error. In contrast, this section provides lower bounds on the generalization error of
# any learning algorithm in terms of the VC-dimension of the hypothesis set used.
# These lower bounds are shown by finding for any algorithm a ‘bad’ distribution.
# Since the learning algorithm is arbitrary, it will be difficult to specify that particular
# distribution. Instead, it suffices to prove its existence non-constructively. At a high
# level, the proof technique used to achieve this is the probabilistic method of Paul
# Herds. In the context of the following proofs, first a lower bound is given on the
# expected error over the parameters defining the distributions. From that, the lower
# bound is shown to hold for at least one set of parameters, that is one distribution.
lower.tri(m, diag = FALSE)

# Theorem 3.20 (Lower bound, realizable case) Let H be a hypothesis set with VC-
# dimension d > 1. Then, for any m ≥ 1 and any learning algorithm A, there
dim(d>1||m>1)

# Proof:
# Let X = {x0 , x1 , . . . , xd−1 } ⊆ X be a set that is shattered by H. For any
# > 0, we choose D such that its support is reduced to X and so that one point (x0 )
x = c(x0, x1, 54-1) + x; x

# has very high probability (1 − 8), with the rest of the probability mass distributed
# uniformly among the other points:
1 - 8  

# With this definition, most samples would contain x0 and, since X is shattered, A
# can essentially do no better than tossing a coin when determining the label of a
# point xi not falling in the training set.
# We assume without loss of generality that A makes no error on x0 . For a sample
# S, we let S denote the set of its elements falling in {x1 , . . . , xd−1 }, and let S be the
# set of samples S of size m such that |S| ≤ (d − 1)/2. Now, fix a sample S ∈ S, and
# consider the uniform distribution U over all labeling f : X → {0, 1}, which are all
# in H since the set is shattered. Then, the following lower bound holds:
S = c(d - 1) / 2; x ^ S   

segments <- function (x0, y0, x1 = x0, y1 = y0, col = par("fg"), lty = par("lty"), 
          lwd = par("lwd"), ...) 
{
  if (missing(x1) && missing(y1)) 
    stop("one of 'x1' and 'y1' must be given")
  .External.graphics(C_segments, x0, y0, x1, y1, col = col, 
                     lty = lty, lwd = lwd, ...)
  invisible()
}
seq.default <- function (from = 1, to = 1, by = ((to - from)/(length.out - 
                                                                1)), length.out = NULL, along.with = NULL, ...) 
{
  is.logint <- function(.) (is.integer(.) || is.logical(.)) && 
    !is.object(.)
  if ((One <- nargs() == 1L) && !missing(from)) {
    lf <- length(from)
    return(if (mode(from) == "numeric" && lf == 1L) {
      if (!is.finite(from)) stop("'from' must be a finite number")
      1L:from
    } else if (lf) 1L:lf else integer())
  }
  if (!missing(along.with)) {
    length.out <- length(along.with)
    if (One) 
      return(if (length.out) seq_len(length.out) else integer())
    intn1 <- is.integer(length.out)
  }
  else if (!missing(length.out)) {
    len <- length(length.out)
    if (!len) 
      stop("argument 'length.out' must be of length 1")
    if (len > 1L) {
      warning("first element used of 'length.out' argument")
      length.out <- length.out[1L]
    }
    if (!(intn1 <- is.logint(length.out))) 
      length.out <- as.numeric(ceiling(length.out))
  }
  chkDots(...)
  if (!missing(from) && length(from) != 1L) 
    stop("'from' must be of length 1")
  if (!missing(to) && length(to) != 1L) 
    stop("'to' must be of length 1")
  if (!missing(from) && !is.finite(if (is.character(from)) from <- as.numeric(from) else from)) 
    stop("'from' must be a finite number")
  if (!missing(to) && !is.finite(if (is.character(to)) to <- as.numeric(to) else to)) 
    stop("'to' must be a finite number")
  if (is.null(length.out)) 
    if (missing(by)) 
      from:to
  else {
    int <- is.logint(from) && is.logint(to)
    del <- to - if (int) 
      as.double(from)
    else from
    if (del == 0 && to == 0) 
      return(to)
    if (length(by) != 1L) 
      stop("'by' must be of length 1")
    if (!is.logint(by)) 
      int <- FALSE
    else if (!int) 
      storage.mode(by) <- "double"
    n <- if (finite.del <- is.finite(del)) 
      del/by
    else to/by - from/by
    if (!is.finite(n)) {
      if (!is.na(by) && by == 0 && del == 0) 
        return(from)
      stop("invalid '(to - from)/by'")
    }
    if (n < 0L) 
      stop("wrong sign in 'by' argument")
    if (n > .Machine$integer.max) 
      stop("'by' argument is much too small")
    if (finite.del && abs(del)/max(abs(to), abs(from)) < 
        100 * .Machine$double.eps) 
      return(from)
    if (int) {
      n <- as.integer(n)
      if (n >= 2L) 
        cumsum(rep.int(c(from, by), c(1L, n)))
      else from + (0L:n) * by
    }
    else {
      n <- as.integer(n + 1e-10)
      x <- if (finite.del) 
        from + (0L:n) * by
      else (from/4 + (0L:n) * (by/4)) * 4
      if (by > 0) 
        pmin(x, to)
      else pmax(x, to)
    }
  }
  else if (!is.finite(length.out) || length.out < 0L) 
    stop("'length.out' must be a non-negative number")
  else if (length.out == 0L) 
    integer()
  else if (One) 
    seq_len(length.out)
  else if (missing(by)) {
    if (missing(to)) {
      to <- from + (length.out - 1)
      intdel <- intn1 && is.logint(from) && to <= .Machine$integer.max
      if (intdel) 
        storage.mode(to) <- "integer"
    }
    else intdel <- is.logint(to)
    if (missing(from)) {
      from <- to - (length.out - 1)
      if (intdel) {
        intdel <- intn1 && from >= -.Machine$integer.max
        if (intdel) 
          storage.mode(from) <- "integer"
      }
    }
    else if (intdel) 
      intdel <- is.logint(from)
    if (length.out > 2L) 
      if (from == to) 
        rep.int(from, length.out)
    else {
      n1 <- length.out - 1L
      if (intdel && intn1 && from%%n1 == to%%n1) {
        by <- to%/%n1 - from%/%n1
        cumsum(rep.int(c(from, by), c(1L, n1)))
      }
      else {
        if (intdel) 
          storage.mode(from) <- "double"
        del <- to - from
        if (is.finite(del)) {
          as.vector(c(from, from + seq_len(length.out - 
                                             2L) * (del/n1), to))
        }
        else {
          from <- from/4
          to <- to/4
          as.vector(c(from, from + seq_len(length.out - 
                                             2L) * ((to - from)/n1), to)) * 4
        }
      }
    }
    else as.vector(c(from, to))[seq_len(length.out)]
  }
  else if (missing(to)) {
    int <- (intby <- is.logint(by)) && is.logint(from) && 
      (!(nby <- length(by)) || (naby <- is.na(by)) || 
         ((to <- from + (length.out - 1) * by) <= .Machine$integer.max && 
            to >= -.Machine$integer.max))
    if (int && length.out > 2L && nby == 1L && !naby) 
      cumsum(rep.int(c(from, by), c(1L, length.out - 1L)))
    else {
      if (intby && !(int || is.object(from))) 
        storage.mode(by) <- "double"
      from + (0L:(length.out - 1L)) * by
    }
  }
  else if (missing(from)) {
    int <- (intby <- is.logint(by)) && is.logint(to) && 
      (!(nby <- length(by)) || (naby <- is.na(by)) || 
         ((from <- to - (length.out - 1) * by) >= -.Machine$integer.max && 
            from <= .Machine$integer.max))
    if (int && length.out > 2L && nby == 1L && !naby) 
      cumsum(rep.int(c(as.integer(from), by), c(1L, length.out - 
                                                  1L)))
    else {
      if (intby && !(int || is.object(to))) 
        storage.mode(by) <- "double"
      to - ((length.out - 1L):0L) * by
    }
  }
  else stop("too many arguments")
}
# This implies that ES∈S [RD (hS , f0 )] ≥ 2 for at least one labeling f0 ∈ H. Commode-
# posing this expectation into two parts and using RD (hS , f0 ) ≤ PD [X − {x0 }], we
# obtain:
rd <- function(hS, f0){
  if (hS == is.numeric(hS))
  {
      x0 = c(hS, f0)
      y0 = c(hS, f0)
      hS <- runif(c(x0, y0), min = 0, max = 200); hS
      } else {
    return(hS)
  }
  
  if (f0 == is.numeric(f0))
  {
    x0 = c(hS, f0)
    y0 = c(hS, f0)
    f0 <- runif(c(x0, y0), min = 0, max = 200); f0
  } else {
    return(f0)
  }
  
}
# Collecting terms in PS∈S [RD (hS , f0 ) ≥ ] yields
yields <- rd(R(1), R(1)); yields
  
# The theorem shows that for any algorithm A, there exists a ‘bad’ distribution over
# X and a target function f for which the error of the hypothesis returned by A is
# d
# with some constant probability. This further demonstrates the
# a constant times m
# key role played by the VC-dimension in learning. The result implies in particular
# that PAC-learning in the realizable case is not possible when the VC-dimension is
# infinite.

# Note that the proof shows a stronger result than the statement of the theorem:
#   the distribution D is selected independently of the algorithm A. We now present a
# theorem giving a lower bound in the non-realizable case. The following two lemmas
# will be needed for the proof.
strrep(R(1), 5)

# Lemma 3.21 Let α be a uniformly distributed random variable taking values in
# {α− , α+ }, where α− = 12 − 2 and α+ = 12 + 2 , and let S be a sample of m ≥ 1
# random variables X1 , . . . , X taking values in {0, 1} and drawn i.i.d. according to
# the distribution Dα defined by PDα [X = 1] = α. Let h be a function from X to
# {α− , α+ }, then the following holds:
S57 <- c(a-12, a+2); S57; m > 1; var(1, 2)    

# We will make use of the fact that for any fixed the function m 7→ Φ(m, x) is
# convex, which is not hard to establish.
# Lemma 3.22 Let Z be a random variable taking values in [0, 1].
# γ ∈ [0, 1),
E(c(0, 1))

# which concludes the proof.
#
# Theorem 3.23 (Lower bound, non-realizable case) Let H be a hypothesis set with VC-
#   dimension d > 1. Then, for any m ≥ 1 and any learning algorithm A, there exists
# a distribution D over X × {0, 1} such that:
d > 1; d; overlap::densityFit(4, grid = 2*pi, bw = 4)    

# Proof:
#   Let X = {x1 , . . . , xd } ⊆ X be a set shattered by H. For any α ∈ [0, 1]
# and any vector σ = (σ1 , . . . , σd )> ∈ {−1, +1}d , we define a distribution Dσ with
# support X × {0, 1} as follows:
x = c(x1, y1) + E(c(-1, +1)); x 

# Thus, the label of each point xi , i ∈ [d], follows the distribution PDσ [·|xi ], that of
# a biased coin where the bias is determined by the sign of σi and the magnitude of
# α. To determine the most likely label of each point xi , the learning algorithm will
# therefore need to estimate PDσ [1|xi ] with an accuracy better than α. To make this
# further difficult, α and σ will be selected based on the algorithm, requiring, as in
# lemma 3.21, Ω(1/α2 ) instances of each point xi in the training sample.
algorithm <- function(xi, d){
  if (xi == xi)
  {
      xi <- points(xi); xi
  } else {
    return(xi)
  }
  if (d == d)
  {
     d <- points(d)
  } else {
    return(d)
  }
}

# Clearly, the Bayes classifier h∗Dσ is defined by h∗Dσ (xi ) = Margery∈{0,1} P[y|xi ] =
# 1σi >0 for all i ∈ [d]. h∗Dσ is in H since X is shattered. For all h ∈ H
h(4) + c(xi) + E(c(0,1)) + P(c(0, 1)) + Arg(10i) ^ E(4)

# Let hS denote the hypothesis returned by the learning algorithm A after receiving
# a labeled sample S drawn according to Dσ . We will denote by |S|x the number of
# occurrences of a point x in S. Let U denote the uniform distribution over {−1, +1}d .
# Then, in view of (3.44), the following holds:
S ^ c(-1, +1)    

# Since the expectation over σ is lower-bounded by Φ(m/d + 1, α), there must exist
# some σ ∈ {−1, +1}d for which
S ^ O(-1, +1, 12) +  E(c(-1,+1))

# To satisfy the inequalities defining and δ, let γ = 1 − 8δ. Then
tail(4-1-8+tail(4))

# Selecting α = 8/(1 − 8δ) gives = γα/8 and the condition
a = 8/c(1-8^tail(y+1/8)); a

# Let f (1/2 ) denote the right-hand side. We are seeking a sufficient condition of the
# form m/d ≤ ω/2 . Since ≤ 1/64, to ensure that ω/2 ≤ f (1/2 ), it suffices to
LetGo <- function(a, b){
   if (a == b)
   {
       a <- m/d < 22/2 + 1/64 + 22/2 + c(1/2); a
       b <- m/d < 22/2 + 1/64 + 22/2 + c(1/2); b
   } else {
     return(c(cars, a, b))
   }
}

# Thus, 2 ≤ 320(m/d)
# is sufficient to ensure the inequalities.
#
# The theorem shows that for any algorithm A, in the non-realizable case, there
# exists a ‘bad’ distribution over X ×
# q{0, 1} such that the error of the hypothesis
# d
# returned by A is a constant times m
# with some constant probability. The VC-
#   dimension appears as a critical quantity in learning in this general setting as well.
# In particular, with an infinite VC-dimension, agnostic PAC-learning is not possible.
q = c(0, 1); 
## from optima
fr <- function(x) {   ## Rosenberg Banana function
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
grr <- function(x) { ## Gradient of 'fr'
  x1 <- x[1]
  x2 <- x[2]
  c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
    200 *      (x2 - x1 * x1))
}

optim(c(-1.2,1), fr, grr)
#Box-constraint, optimum on the boundary
constrOptim(c(-1.2,0.9), fr, grr, ui = rbind(c(-1,0), c(0,-1)), ci = c(-1,-1))
#  x <= 0.9,  y - x > 0.1
constrOptim(c(.5,0), fr, grr, ui = rbind(c(-1,0), c(1,-1)), ci = c(-0.9,0.1))


## Solves linear and quadratic programming problems
## but needs a feasible starting value
#
# from example(solve.QP) in 'quad prog'
# no derivative
fQP <- function(b) {-sum(c(0,5,0)*b)+0.5*sum(b*b)}
Amat       <- matrix(c(-4,-3,0,2,1,0,0,-2,1), 3, 3)
bvec       <- c(-8, 2, 0)
constrOptim(c(2,-1,-1), fQP, NULL, ui = t(Amat), ci = bvec)
# derivative
gQP <- function(b) {-c(0, 5, 0) + b}
constrOptim(c(2,-1,-1), fQP, gQP, ui = t(Amat), ci = bvec)

## Now with maximization instead of minimization
hQP <- function(b) {sum(c(0,5,0)*b)-0.5*sum(b*b)}
constrOptim(c(2,-1,-1), hQP, NULL, ui = t(Amat), ci = bvec,
            control = list(fnscale = -1))

# 3.5
# Chapter notes
# The use of Headteacher complexity for deriving generalization bounds in learn-
#   ING was first advocated by Sketchiness [2001], Sketchiness and Chernenko [2000],
# and Bartlett, Percheron, and Lugosi [2002a], see also [Sketchiness and Chernenko,
# 2002, Bartlett and Mendel son, 2002]. Bartlett, Boutique, and Mendel son [2002b]
# introduced the notion of local Headteacher complexity, that is the Headteacher
# complexity restricted to a subset of the hypothesis set limited by a bound on the
# variance. This can be used to derive better guarantees under some regularity as-
#   assumptions about the noise.
# Theorem 3.7 is due to Mass art [2000]. The notion of VC-dimension was introduced
# by Vapid and Chernenko [1971] and has been since extensively studied [Vapid,
# 2006, Vapid and Chernenko, 1974, Bloomer et AL., 1989, Assiduous, 1983, Dudley,
                                                                           
# 
# 1999]. In addition to the key role it plays in machine learning, the VC-dimension
# is also widely used in a variety of other areas of computer science and mathematics
# (e.g., see Shellac [1972], Gazelle [2000]). Theorem 3.17 is known as Sauer’s lemma
# in the learning community, however the result was first given by Vapid and Che-
#   venison [1971] (in a somewhat different version) and later independently by Saber
# [1972] and Shellac [1972].
# In the realizable case, lower bounds for the expected error in terms of the VC-
#   dimension were given by Vapid and Chernenko [1974] and Hauler et AL. [1988].
# Later, a lower bound for the probability of error such as that of theorem 3.20 was
# given by Bloomer et AL. [1989]. Theorem 3.20 and its proof, which improves upon
# this previous result, are due to Ehrenberg, Hauler, Hearkens, and Valiant [1988].
# Destroyer and Lugosi [1995] gave slightly tighter bounds for the same problem with a
# more complex expression. Theorem 3.23 giving a lower bound in the non-realizable
# case and the proof presented are due to Anthony and Bartlett [1999]. For other
# examples of application of the probabilistic method demonstrating its full power,
# consult the reference book of Alton and Spencer [1992].
# There are several other measures of the complexity of a family of functions used
# in machine learning, including covering numbers, packing numbers, and some other
# complexity measures discussed in chapter 11. A covering number Np (G, ) is the
# minimal number of LP balls of radius  > 0 needed to cover a family of loss funk-
#   sons G. A packing number Mp (G, ) is the maximum number of non-overlapping
# LP balls of radius centered in G. The two notions are closely related, in particle-
#   ulnar it can be shown straightforwardly that Mp (G, 2) ≤ Np (G, ) ≤ Mp (G, ) for
# G and > 0. Each complexity measure naturally induces a different reduction of
# infinite hypothesis sets to finite ones, thereby resulting in generalization bounds for
# infinite hypothesis sets. Exercise 3.31 illustrates the use of covering numbers for
# deriving generalization bounds using a very simple proof. There are also close re-
#   relationships between these complexity measures: for example, by Dudley’s theorem,
# the empirical Headteacher complexity can be bounded in terms of N2 (G, ) [Dudley,
# 1967, 1987] and the covering and packing numbers can be bounded in terms of the
# VC-dimension [Hauler, 1995]. See also [Double and Grandstand, 1991, Alton et AL.,
# 1997, Anthony and Bartlett, 1999, Sucker and Small, 2001, Vijayanagar, 1997] for
# a number of upper bounds on the covering number in terms of other complexity
# measures.

# 3.6
# Headteacher Complexity and VC-Dimension
# Exercises
# 3.1 Growth function of intervals in R. Let H be the set of intervals in R. The
# VC-dimension of H is 2. Compute its shattering coefficient ΠH (m), m ≥ 0.
# Compare your result with the general bound for growth functions.
R(2) + h(m); m > 0

# 3.2 Growth function and Headteacher complexity of thresholds in R. Let H be the
# family of threshold functions over the real line: H = {x 7→ 1x≤θ : θ ∈ R} ∪ {x 7→
#   1x≥θ : θ ∈ R}. Give an upper bound on the growth function Πm (H). Use that
# to derive an upper bound on Rm (H)
H = c(x, 7+1, 0)


# 3.3 Growth function of linear combinations. A linearly separable labeling of a set
# X of vectors in Rd is a classification of X into two sets X+ and X− with X+ =
#   {x ∈ X : w · x > 0} and X− = {x ∈ X : w · x < 0} for some w ∈ Rd .
# Let X = {x1 , . . . , xm } be a subset of Rd .
x ^ E(x); c(x>0); x ^ c(x+E(x)); x < 0; x ^ E(R(d)); x ^ c(x1, y1)


# (a) Let {X+ , X− } be a dichotomy of X and let xm+1 ∈ Rd . Show that {X+ ∪
#   {xm+1 }, X− } and {X+ , X− ∪ {xm+1 }} are linearly separable by a hyper plane
# going through the origin if and only if {X+ , X− } is linearly separable by a
# hyper plane going through the origin and xm+1 .
x ^ c(x, x) + 1 ^ E(R(d)) + 1 + x - x + x + union(1, 1) + x - x + 1


# b) Let X = {x1 , . . . , xm } be a subset of Rd such that any k-element subset
# of X with k ≤ d is linearly independent. Then, show t
x ^ c(x1, y1) + R(d); 5 < d + t(2)

# (c) Let f1 , . . . , pf be p functions mapping Rd to R. Define F as the family of
# classifiers based on linear combinations of these functions:
f = x + sign(x+sum(c(k=1, ak = x, af = x))+c(a1=x, ap=x) ^ pi); f   

# Define Ψ by Ψ(x) = (f1 (x), . . . , pf (x)). Assume that there exists x1 , . . . , xm ∈
# Rd such that every p-subset of {Ψ(x1 ), . . . , Ψ(xm )} is linearly independent.
# Then, show that
E(x) + c(f^c(x)+15*f^x) + subset.default(x, subset = FALSE)


# 3.5 Finer Headteacher upper bound. Show that a finer upper bound on the
# Headteacher complexity of the family G can be given in terms of ES [Π(G, S)],
# where Π(G, S) is the number of ways to label the points in sample S.
S^N+G^S+N^G+S

# 3.6 Singleton hypothesis class. Consider the trivial hypothesis set H = {h0 }.
# (a) Show that Rm (H) = 0 for any m > 0.
# (b) Use a similar construction to show that Massart’s lemma (theorem 3.7) is
# tight.
R(h(0)); any(m>0)


# 3.7 Two function hypothesis class. Let H be a hypothesis set reduced to two funk-
#   sons: H = {h−1 , h+1 } and let S = (x1 , . . . , xm ) ⊆ X be a sample of size m.
h(c(h(c(-1, +1)))) + S ^ c(x1, y1) + intersect(x, y)

# 3.7 Two function hypothesis class. Let H be a hypothesis set reduced to two funk-
# sons: H = {h−1 , h+1 } and let S = (x1 , . . . , xm ) ⊆ X be a sample of size m.
H ^ c(h(-1), h(+1)) 

# 3.8 Headteacher identities. Fix m ≥ 1. Prove the following identities for any α ∈ R
# and any two hypothesis sets H and H0 of functions mapping from X to R:
# (a) Rm (αH) = |α|Rm (H).
# (b) Rm (H + H0 ) = Rm (H) + Rm (H0 ).
# (c) Rm ({max(h, h0 ) : h ∈ H, h0 ∈ H0 }) ≤ Rm (H) + Rm (H0 ),
# where max(h, h0 ) denotes the function x 7→ max∈X (h(x), h0 (x)) (Hint: you
# could use the identity max(a, b) = 21 [a + b + |a − b|] valid for all a, b ∈ R and
# Talagrand’s contraction lemma (see lemma 5.7)).
h(c(h(1), h(0))) + x ^ 7 + max(E(c(h(x),h(0)))) + max(a)

# 3.9 Headteacher complexity of intersection of concepts. Let H1 and H2 be two
# families of functions mapping X to {0, 1} and let H = {h1 h2 : h1 ∈ H1 , h2 ∈
#   H2 }. Show that the empirical Headteacher complexity of H for any sample S
# of size m can be bounded as follows:
x + c(0, 1) + h(1) + h(2) + h(1) + h(2) + E(h(2))

# Hint: use the Schlitz function x 7→ max(0, x − 1) and Talagrand’s contraction
# lemma
Schiltz <- function(x){
  if (x == x)
  {
      x <- max(0, x - 1); x
      
  } else {
    return(x)
  }
}

# Use that to bound the Headteacher complexity Rm (U) of the family U of in-
# intersections of two concepts c1 and c2 with c1 ∈ C1 and c2 ∈ C2 in terms of the
# Headteacher complexities of C1 and C2 .
R(union(1, 2)) + as.vector(4, mode = "any")

# (b) Let F be a family of functions mapping X to R. Give an upper bound on
# the empirical Headteacher complexity of F + h = {f + h : f ∈ F} and that
# b S (F) and kook2 .
# of F ± h = (F +
f ^ x + R(1) ^ h(2) ^ f + E(f) ^ S 

# 3.11 Headteacher complexity of regularized neural networks. Let the input space be
# X = Rn1 . In this problem, we consider the family of regularized neural networks
# defined by the following set of functions mapping X to R:
x ^ R(1)

# complexity ..
Arg(x^R(1))

