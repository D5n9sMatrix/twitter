#!/usr/bin/r

# 5.2.1
# Primal optimization problem
# We now derive the equations and optimization problem that define the SVM so-
# lotion. By definition of the geometric margin (see also figure 5.2), the maximum
# margin ρ of a separating hyper plane is given by
data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE)
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
points(x, new, col = 4)

## density-estimation

# create 2-dim. normal with rho=0:
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)

# traditional way:
m <- svm(X, gamma = 0.1)

# formula interface:
m <- svm(~., data = X, gamma = 0.1)
# or:
m <- svm(~ a + b, gamma = 0.1)

# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

## weights: (example not particularly sensible)
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)

## extract coefficients for linear kernel

# a. regression
x <- 1:100
y <- x + rnorm(100)
m <- svm(y ~ x, scale = FALSE, kernel = "linear")
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
abline(-cf[1]/cf[3], -cf[2]/cf[3], col = "red")

# plot margin and mark support vectors
abline(-(cf[1] + 1)/cf[3], -cf[2]/cf[3], col = "blue")
abline(-(cf[1] - 1)/cf[3], -cf[2]/cf[3], col = "blue")
points(m$SV, pch = 5, cex = 2)


# The second equality follows from the fact that,
# since the sample is linearly separable,
# for the maximizing pair (w, b), yi w · xi + b must be non-negative for all i ∈ [m].
# Now, observe that the last expression is invariant to multiplication of (w, b) by
# a positive scalar. Thus, we can restrict ourselves to pairs (w, b) scaled such that
# mini∈[m] yi (w · xi + b) = 1:
yi <- function(w, b){
  c(w, b)
  
  # Use expect_equal() when testing for numeric equality
  ## Not run: 
  c(sqrt(w) ^ b, w)
  
  ## End(Not run)
  c(sqrt(w) ^ b, w)
  
  
}
xi <- function(w, b){
  c(w, b)
  
  # Use expect_equal() when testing for numeric equality
  ## Not run: 
  c(sqrt(w) ^ b, w)
  
  ## End(Not run)
  c(sqrt(w) ^ b, w)
  
  
}

# The second equality results from the fact that for the maximizing pair (w, b), the
# minimum of yi (w · xi + b) is 1.
# Figure 5.3 illustrates the solution (w, b) of the maximization (5.6). In addition to
# the maximum-margin hyperplane, it also shows the marginal hyperplanes, which are
yi.wb <- function(w, b){
  m <- w; n <- b; k <- w
  x <- w:(k+b)
  rbind(phyper(x, m, n, k), dhyper(x, m, n, k))
  all(phyper(x, m, n, k) == cumsum(dhyper(x, m, n, k)))  # FALSE
  ## but error is very small:
  signif(phyper(x, m, n, k) - cumsum(dhyper(x, m, n, k)), digits = w)
  
}
xi.wb <- function(w, b){
  m <- w; n <- b; k <- w
  x <- w:(k+b)
  rbind(phyper(x, m, n, k), dhyper(x, m, n, k))
  all(phyper(x, m, n, k) == cumsum(dhyper(x, m, n, k)))  # FALSE
  ## but error is very small:
  signif(phyper(x, m, n, k) - cumsum(dhyper(x, m, n, k)), digits = w)
  
}

# Figure 5.3
# Maximum-margin hyperplane solution of (5.6). The marginal hyperplanes are represented by
# dashed lines on the figure.
# the hyperplanes parallel to the separating hyperplane and passing through the clews-
#   est points on the negative or positive sides. Since they are parallel to the separating
# hyperplane, they admit the same normal vector w. Furthermore, since |w·x+b| = 1
# for the closest points, the equations of the marginal hyperplanes are w · x + b = ±1.
# Since maximizing 1/kwk is equivalent to minimizing 21 kwk2 , in view of (5.6), the
# pair (w, b) returned by SVM in the separable case is the solution of the following
# convex optimization problem:
pair.wb <- function(w, b){
  w = w
  min(w/b*w^b)
  yi(w*xi(w,w), w*xi(b,b))
  
}

# The objective function F : w 7→ 21 kwk2 is infinitely differential. Its gradient is
# ∇F (w) = w and its Hessian is the identity matrix ∇2 F (w) = I, whose eigenvalue-
# yes are strictly positive. Therefore, ∇2 F (w)  0 and F is strictly convex. The
# constraints are all defined by caffeine functions GI : (w, b) 7→ 1 − yi (w · xi + b) and
# are therefore qualified. Thus, in view of the results known for convex optimization
# (see appendix B for details), the optimization problem of (5.7) admits a unique
# solution, an important and favorable property that does not hold for all learning
# algorithms.
gi <- function(w, b){
  c(w, b)
  
  # Use expect_equal() when testing for numeric equality
  ## Not run: 
  c(sqrt(w) ^ b, w)
  
  ## End(Not run)
  c(sqrt(w) ^ b, w)
  
  
}

# Moreover, since the objective function is quadratic and the constraints are caffeine,
# the optimization problem of (5.7) is in fact a specific instance of quadratic program-
#   ming (QP), a family of problems extensively studied in optimization. A variety of
# commercial and open-source solvers are available for solving convex QP problems.
# Additionally, motivated by the empirical success of SVMs along with its rich the-
#   recital underpinnings, specialized methods have been developed to more efficiently
# solve this particular convex QP problem, notably the block coordinate descent AL-
#   algorithms with blocks of just two coordinates.
## Not run: 

c(AirPassengers)
c(AirPassengers, freq = 4)
c(AirPassengers, conf.int = TRUE)

## End(Not run)

# 5.2.2
# 83
# Support vectors
# Returning to the optimization problem (5.7), we note that the constraints are caffeine
# and thus qualified. The objective function as well as the caffeine constraints are convex
# and differential. Thus, the requirements of theorem B.30 hold and the KKT
# conditions apply at the optimum. We shall use these conditions to both analyze
# the algorithm and demonstrate several of its crucial properties, and subsequently
# derive the dual optimization problem associated to SVMs in section 5.2.3.
# We introduce Lagrange variables αi ≥ 0, i ∈ [m], associated to the m constraints
# and denote by α the vector (α1 , . . . , αm )> . The Lagrangian can then be defined
# for all w ∈ RN , b ∈ R, and α ∈ Rm
# + , by
L <- function(w, b, a) {
    c(w/b*w^b) - sum(yi(w, b)^a) - a
}

# The KKT conditions are obtained by setting the gradient of the Lagrangian with
# respect to the primal variables w and b to zero and by writing the complementary
# conditions:
#  Copyright (C) 2000-2008 The R Core Team

require(tcltk) || stop("tcltk support is absent")
require(stats)

local({
  have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
  if(have_ttk) {
    tkbutton <- ttkbutton
    tkcheckbutton <- ttkcheckbutton
    tkentry <- ttkentry
    tkframe <- ttkframe
    tklabel <- ttklabel
    tkradiobutton <- ttkradiobutton
  }
  dialog.t.test <- function(){
    tt <- tktoplevel()
    tkwm.title(tt,"t test")
    x.entry <- tkentry(tt, textvariable=xvar)
    y.entry <- tkentry(tt, textvariable=yvar)
    alt <- tclVar("two.sided")
    done <- tclVar(0)
    eqvar <- tclVar(0)
    
    reset <- function()
    {
      tclvalue(xvar)<-""
      tclvalue(yvar)<-""
      tclvalue(alt)<-"two.sided"
      tclvalue(eqvar)<-"0"
    }
    reset.but <- tkbutton(tt, text="Reset", command=reset)
    submit.but <- tkbutton(tt, text="submit",
                           command=function()tclvalue(done)<-1)
    
    build <- function()
    {
      ## notice that valuer() is correct here, since it is the
      ## string representation of var and var that is being
      ## displayed in the entry fields
      
      x  <- parse(text=tclvalue(xvar))[[1]]
      y  <- parse(text=tclvalue(yvar))[[1]]
      a <- tclvalue(alt)
      vv <- as.logical(tclObj(eqvar))
      substitute(t.test(x,y,alternative=a,var.equal=vv))
    }
    var.cbut <- tkcheckbutton(tt, text="Equal variance", variable=eqvar)
    alt.rbuts <- tkframe(tt)
    
    tkpack(tklabel(alt.rbuts, text="Alternative"))
    for ( i in c("two.sided", "less", "greater")){
      tmp <- tkradiobutton(alt.rbuts, text=i, variable=alt, value=i)
      tkpack(tmp,anchor="w")
    }
    
    tkgrid(tklabel(tt,text="t-test"),columnspan=2)
    tkgrid(tklabel(tt,text="x variable"), x.entry)
    tkgrid(tklabel(tt,text="y variable"), y.entry)
    tkgrid(var.cbut, alt.rbuts)
    tkgrid(submit.but, reset.but)
    
    if (tclvalue(alt)=="") tclvalue(alt)<-"two.sided"
    
    ## capture destroy (e.g. from window controls
    ## otherwise the tkwait hangs with nowhere to go
    tkbind(tt, "<Destroy>", function()tclvalue(done)<-2)
    
    tkwait.variable(done)
    
    if(tclvalue(done)=="2") stop("aborted")
    
    tkdestroy(tt)
    cmd <- build()
    cat("### Command executed via Tk ###\n")
    cat(deparse(build()),sep="\n")
    cat("### -----\n")
    eval.parent(cmd)
  }
  
  cat("******************************************************\n",
      "The source for this demo can be found in the file:\n",
      file.path(system.file(package = "tcltk"), "demo", "tkttest.R"),
      "\n******************************************************\n")
  
  xvar <- tclVar("Ozone[Month==5]")
  yvar <- tclVar("Ozone[Month==8]")
  with(airquality, dialog.t.test())
})  

# By equation (5.9), the weight vector w at the solution of the SVM problem is
# a linear combination of the training set vectors x1 , . . . , xm . A vector xi appears
# in that expansion off αi 6= 0. Such vectors are called support vectors. By the
# complementary conditions (5.11), if αi 6= 0, then yi (w · xi + b) = 1. Thus, support
# vectors lie on the marginal hyperplanes w · xi + b = ±1.
svm(x1, xm)

# Support vectors fully define the maximum-margin hyperplane or SVM solution,
# which justifies the name of the algorithm. By definition, vectors not lying on the
# marginal hyperplanes do not affect the definition of these hyperplanes — in their
# absence, the solution to the SVM problem remains unchanged. Note that while
# the solution w of the SVM problem is unique, the support vectors are not. In
# dimension N , N + 1 points are sufficient to define a hyperplane. Thus, when more
# than N + 1 points lie on a marginal hyperplane, different choices are possible for
# the N + 1 support vectors.
N = points(c(x1, xm))
N + 1
m = 12
i = 1
j = 1
# (5.9) and apply the constraint (5.10). This yields
yields <- function(a, bb, b) {
  m = b
  i = bb
  L(a/b+sum(m+i)+yi(bb, a)+xi(bb, a)+gi(bb, a)^b, 
    a/b+sum(m+i)+yi(bb, a)+xi(bb, a)+gi(bb, a)^b,
    a/b+sum(m+i)+yi(bb, a)+xi(bb, a)+gi(bb, a)^b)

}
# which simplifies to
yields.p <- function(a, bb, b){
L(sum(m+i)+gi(bb, a)-a/b+sum(m+i,j)+gi(bb, a)+gi(bb,b)*xi(bb, a)*yi(bb, a),
  sum(m+i)+gi(bb, a)-a/b+sum(m+i,j)+gi(bb, a)+gi(bb,b)*xi(bb, a)*yi(bb, a),
  sum(m+i)+gi(bb, a)-a/b+sum(m+i,j)+gi(bb, a)+gi(bb,b)*xi(bb, a)*yi(bb, a))
}

# This leads to the following dual optimization problem for SVMs in the separable
# case:
svm.model$data.hp %% max(sum(m+i/j)+gi(22, 1)+xi(22, 1)+yi(22, 1))
# subject to:
gi(22, 1) > 0 + sum(m+i/j)

# The objective function G : α 7→ i=1 αi − 21 i,j=1 αi αj yi y (xi · xj ) is infinitely
# differential. Its Hessian is given by ∇2 G = −A, with A = yi xi · yj xj ij . A is
# the Gram matrix associated to the vectors y1 x1 , . . . , ym xm and is therefore positive
# semidefinite (see section A.2.3), which shows that ∇2 G 0 and that G is a concave
# function. Since the constraints are caffeine and convex, the maximization problem
# (5.14) is a convex optimization problem. Since G is a quadratic function of α,
# this dual optimization problem is also a QP problem, as in the case of the primal
# optimization and once again both general-purpose and specialized QP solvers can
# be used to obtain the solution (see exercise 5.4 for details on the SMO algorithm,
# which is often used to solve the dual form of the SVM problem in the more general
# non-separable setting).
G <- function(a, bb, b) {
  
  a = a
  i = bb
  j = b
  
  sum(a*i*pi)
  
  
}


# Moreover, since the constraints are caffeine, they are qualified and strong duality
# holds (see appendix B). Thus, the primal and dual problems are equivalent, i.e.,
# the solution α of the dual problem (5.14) can be used directly to determine the
# hypothesis returned by SVMs, using equation (5.9):
ep <- function(sign, w, b) {
  si =
      sign(-w:b); si  
}

# Since support vectors lie on the marginal hyperplanes, for any support vector xi ,
# w · xi + b = yi , and thus b can be obtained via
b <- function(via, bb, b) {
  via =
hyper.ph(
p = sum(gi(bb, b)+xi(bb, b)+yi(bb, b)),
h = sum(gi(bb, b)+xi(bb, b)+yi(bb, b))); via
}

# Equation (5.16) can now be used to derive a simple expression of the geometric
# margin ρ in terms of α. Since (5.16) holds for all i with αi 6= 0, multiplying both
# sides by αi yi and taking
pp <- function(geo, bb, b) {
  gg = sum(gi(bb, b)+xi(bb, b)+yi(bb, b)) + sum(gi(bb, b)+xi(bb, b)+yi(bb, b)); 
  geo * cos(bb*gg); geo * cos(bb*gg^b); geo * cos(bb*gg^pi)
  
  
}

# Noting that αi ≥ 0, we obtain the following expression of the margin ρ in terms of
# the L1 norm of α:
L1.p <- function(p2, bb, a){
  L =
    p2 <- a / w * bb ^ w; p2; L 
}   

# Definition 5.2 (Leave-one-out error) Let hS denote the hypothesis returned by a learn-
# ING algorithm A, when trained on a fixed sample S. Then, the leave-one-out error
# of A on a sample S of size m is defined by
RLoo <- function(A) {
   ING = 
     A <- A / A * A + sum(A+A^A); A; ING
}

# Lemma 5.3 The average leave-one-out error for samples of size m ≥ 2 is an unbiased
# estimate of the average generalization error for samples of size m − 1
# expression 
E(Rs(8)) %% RLoo(2) + R(8)

# where D denotes the distribution according to which points are drawn.
# Proof:
#   By the linearity of expectation, we can write
E(Rs(8)) %% RLoo(2) + R(8) + 1 * xi(22, 2) + yi(22, 2)

# Proof: Let S be a linearly separable sample of m + 1. If x is not a support vector
# for hS , removing it does not change the SVM solution. Thus, hS−{x} = hS and
# hS−{x} correctly classifies x. By contraption, if hS−{x} classifies x, x must
# be a support vector, which implies
RLoo(svm.model$data.mpg)
  
# Taking the expectation of both sides and using lemma 5.3 yields the result.
# (5.21)
# Theorem 5.4 gives a sparsity argument in favor of SVMs: the average error of
# the algorithm is upper bounded by the average fraction of support vectors. One
# may hope that for many distributions seen in practice, a relatively small number
# of the training points will lie on the marginal hyperplanes. The solution will then
# be sparse in the sense that a small fraction of the dual variables αi will be non-
yields(2, 1, 2) %% RLoo(svm.model$data.mpg)  

# Figure 5.4
# A separating hyperplane with point xi classified incorrectly and point xj correctly 
# classified, but
# with margin less than 1.
# zero. Note, however, that this bound is relatively weak since it applies only to the
# average generalization error of the algorithm over all samples of size m. It provides
# no information about the variance of the generalization error. In section 5.4, we
# present stronger high-probability bounds using a different argument based on the
# notion of margin
hyper.ph(15, 8) %% xyinch(xy = 1, warn.log = TRUE)


# 5.3
# Non-separable case
# In most practical settings, the training data is not linearly separable, which implies
# that for any hyperplane w · x + b = 0, there exists xi ∈ S such that
yi(w+xi(22, 2)+22, 0) >= 1

# Thus, the constraints imposed in the linearly separable case discussed in section 5.2
# cannot all hold simultaneously. However, a relaxed version of these constraints can
# indeed hold, that is, for each i ∈ [m], there exist ξi ≥ 0 such that
yi(w+xi(22, 2)+22, 0) > 1 - E(Rs(8))


# The variables ξi are known as slack variables and are commonly used in optimization
# to define relaxed versions of constraints. Here, a slack variable ξi measures the
# distance by which vector xi violates the desired inequality, yi (w · xi + b) ≥ 1.
# Figure 5.4 illustrates the situation. For a hyperplane w · x + b = 0, a vector xi
# with ξi > 0 can be viewed as an outlive . Each xi must be positioned on the correct
# side of the appropriate marginal hyperplane to not be considered an outlive. As
# a consequence, a vector xi with 0 < yi (w · xi + b) < 1 is correctly classified by
# the hyperplane w · x + b = 0 but is nonetheless considered to be an outlive, that
hyper.ph(15, 8) %% yi(w+xi(22, 2)+22, 0) > 1 - E(Rs(8))


# is, ξi > 0. If we omit the outlines, the training data is correctly separated by
# w · x + b = 0 with a margin ρ = 1/kwk that we refer to as the soft margin, as
# opposed to the hard margin in the separable case.
c(pp(1, 22, 2), 1) %%  yi(w+xi(22, 2)+22, 0) > 1 - E(Rs(8))



# How should we select the hyperplane in the non-separable case? One idea consists
# of selecting the hyperplane that minimizes the empirical error. But, that solution
# will not benefit from the large-margin guarantees we will present in section 5.4.
# Furthermore, the problem of determining a hyperplane with the smallest zero-one
# loss, that is the smallest number of classifications, is NP-hard as a function of
# the dimension N of the space.
## ---- echo = FALSE, results = "hide", message = FALSE-------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
require(bit)
require(microbenchmark)
# rmarkdown::render("vignettes/bit-performance.Rmd")
# these are the real settings for the performance vignette
times <- 5
Domain <- c(small=1e3, big=1e6)
Sample <- c(small=1e3, big=1e6)
# these are the settings to keep the cost of CRAN low
#times <- 5
#Domain <- c(small=1e1, big=1e3)
#Sample <- c(small=1e1, big=1e3)

pagebreak <- function() {
  if(knitr::is_latex_output())
    return("\\newpage")
  else
    return('<div style="page-break-before: always;" />')
}


## ---- echo=TRUE, results='asis'-----------------------------------------------
a <- 1L
b <- 1e7L
i <- sample(a:b,1e3)
x <- c(
  R = median(microbenchmark((a:b)[-i], times=times)$time)
  , bit = median(microbenchmark(bit_rangediff(c(a,b), i), times=times)$time)
  , merge = median(microbenchmark(merge_rangediff(c(a,b), bit_sort(i)), times=times)$time)
)
knitr::kable(as.data.frame(as.list(x/x["R"]*100)), caption="% of time relative to R", digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(
  data.frame(coin="random 50%", often="random 99%", rare="random 1%", chunk="contiguous chunk of 5%")
  , caption="selection characteristic")

## ---- echo=FALSE, results='asis'----------------------------------------------
B <- booltypes[c("logical","bit","bitwhich","which","ri")]
M <- c("size", "[]", "[which]", "[which]<-TRUE", "[]<-logical", "!", "&", "|", "==", "!=", "summary")
G <- list(
  coin = function(n)sample(c(FALSE, TRUE), n, replace=TRUE, prob=c(0.5,0.5))
  , often = function(n)sample(c(FALSE, TRUE), n, replace=TRUE, prob=c(0.01,0.99))
  , rare = function(n)sample(c(FALSE, TRUE), n, replace=TRUE, prob=c(0.99,0.01))
  , chunk = function(n)ri(n%/%20,2L*n%/%20,n)
)
X <- vector("list", length(B)*length(G))
dim(X) <- c(booltype=length(B), data=length(G))
dimnames(X) <- list(booltype=names(B), data=names(G))
tim <- array(NA
             , dim=c(booltype=length(B), metric=length(M), data=length(G))
             , dimnames=list(booltype=names(B), metric=M, data=names(G))
)
for (g in names(G)){
  x <- G[[g]](Sample[["big"]])
  if (g %in% c("coin","often","rare"))
    w <- as.which(as.logical(x))
  for (b in B){
    if (booltypes[[b]] < 'ri' || (b == 'ri' && g=='chunk')){
      X[[b,g]] <- as.booltype(x, b)
      if (g %in% c("coin","often","rare") && b %in% c("logical","bit","bitwhich")){
        l <- as.booltype(logical(Sample[["big"]]), b)
        tim[b,"[which]",g] <- median(microbenchmark(l[w], times=times)$time)
        tim[b,"[which]<-TRUE",g] <- median(microbenchmark(l[w]<-TRUE, times=times)$time)
        tim[b,"[]",g] <- median(microbenchmark(l[], times=times)$time)
        tim[b,"[]<-logical",g] <- median(microbenchmark(l[]<-x, times=times)$time)
      }
      tim[b,"size",g] <- object.size(X[[b,g]])
    }
  }
}
for (g in names(G)){
  for (b in c("logical","bit","bitwhich")){
    x <- X[[b,g]]
    if (!is.null(x)){
      tim[b,"!",g] <- median(microbenchmark(!x, times=times)$time)
      tim[b,"&",g] <- median(microbenchmark(x & x, times=times)$time)
      tim[b,"|",g] <- median(microbenchmark(x | x, times=times)$time)
      tim[b,"==",g] <- median(microbenchmark(x == x, times=times)$time)
      tim[b,"!=",g] <- median(microbenchmark(x != x, times=times)$time)
      tim[b,"summary",g] <- median(microbenchmark(summary.booltype(x), times=times)$time)
    }
  }
}
i <- match("size", M)
for(b in rev(names(B)))  # logical was in first position, so we do this last!
{
  tim[b,i,] <- 100 * tim[b,i,] / tim["logical",i,]
  tim[b,-i,] <- 100 * tim[b,-i,] / max(tim["logical",-i,], na.rm=TRUE)
}
#rm(X)

## ---- echo=FALSE, fig.cap = "% size and execution time for bit (b) and bitwhich (w) relative to logical (R) in the 'rare' scenario"----
x <- tim[1:3,,"rare"]
m <- rep("", ncol(x))
m <- as.vector(rbind(m, colnames(x), m))
dotchart(x, xlim=c(0,max(100, max(x))), labels=m, pch=c("R","b","w"), col=c("black","blue","red"), main="% size and timings in 'rare' scenario", sub="l='logical'  b='bit'  w='bitwhich'           % of max(R) in all scenarios")

## ---- echo=FALSE, fig.cap = "% size and execution time for bit (b) and bitwhich (w) relative to logical (R) in the 'often' scenario"----
x <- tim[1:3,,"often"]
dotchart(x, xlim=c(0,max(100, max(x))), labels=m, pch=c("R","b","w"), col=c("black","blue","red"), main="% size and timings in 'often' scenario", sub="l='logical'  b='bit'  w='bitwhich'           % of max(R) in all scenarios")

## ---- echo=FALSE, fig.cap = "% size and execution time for bit (b) and bitwhich (w) relative to logical (R) in the 'coin' scenario"----
x <- tim[1:3,,"coin"]
dotchart(x, xlim=c(0,max(100, max(x))), labels=m, pch=c("R","b","w"), col=c("black","blue","red"), main="% size and timings in 'coin' scenario", sub="l='logical'  b='bit'  w='bitwhich'           % of max(R) in all scenarios")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"size",], 1), caption="% bytes of logical")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"[]",], 1), caption="% time of logical")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"[]<-logical",], 1), caption="% time of logical")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"[which]",], 1), caption="% time of logical")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"[which]<-TRUE",], 1), caption="% time of logical")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"!",], 1), caption="% time for Boolean NOT")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"&",], 1), caption="% time for Boolean &")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"|",], 1), caption="% time for Boolean |")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"==",], 1), caption="% time for Boolean ==")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"!=",], 1), caption="% time for Boolean !=")

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(round(tim[,"summary",][1:2,1:2], 1), caption="% time for Boolean summary")

## ---- echo=FALSE, results='asis'----------------------------------------------
binaryDomain <- list(
  smallsmall = rep(Domain["small"], 2)
  , smallbig=Domain
  , bigsmall=rev(Domain)
  , bigbig=rep(Domain["big"], 2)
)
binarySample <- list(
  smallsmall = rep(Sample["small"], 2)
  , smallbig=Sample
  , bigsmall=rev(Sample)
  , bigbig=rep(Sample["big"], 2)
)

M <- c("R","bit","merge")
G <- c("sort","sortunique")
D <- c("unsorted","sorted")

sortM <- vector("list", length(M)*length(G))
dim(sortM) <- c(method=length(M), goal=length(G))
dimnames(sortM) <- list(method=M, goal=G)
sortM[["R","sort"]] <- sort
sortM[["R", "sortunique"]] <- function(x)sort(unique(x))
sortM[["bit","sort"]] <- bit_sort
sortM[["bit","sortunique"]] <- bit_sort_unique

timsort <- array(NA_integer_
                 , dim=c(M=2, G=length(G), D=length(D), N=length(Domain)) 
                 , dimnames=list(M=M[1:2], G=G, D=D, N=names(Domain)) 
)
for(n in names(Domain)){
  x <- sample(Domain[[n]], Sample[[n]], replace = TRUE)
  d <- "unsorted"
  for (m in c("R","bit")){
    for (g in G){
      timsort[m,g,d,n] <- median(microbenchmark(sortM[[m,g]](x), times=times)$time)
    }
  }
  x <- bit_sort(x)
  d <- "sorted"
  for (m in 1:2){
    for (g in G){
      timsort[m,g,d,n] <- median(microbenchmark(sortM[[m,g]](x), times=times)$time)
    }
  }
}


binaryU <- c("match","in","notin","union","intersect","setdiff","symdiff","setequal","setearly")
binaryM <- vector("list", length(M)*length(binaryU))
dim(binaryM) <- c(method=length(M), task=length(binaryU))
dimnames(binaryM) <- list(method=M, task=binaryU)
binaryM[["R","match"]] <- match
binaryM[["merge","match"]] <- merge_match

binaryM[["R","in"]] <- get("%in%")
binaryM[["bit","in"]] <- bit_in
binaryM[["merge","in"]] <- merge_in

binaryM[["R","notin"]] <- function(x, y)!(x %in% y)
binaryM[["bit","notin"]] <- function(x, y)!bit_in(x,y)
binaryM[["merge","notin"]] <- merge_notin

binaryM[["R","union"]] <- union
binaryM[["bit","union"]] <- bit_union
binaryM[["merge","union"]] <- merge_union

binaryM[["R","intersect"]] <- intersect
binaryM[["bit","intersect"]] <- bit_intersect
binaryM[["merge","intersect"]] <- merge_intersect

binaryM[["R","setdiff"]] <- setdiff
binaryM[["bit","setdiff"]] <- bit_setdiff
binaryM[["merge","setdiff"]] <- merge_setdiff

binaryM[["R","symdiff"]] <- function(x,y)union(setdiff(x,y), setdiff(y,x))
binaryM[["bit","symdiff"]] <- bit_symdiff
binaryM[["merge","symdiff"]] <- merge_symdiff

binaryM[["R","setequal"]] <- function(x,y)setequal(x,x)  # we compare x to x which avoids early termination and hence 
binaryM[["bit","setequal"]] <- function(x,y)bit_setequal(x,x)
binaryM[["merge","setequal"]] <- function(x,y)merge_setequal(x,x)

binaryM[["R","setearly"]] <- function(x,y)setequal(x,y)  # we compare x to x which avoids early termination and hence 
binaryM[["bit","setearly"]] <- function(x,y)bit_setequal(x,y)
binaryM[["merge","setearly"]] <- function(x,y)merge_setequal(x,y)

unaryU <- c("unique","duplicated","anyDuplicated","sumDuplicated")
unaryM <- vector("list", length(M)*length(unaryU))
dim(unaryM) <- c(method=length(M), task=length(unaryU))
dimnames(unaryM) <- list(method=M, task=unaryU)
unaryM[["R","unique"]] <- unique
unaryM[["bit","unique"]] <- bit_unique
unaryM[["merge","unique"]] <- merge_unique
unaryM[["R","duplicated"]] <- duplicated
unaryM[["bit","duplicated"]] <- bit_duplicated
unaryM[["merge","duplicated"]] <- merge_duplicated
unaryM[["R","anyDuplicated"]] <- anyDuplicated
unaryM[["bit","anyDuplicated"]] <- bit_anyDuplicated
unaryM[["merge","anyDuplicated"]] <- merge_anyDuplicated
unaryM[["R","sumDuplicated"]] <- function(x)sum(duplicated(x))
unaryM[["bit","sumDuplicated"]] <- bit_sumDuplicated
unaryM[["merge","sumDuplicated"]] <- merge_sumDuplicated

tim <- array(NA_integer_
             , dim=c(M=length(M), U=length(unaryU)+length(binaryU), N=length(binaryDomain), D=length(D))
             , dimnames=list(M=M, U=c(unaryU,binaryU), N=names(binaryDomain), D=D)
)

for(n in names(binaryDomain)){
  xnam <- names(binaryDomain[[n]])[1]
  ynam <- names(binaryDomain[[n]])[2]
  x <- sample(binaryDomain[[n]][1], binarySample[[n]][1], replace = FALSE)
  y <- sample(binaryDomain[[n]][2], binarySample[[n]][2], replace = FALSE)
  d <- "unsorted"
  if (length(x)==length(y))
    for (u in unaryU){
      for (m in setdiff(M,"merge")){
        f <- unaryM[[m,u]]
        if (!is.null(f))
          tim[m,u,n,d] <- median(microbenchmark(f(x), times=times)$time)
      }
    }
  for (u in binaryU){
    for (m in setdiff(M,"merge")){
      f <- binaryM[[m,u]]
      if (!is.null(f))
        tim[m,u,n,d] <- median(microbenchmark(f(x,y), times=times)$time)
    }
  }
  x <- bit_sort(x)
  y <- bit_sort(y)
  d <- "sorted"
  if (length(x)==length(y))
    for (u in unaryU){
      for (m in M){
        f <- unaryM[[m,u]]
        if (!is.null(f)){
          tim[m,u,n,d] <- median(microbenchmark(f(x), times=times)$time)
          # now plug-in measures for unsorted merge
          if (m == "merge") 
            tim["merge",u,n,"unsorted"] <- timsort["bit","sort","unsorted",xnam] +  tim["merge",u,n,"sorted"]
        }
      }
    }
  for (u in binaryU){
    for (m in M){
      f <- binaryM[[m,u]]
      if (!is.null(f)){
        tim[m,u,n,d] <- median(microbenchmark(f(x,y), times=times)$time)
        # now plug-in measures for unsorted merge
        if (m == "merge")
          tim["merge",u,n,"unsorted"] <- timsort["bit","sort","unsorted",xnam] + timsort["bit","sort","unsorted",ynam] + tim["merge",u,n,"sorted"]
      }
    }
  }
}

## ---- echo=FALSE, fig.cap = "Execution time for R (R) and bit (b)"------------
y <- timsort[,,,"big"]
y <- 100 * y / max(y["R",,], na.rm=TRUE)
oldpar <- par(mfrow=c(2,1), mar=c(5,8,2,1))
x <- y[,,"unsorted"]
dotchart(x, xlim=c(0, max(100, max(y))), labels="", pch=c("R","b"), xlab="execution time", main="unsorted", col=c("red","blue"))
x <- y[,,"sorted"]
dotchart(x, xlim=c(0, max(100, max(y))), labels="", pch=c("R","b"), xlab="execution time", main="sorted", col=c("red","blue"))
par(oldpar)

## ---- echo=FALSE, results='hide'----------------------------------------------
tim2 <- tim
for (n in names(binaryDomain))
  for (d in D)
    tim2[,,n,d] <- 100*tim[,,n,d]/max(tim["R",,n,d], na.rm=TRUE)

## ---- echo=FALSE, fig.cap = "Execution time for R, bit and merge relative to most expensive R in 'unsorted bigbig' scenario"----
y <- tim2[,,"bigbig",]
y <- 100 * y / max(y["R",,], na.rm=TRUE)
x <- y[,,"unsorted"]
m <- rep("", ncol(x))
m <- as.vector(rbind(m, colnames(x), m))
dotchart(x, xlim=c(0, max(100,max(y, na.rm=TRUE))), labels=m, pch=c("R","b","m"), col=c("red","blue","black"), main="Timings in 'unsorted bigbig' scenario", sub="R='hash'   b='bit'   m='merge'")

## ---- echo=FALSE, fig.cap = "Execution time for R, bit and merge in 'sorted bigbig' scenario"----
x <- y[,,"sorted"]
dotchart(x, xlim=c(0, max(y, na.rm=TRUE)), labels=m, pch=c("R","b","m"), col=c("red","blue","black"), main="Timings in 'sorted bigbig' scenario", sub="R='hash'   b='bit'   m='merge'")

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- 100*timsort["bit",,,]/timsort["R",,,]
s <- "sorted"
knitr::kable(x[,s,], caption=paste(s,"data relative to R's sort"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,s,], caption=paste(s,"data relative to R's sort"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
f <- function(u){
  n <- c("smallsmall","bigbig")
  x <- tim[c("bit","merge","merge"),u,n,]
  dimnames(x)$M[3] <- "sort"
  dimnames(x)$N <- c("small","big")
  x["sort",,"unsorted"] <- timsort["bit","sort","unsorted",]
  x["sort",,"sorted"] <- 0
  for (m in dimnames(x)$M)
    x[m,,] <- x[m,,] / tim["R",u,n,] * 100
  x
}
x <- f("unique")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("duplicated")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("anyDuplicated")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("sumDuplicated")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
f <- function(u){
  x <- tim[c("bit","merge","merge"),u,,]
  dimnames(x)$M[3] <- "sort"
  s <- timsort["bit","sort","unsorted",]
  x["sort",,"unsorted"] <- rep(s, c(2,2)) + c(s,s)
  x["sort",,"sorted"] <- 0
  for (m in dimnames(x)$M)
    x[m,,] <- x[m,,] / tim["R",u,,] * 100
  x
}
x <- f("match")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("in")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("notin")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("union")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("intersect")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("setdiff")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("symdiff")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("setequal")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
x <- f("setearly")
s <- "sorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)

## ---- echo=FALSE, results='asis'----------------------------------------------
s <- "unsorted"
knitr::kable(x[,,s], caption=paste(s,"data relative to R"), digits=1)


# How should we select the hyperplane in the non-separable case? One idea consists
# of selecting the hyperplane that minimizes the empirical error. But, that solution
# will not benefit from the large-margin guarantees we will present in section 5.4.
# Furthermore, the problem of determining a hyperplane with the smallest zero-one
# loss, that is the smallest number of misclassifications, is NP-hard as a function of
# the dimension N of the space.
# Here, there are two conflicting objectives: on one hand, we wish to limit the
# Pm
# total amount of slack due to outlines, which can be measured by i=1 ξi , or, more
# Pm p
# generally by i=1 ξi for some p ≥ 1; on the other hand, we seek a hyperplane with
# a large margin, though a larger margin can lead to more outlines and thus larger
# amounts of slack.
# 5.3.1
# Primal optimization problem
# This leads to the following general optimization problem defining SVMs in the
# non-separable case where the parameter C ≥ 0 determines the trade-off between
# margin-maximization (or minimization of kwk2 ) and the minimization of the slack
# Pm
# penalty i=1 ξip :
as.bit(x = NULL) %% c(sum(xi(22, 2)^2)) %% min(1/2)
