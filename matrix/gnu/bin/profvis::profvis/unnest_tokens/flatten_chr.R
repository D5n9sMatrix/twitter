#!/usr/bin/r
# or the indicator function of these points. In such cases, we will say in short that
# the concept to learn is a triangle. A concept class is a set of concepts we may wish
# to learn and is denoted by C. This could, for example, be the set of all triangles in
# the plane.
# We assume that examples are independently and identically distributed (i.i.d.)
# according to some fixed but unknown distribution D. The learning problem is
# then formulated as follows. The learner considers a fixed set of possible concepts
# H, called a hypothesis set, which might not necessarily coincide with C. It re-
#   ceives a sample S = (x1 , . . . , xm ) drawn i.i.d. according to D as well as the labels
# (c(x1 ), . . . , c(xm )), which are based on a specific target concept c ∈ C to learn. The
# task is then to use the labeled sample S to select a hypothesis HS ∈ H that has a
# small generalization error with respect to the concept c. The generalization error
# of a hypothesis h ∈ H, also referred to as the risk or true error (or simply error )
# of h is denoted by R(h) and defined as follows.1
# Definition 2.1 (Generalization error) Given a hypothesis h ∈ H, a target concept c ∈ C,
# and an underlying distribution D, the generalization error or risk of h is defined by
x <- 1:15
h <- function(x) { return(x) }
P <- function(x) { return(x) } 
E <- function(x) { return(x) }  
  
R <- c(h(x),c(x)) 

# commands buffer full step ...
R ~ full_seq(x)
R ~ full_seq(h(x))
R ~ full_seq(P(x))
R ~ full_seq(E(x))


# where 1ω is the indicator function of the event ω.2
# The generalization error of a hypothesis is not directly accessible to the learner
# since both the distribution D and the target concept c are unknown. However, the
# learner can measure the empirical error of a hypothesis on the labeled sample S.
R

# Definition 2.2 (Empirical error) Given a hypothesis h ∈ H, a target concept c ∈ C, and
# a sample S = (x1 , . . . , xm ), the empirical error or empirical risk of h is defined by
s <- function(x) { return(x) }
m = 12  
R <- 1 / m * m + sum(m) ^ s(x)

# Thus, the empirical error of h ∈ H is its average error over the sample S, while the
# generalization error is its expected error based on the distribution D. We will see in
# this chapter and the following chapters a number of guarantees relating these two
# quantities with high probability, under some general assumptions. We can already
# note that for a fixed h ∈ H, the expectation of the empirical error based on an i.i.d
R

# 1 The choice of R instead of E to denote an error avoids possible confusions 
# with the notation for expectations and is further justified by the fact that 
# the term risk is also used in machine learning
# and statistics to refer to an error.
# 2 For this and other related definitions, the family of functions H and the 
# target concept c must be measurable. The function classes we consider in this 
# book all have this property.
# www.dbooks.org2.1 The PAC learning model.
# 11

# sample S is equal to the generalization error:
Rs <- function(h) { return(h) }
R  <- function(h) { return(h) }
E  <- function(Rs) { return(Rs) }
# able product ...
E1 <- R(100) + E(110) + Rs(220) 
# need, by the linearity of the expectation and the fact that the sample is drawn
# i.i.d., we can write
E2 <- sum(R(100)+1/m+E(110)+Rs(220)*R(100)+1/m+E(110)+Rs(220))
# The following introduces the Probably Approximately Correct (PAC) learning

# framework. Let n be a number such that the computational cost of representing
# any element x ∈ X is at most O(n) and denote by size(c) the maximal cost of the
# computational representation of c ∈ C. For example, x may be a vector in Rn , for
# which the cost of an array-based representation would be in O(n). In addition, let
# HS denote the hypothesis returned by algorithm A after receiving a labeled sample
# S. To keep notation simple, the dependency of HS on A is not explicitly indicated.

ggy <- function(n) { return(n) }
fap <- function(n) { return(n) }

size <- sum(ggy(21), fap(8) + Rs(220) + R(100) + E(110) / P(1:50))

# Definition 2.3 (PAC-learning) A concept class C is said to be PAC-learn able if there
# exists an algorithm A and a polynomial function poly(·, ·, ·, ·) such that for any
# > 0 and δ > 0, for all distributions D on X and for any target concept c ∈ C, the
# following holds for any sample size m ≥ poly(1/, 1/δ, n, size(c)):
#   P [R(HS ) ≤ ] ≥ 1 − δ. S
poly(x, degree = 1, coefs = NULL, raw = FALSE, simple = FALSE)

# If A further runs in poly(1/, 1/δ, n, size(c)), then C is said to be efficiently PAC-
# learn able. When such an algorithm A exists, it is called a PAC-learning algorithm
# for C.
S1 <- poly(1:27, 4)
# loading ...
S1
# A concept class C is thus PAC-learn able if the hypothesis returned by the algorithm
# after observing a number of points polynomial in 1/ and 1/δ is approximately
# correct (error at most ) with high probability (at least 1 − δ), which justifies the
# PAC terminology. The parameter δ > 0 is used to define the confidence 1 − δ
# and  > 0 the accuracy 1 − . Note that if the running time of the algorithm is
# polynomial in 1/ and 1/δ, then the sample size m must also be polynomial if the
# full sample is received by the algorithm.
S2 <- poly(1:21, 4)
# loading read ...
S2

# Several key points of the PAC definition are worth emphasizing. First, the PAC
# framework is a distribution-free model : no particular assumption is made about
# the distribution D from which examples are drawn. Second, the training sample
# and the test examples used to define the error are drawn according to the same
# distribution D. This is a natural and necessary assumption for generalization to
# Demo for shapes
par(ask=TRUE)

##drapecol
persp(volcano,theta = 135, phi = 30, col = drapecol(volcano),main="drapecol,femmecol")

##intpalette
intpalette(c("white","black"),n=10)
grey(seq(1,0,length.out=10))
image(matrix(nrow=1,ncol=100,data=1:100),main="intpalette",
      col=c(c("red","blue"),numcol=100))
image(matrix(nrow=1,ncol=100,data=1:100),main="intpalette",
      col=c(c("red","blue","yellow"),numcol=100))
image(matrix(nrow=1,ncol=100,data=1:100),main="intpalette", 
      col=c(c("red","blue","yellow","green"),numcol=100))
image(matrix(nrow=1,ncol=100,data=1:100),main="intpalette", 
      col=intpalette(c("red","blue","yellow","green","black"),numcol=100))


##shadepalette
c(n=10,"white","black")
image(matrix(nrow=1,ncol=100,data=1:100),col=c(100,"red","blue"),
      main="shadepalette")   
# rotatexy
#----------------
xy <- matrix(ncol=2,data=c(1:5,rep(1,5)))
plot(xy,xlim=c(-6,6),ylim=c(-6,6),type="b",pch=16,main="rotatexy",col=1)
for ( i in 1:5) points(rotatexy(xy,mid=c(0,0),angle=60*i),col=i+1,type="b",pch=16)
points(0,0,cex=2,pch=22,bg="black")

legend("topright",legend=60*(0:5),col=1:6,pch=16,title="angle")
legend("topleft",legend="midpoint",pt.bg="black",pt.cex=2,pch=22,box.lty=0)

x <- seq(0,2*pi,pi/100)
y <- sin(x)
cols <- c(c("blue","green","yellow","red"),n=250)
cols <- c(cols,rev(cols))
plot(x,y,type="l",ylim=c(-3,3),main="rotatexy",col=cols[1],lwd=2,xlim=c(-1,7))
for (i in 2:500) lines(rotatexy(cbind(x,y),angle=0.36*i),col=cols[i],lwd=2)

x <- seq(0,2*pi,pi/100)
y <- sin(x*2)

cols <- c(c("red","yellow","black"),n=250)
cols <- c(cols,rev(cols))
plot(x,y,type="l",ylim=c(-4,5),main="rotatexy, asp=TRUE",col=cols[1],lwd=2,xlim=c(-1,7))
for (i in 2:500) lines(rotatexy(cbind(x,y),angle=0.36*i,asp=TRUE),col=cols[i],lwd=2)


cols <- c(1000)
plot(x,y,xlim=c(-1,1),ylim=c(-1,1),main="rotatexy",col=cols[1],type="n")
for (i in 2:1000) {xy<-rotatexy(c(0,1),angle=0.36*i, mid=c(0,0));
points(xy[1],xy[2],col=cols[i],pch=".",cex=2)}

# ellipses
#----------------
c(c(-1.5,1.5))
cols <- rainbow(10)
SS   <- rev(seq(0.1,1,0.1))
c(rx=1.5,ry=0.6)
c(rx=1.2,ry=0.4)
for (i in 1:length(SS)) plotellipse(1,SS[i],col=cols[i],type="n")
title("plotellipse")
c(rx=1.5,ry=0.6,angle=0,from=pi,to=2*pi,arrow=TRUE,arr.pos=0.5)
c(rx=1.2,ry=0.4,from=pi,to=2*pi)

# 
c(c(-1.,1.))
col  <- c(endcol="darkblue",n=100)
c(rx1=1,ry1=0.8,col=col,lcol="black")
c(rx=1,ry=0.35 ,angle=0,from=pi,to=2*pi,lwd=1,mid=c(0,0.))
c(rx=1.1,ry=0.6 ,angle=0,from=1.28*pi,to=1.72*pi,lwd=1,mid=c(0,-0.1))
c(rx=0.9 ,ry=0.32,angle=0,from=pi,to=2*pi,lwd=1,mid=c(0,0.35))
c(rx=0.65,ry=0.25,angle=0,from=pi,to=2*pi,lwd=1,mid=c(0,0.6))
c(rx=0.3 ,ry=0.1,angle=0,from=pi,to=2*pi,lwd=1,mid=c(0,0.75))

c(rx=0.8,ry=0.0,angle=90,from=pi,to=2*pi,lwd=1)
c(rx=0.8,ry=0.35,angle=90,from=pi,to=2*pi,lwd=1)
c(rx=0.8,ry=0.6,angle=90,from=pi,to=2*pi,lwd=1)
c(rx=0.8,ry=0.8,angle=90,from=pi,to=2*pi,lwd=1)

c(rx=0.8,ry=0.35,angle=-90,from=pi,to=2*pi,lwd=1)
c(rx=0.8,ry=0.6,angle=-90,from=pi,to=2*pi,lwd=1)
c(rx=0.8,ry=0.8,angle=-90,from=pi,to=2*pi,lwd=1)
title("plotellipse, C")

#
c(c(-1,1))
c(rx1=1,rx2=0.5,dr=0.01,col=col)
title("C")

#
c(c(-1,1),c(-1,1))
c(col=col)
title("C")

# 
color <-c(n=50)
dr    <- 0.05
c(xlim=c(-2,2),ylim=c(-2,2),col=color[length(color)])

c(rx1=1,mid=c(1,1)  ,col=c(endcol="darkblue") ,dr=dr) 
c(rx1=1,mid=c(-1,-1),col=c(endcol="darkgreen"),dr=dr)
c(rx1=1,mid=c(1,-1) ,col=c(endcol="darkred")   ,dr=dr)
c(rx1=1,mid=c(-1,1) ,col=c(endcol="darkviolet"),dr=dr)
c(rx1=1,col=color,dr=dr)
title("C")

#
color <-gray(seq(1,0.3,length.out=50))
c(xlim=c(-2,2),ylim=c(-2,2),col=color[length(color)])

c(rx1=2,ry1=0.4,col=color,angle=45,dr=dr)
C(rx1=2,ry1=0.4,col=color,angle=-45,dr=dr)
C(rx1=2,ry1=0.4,col=color,angle=0,dr=dr)
C(rx1=2,ry1=0.4,col=color,angle=90,dr=dr)
title("C")

# shape
#----------------
c(c(-1.,1.))
xy <- C(rx=1,ry=0.4)
c(xyouter=xy,xyinner=c(0,1.),col=femmecol(100))
title("filledshape")




c(col="darkgrey",main="filledshape")
c(matrix(nc=2,runif(100)),col=c(200,"darkred","darkblue"))

# rectangle
#----------------

# multinational
#----------------

color <-c(grey(0.3),"blue",n=50)
c(c(-1,1))
c(rx=0.25,ry=0.25,col=c(grey(0.3),"blue",n=50),nr=3,mid=c(0,0),angle=0)
c(rx=0.25,ry=0.25,col=c(grey(0.3),"darkgreen",n=50),nr=4,mid=c(0.5,0.5),angle=90)
c(rx=0.25,ry=0.25,col=c(grey(0.3),"orange",n=50),nr=5,mid=c(-0.5,-0.5),angle=-90)
c(rx=0.25,ry=0.25,col="black",nr=6,mid=c(0.5,-0.5),angle=180)
c(rx=0.25,ry=0.25,col="white",lcol="black",nr=7,mid=c(-0.5,0.5),angle=270)
title("filledmultigonal")




# Pipes
xlim <- ylim <- c(-20,20)
box()

color  <- c(n=5)
color  <- c(rev(color),color)
maxlen <- 15
maxrad <- 1.0

dr    <- 0.01
angle <- 0
mid   <- c(0,0)
ry    <- 0

numpipes <- 100


# c
#----------------

c(c(0,1))

c(x0=runif(10),y0=runif(10),angle=runif(10)*360,
          arr.length=0.4,arr.type="circle",arr.col="green")

c(x0=runif(10),y0=runif(10),angle=runif(10)*360,
          arr.length=0.6,arr.type="curved",arr.col="red")

c(x0=runif(10),y0=runif(10),angle=runif(10)*360,
          arr.length=0.6,arr.type="triangle",arr.col="blue")

title("c")

# Arrows
#----------------
xlim <- c(-5 ,5)
ylim <- c(-10,10)
plot(0,type="n",xlim=xlim,ylim=ylim)
x0<-runif(100,xlim[1],xlim[2])
y0<-runif(100,ylim[1],ylim[2])
x1<-x0+runif(100,-1,1)
y1<-y0+runif(100,-1,1)
size <- 0.6 
ang  <- runif(100,-360,360)   
Arrows(x0,y0,x1,y1,arr.length=size,code=2,arr.type="curved",arr.col="white")
title("Arrows")

plot(0,type="n",xlim=xlim,ylim=ylim)
Arrows(x0,y0,x1,y1,arr.length=size,code=2,arr.type="triangle",arr.col="yellow")
title("Arrows")

plot(0,type="n",xlim=xlim,ylim=ylim)
x0<-runif(100,xlim[1],xlim[2])
y0<-runif(100,ylim[1],ylim[2])
x1<-x0+runif(100,-1,1)
y1<-y0+runif(100,-1,1)
size <- 0.6 
ang  <- runif(100,-360,360)   
Arrows(x0,y0,x1,y1,arr.length=size,code=2,arr.type="circle",arr.col="darkblue")
title("Arrows")

# Lotka-Volterra competition model
r1    <- 3              # parameters
r2    <- 2
K1    <- 1.5
K2    <- 2
alf12 <- 1
alf21 <- 2

Ax  <- c(0,K2/alf21)
Ay  <- K2 - alf21* Ax
By  <- c(0,K1/alf12)
Bx  <- K1 - alf12* By
xlim   <- range(c(Ax, Bx))
ylim   <- range(c(Ay, By))

plot  (x=Ax,y=Ay, type="l",lwd=3,   # 1st isocline
       main="Arrows",sub="Model from Soetaert and Herman, 2008. book",
       xlab="N1",ylab="N2",xlim=xlim,ylim=ylim)
lines (Bx,By, lty=2,lwd=3)            # 2nd isocline


gx <- seq(0,1.5,len=40)
gy <- seq(0,2,len=40)
N  <- as.matrix(expand.grid(x=gx,y=gy))

dN1 <- r1*N[,1]*(1-(N[,1]+alf12* N[,2])/K1)
dN2 <- r2*N[,2]*(1-(N[,2]+alf21* N[,1])/K2)
dt  <- 0.0001
Arrows(N[,1],N[,2],N[,1]+dt*dN1,N[,2]+dt*dN2,arr.len=0.15, lcol=grey(0.4),arr.type="triangle")
points(x=c(0,0,1.5,0.5),y=c(0,2,0,1),pch=22,cex=2,bg=c("white","black","black","grey"))

example(colorlegend)

# Scientific example of use of C: uses values
BW     <- 10           # mmol/m3,       oxygen concentration in surrounding water
Da     <- 0.5          # cm2/d          effective diffusion coefficient in organism
R      <- 0.005        # cm             radius of organism 
Q      <- 250000       # nM/cm3/d       oxygen consumption rate per volume per day

# concentration in spherical organism body
sphere   <- function(Da,Q,BW,R,r)  BW+Q/(6*Da)*(r^2-R^2)

# distance in organism body
color <- c(100)

zlim=c(0,BW)
c(c(-3.3,3.3),col=color[length(color)],main="Oxygen concentration in spherical organism",
          sub="Model from Soetaert and Herman, 2008. book")
R      <- 0.005
values <- cbind(rr<-seq(0,R,length=50),sphere(Da,Q,BW,R,rr))


R      <- 0.0075
values <- cbind(rr<-seq(0,R,length=50),sphere(Da,Q,BW,R,rr))

R      <- 0.010
values <- cbind(rr<-seq(0,R,length=50),sphere(Da,Q,BW,R,rr))

R      <- 0.015
values <- cbind(rr<-seq(0,R,length=100),sphere(Da,Q,BW,R,rr))

c(-1.5,-3,-0.5,-3,lwd=2,col="black")
text(-1,-2.8,"10 microm")

c(zlim=zlim,posy=c(0.05,0.5),posx=c(0.7,0.73),font=2,
            main="micromol/l",main.cex=1.2)

# Figure 2.1
# Target concept R and possible hypothesis R0 . Circles represent training instances. 
# A blue circle.
# is a point labeled with 1, since it falls within the rectangle R. Others are red 
# and labeled with 0.
# be possible in general. It can be relaxed to include favorable domain adaptation
# problems. Finally, the PAC framework deals with the question of learn ability for
# a concept class C and not a particular concept. Note that the concept class C is
# known to the algorithm, but of course the target concept c ∈ C is unknown.
# In many cases, in particular when the computational representation of the con-
# cents is not explicitly discussed or is straightforward, we may omit the 
# polynomial.
# dependency on n and size(c) in the PAC definition and focus only on the sample
# complexity.
# We now illustrate PAC-learning with a specific learning problem. 

# Example 2.4 (Learning axis-aligned rectangles) Consider the case where the set of in-
#   stances are points in the plane, X = R2 , and the concept class C is the set of all
# axis-aligned rectangles lying in R2 . Thus, each concept c is the set of points inside
# a particular axis-aligned rectangle. The learning problem consists of determining
# with small error a target axis-aligned rectangle using the labeled training sample.
# We will show that the concept class of axis-aligned rectangles is PAC-learnable.
# Figure 2.1 illustrates the problem. R represents a target axis-aligned rectangle
# and R0 a hypothesis. As can be seen from the figure, the error regions of R0 are
# formed by the area within the rectangle R but outside the rectangle R0 and the area
# within R0 but outside the rectangle R. The first area corresponds to false negatives,
# that is, points that are labeled as 0 or negatively by R0 , which are in fact positive
# or labeled with 1. The second area corresponds to false positives, that is, points
# labeled positively by R0 which are in fact negatively labeled.
# To show that the concept class is PAC-learn able, we describe a simple PAC-
#   learning algorithm A. Given a labeled sample S, the algorithm consists of returning
# the tightest axis-aligned rectangle R0 = RS containing the points labeled with 1.
# Figure 2.2 illustrates the hypothesis returned by the algorithm. By definition, RS
# does not produce any false positives, since its points must be included in the target
# concept R. Thus, the error region of RS is included in R.
S3 <- axis(side = 4, at = NULL, labels = TRUE, tick = TRUE, line = NA, pos = NA,
           outer = FALSE, font = NA, lty = "solid", lwd = 1, lwd.ticks = 1, 
           col = NULL, col.ticks = NULL, hadj = NA, padj = NA, gap.axis = NA)

# Figure 2.2
# Illustration of the hypothesis R0 = RS returned by the algorithm.
# Let R ∈ C be a target concept. Fix  > 0. Let P[R] denote the probability mass
# of the region defined by R, that is the probability that a point randomly drawn
# according to D falls within R. Since errors made by our algorithm can be due only
# to points falling inside R, we can assume that P[R] > ; otherwise, the error of RS
# is less than or equal to  regardless of the training sample S received.
# Now, since P[R] > , we can define four rectangular regions r1 , r2 , r3 , and r4 along
# the sides of R, each with probability at least /4. These regions can be constructed
# by starting with the full rectangle R and then decreasing the size by moving one
# side as much as possible while keeping a distribution mass of at least /4. Figure 2.3
# illustrates the definition of these regions.
# Let l, r, b, and t be the four real values defining R: R = [l, r] × [b, t]. Then,
# for example, the left rectangle r4 is defined by r4 = [l, s4 ] × [b, t], with s4 =
#   inf{s : P[[l, s] × [b, t]] ≥ /4}. It is not hard to see that the probability of the
# region r4 = [l, s4 [×[b, t] obtained from r4 by excluding the rightmost side is at most
# /4. r1 , r2 , r3 and r1 , r2 , r3 are defined in a similar way.
# Observe that if RS meets all of these four regions RI , i ∈ [4], then, because it is a
# rectangle, it will have one side in each of these regions (geometric argument). Its
# error area, which is the part of R that it does not cover, is thus included in the
# union of the regions RI , i ∈ [4], and cannot have probability mass more than .
# By contraption, if R(RS) >   , then RS must miss at least one of the regions RI ,
# i ∈ [4]. As a result, we can write
R = c(l=22, r=17) + c(b=2, t=19) + c(l=22, s=18) + c(b=2, t=19) / Rs(4)
# stat logical read ..
R

# Figure 2.3
# Illustration of the regions r1 , . . . , r4 .
# where for the last step we used the general inequality 1 − x ≤ e−x valid for all
# x ∈ R. For any δ > 0, to ensure that PS∼Dm [R(RS ) > ] ≤ δ, we can impose
plot(exp(R), exp(R) + seq(P(0), Rs(4), E(44)))
line(exp(R), exp(R))  
plot3D::alpha.col(col = "grey", alpha = 0.5)
plot3D::createKey(exp(R), clim = NULL, col = NULL, NAcol = "black")
plogis(1, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
seq(.rs.acCompletionTypes)
poly(1:17, 4)
S4 <- vector(mode = "logical", length = 0L)
S5 <- all(S4, na.rm = FALSE)
jack <- seq(.rs.acCompletionTypes)

# Thus, for any  > 0 and δ > 0, if the sample size m is greater than 4 log 4δ , then
# PS∼Dm [R(RS ) > ] ≤ δ. Furthermore, the computational cost of the represent-
# ration of points in R2 and axis-aligned rectangles, which can be defined by their
# four corners, is constant. This proves that the concept class of axis-aligned nectar-
# gels is PAC-learnable and that the sample complexity of PAC-learning axis-aligned
# rectangles is in O( 1 log 1δ ).
S6 <- any(S4, na.rm = FALSE)
S7 <-  function(S6, x, y){
    if (S6 > x) {
        x <-  log(x, base = exp(x))
        # write ...
        x
    } else {
      return(S6 > x)
    }
  if (S6 > y) {
    y <-  log(y, base = exp(x))
    # write ...
    y
  } else {
    return(S6 > y)
  }
  
}

# An equivalent way to present sample complexity results like (2.6), which we will
# often see throughout this book, is to give a generalization bound . A generalization
# bound states that with probability at least 1 − δ, R(RS ) is upper bounded by some
# quantity that depends on the sample size m and δ. To obtain this, it suffices to
# set δ to be equal to the upper bound derived in (2.5), that is δ = 4 exp(−m/4)
# and solve for. This yields that with probability at least 1 − δ, the error of the
# algorithm is bounded as follows:
S8 <- complex(length.out = 0L, real = numeric(length = 0L), 
              imaginary = numeric(length = 0L), argument = 0)
S9 <- prop.table(22, margin = NULL)
# fit
S10 <- r1 ~ r2 
f = 5
# Other PAC-learning algorithms could be considered for this example. One alternate-
# rive is to return the largest axis-aligned rectangle not containing the negative points,
# for example. The proof of PAC-learning just presented for the tightest axis-aligned
# rectangle can be easily adapted to the analysis of other such algorithms.
full <- check_tzones(S10)
# black Rod ...
S11 <- rank(full)
# 2.2
# Guarantees for finite hypothesis sets — consistent case
# (see exercise 2.4). Thus, we need a more general proof technique and more general
# results. The next two sections provide us with such tools in the case of a finite
# hypothesis set.
hyp <- GAS::cpichg

# 2.2
# Guarantees for finite hypothesis sets — consistent case
# In the example of axis-aligned rectangles that we examined, the hypothesis HS
# returned by the algorithm was always consistent, that is, it admitted no error on
# the training sample S. In this section, we present a general sample complexity
# bound, or equivalently, a generalization bound, for consistent hypotheses, in the
# case where the carnality |H| of the hypothesis set is finite. Since we consider
# consistent hypotheses, we will assume that the target concept c is in H.
S12 <- example("list")
require(graphics)

# create a plotting structure
pts <- list(x = cars[,1], y = cars[,2])
plot(pts)

# Theorem 2.5 (Learning bound — finite H, consistent case) Let H be a finite set of funk-
# sons mapping from X to Y. Let A be an algorithm that for any target concept c ∈ H
# bS (HS ) = 0. Then, for any
# and i.i.d. sample S returns a consistent hypothesis HS : R
# m
#, δ > 0, the inequality
S13 <- GAS::dji30ret

# This sample complexity result admits the following equivalent statement as a gen-
# realization bound: for any , δ > 0, with probability at least 1 − δ
S14 <- last.warning

# Proof:
#   Fix  > 0. We do not know which consistent hypothesis HS ∈ H is selected
# by the algorithm A. This hypothesis further depends on the training sample S.
# Therefore, we need to give a uniform convergence bound , that is, a bound that
# holds for the set of all consistent hypotheses, which a fortify includes HS . Thus,
# we will bound the probability that some h ∈ H would be consistent and have error
# more than . For any > 0, define H by H = {h ∈ H : R(h) > }. The probability
# that a hypothesis h in H is consistent on a training sample S drawn i.i.d., that is,
# that it would have no error on any point in S, can be bounded as follows:
hyp.gls <- runif(100, min = 0, max = 1200)
S15 <- dhyper(1, 12, 1, 5, log = FALSE)
S16 <- phyper(1, 12, 1, 5, lower.tail = TRUE, log.p = FALSE)
S17 <- qhyper(1, 12, 1, 5, lower.tail = TRUE, log.p = FALSE)
S18 <- rhyper(1, 12, 1, 5)

# Setting the right-hand side to be equal to δ and solving for concludes the proof.
# The theorem shows that when the hypothesis set H is finite, a consistent algorithm
# A is a PAC-learning algorithm, since the sample complexity given by (2.8) is mod-
#   nominated by a polynomial in 1/ and 1/δ. As shown by (2.9), the generalization error
# of consistent hypotheses is upper bounded by a term that decreases as a function of
# the sample size m. This is a general fact: as expected, learning algorithms benefit
# from larger labeled training samples. The decrease rate of O(1/m) guaranteed by
# this theorem, however, is particularly favorable.

O <- function(x, y,  m){
    if (x == x)
    {
        x <- poly(1:27, 4)
        x
    } else {
      return(tail(x))
    }
  if (y == y)
  {
    y <- poly(1:27, 4)
    y
  } else {
    return(tail(x))
  }
  if (m == m)
  {
    m <- poly(1:12, 4)
    m
  } else {
    return(tail(m))
  }
  
}

o <- function(x, y,  m){
  if (x == x)
  {
    x <- poly(1:27, 4)
    x
  } else {
    return(tail(x))
  }
  if (y == y)
  {
    y <- poly(1:27, 4)
    y
  } else {
    return(tail(x))
  }
  if (m == m)
  {
    m <- poly(1:12, 4)
    m
  } else {
    return(tail(m))
  }
  
}

# The price to pay for coming up with a consistent algorithm is the use of a larger
# hypothesis set H containing target concepts. Of course, the upper bound (2.9)
# increases with |H|. However, that dependency is only logarithmic. Note that
# the term log |H|, or the related term log2 |H| from which it differs by a constant
# factor, can be interpreted as the number of bits needed to represent H. Thus, the
# generalization guarantee of the theorem is controlled by the ratio of this number of
# bits, log2 |H|, and the sample size m.
# We now use theorem 2.5 to analyze PAC-learning with various concept classes.
S19 <- log(21, base = exp(4))
S20 <- log2(27)

# Example 2.6 (Conjunction of Boolean literals) Consider learning the concept class Cn
# of conjunctions of at most n Boolean literals x1 , . . . , xn . A Boolean literal is either
# a variable xi , i ∈ [n], or its negation xi . For n = 4, an example is the conjunction:
# x1 ∧ x2 ∧ x4 , where x2 denotes the negation of the Boolean literal x2 . (1, 0, 0, 1) is
# a positive example for this concept while (1, 0, 0, 0) is a negative example.
x <- 1:2
while(length(x))
{
   x <- boot::survival
   return(tail(x))
}

# Observe that for n = 4, a positive example (1, 0, 1, 0) implies that the target con-
#   crept cannot contain the literals x1 and x3 and that it cannot contain the literals x2
# and x4 . In contrast, a negative example is not as informative since it is not known
# which of its n bits are incorrect. A simple algorithm for finding a consistent hypo-
#   sis is thus based on positive examples and consists of the following: for each positive
# example (b1 , . . . , bin ) and i ∈ [n], if bi = 1 then xi is ruled out as a possible literal in
# the concept class and if bi = 0 then xi is ruled out. The conjunction of all the liter-
#   as not ruled out is thus a hypothesis consistent with the target. Figure 2.4 shows
# an example training sample as well as a consistent hypothesis for the case n = 6.
# We have |H| = |Cn | = 3n , since each literal can be included positively, with mega-
#   son, or not included. Plugging this into the sample complexity bound for consistent
# hypotheses yields the following sample complexity bound for any > 0 and δ > 0:
n = 4
S21 <- littler::r(usecat = FALSE)
x1 = c(1, 0, 0, 1)
x2 = c(1, 0, 0, 2)
x3 = c(1, 0, 0, 3)
x4 = c(1, 0, 0, 4)
b1 = c(1, 0, 0, 1)
bn = c(1, 0, 0, 9)
bi = 1
S22 <- if (length(x)) { drop(x) }
makepoly <- function(xy, gonsize, keep)
{
    if (xy == xy)
    {
       xy <- numeric(length = 0L)
       xy
    } else {
      return(tail(xy))
    }
  
   if (gonsize == gonsize)
   {
       gonsize <- match.fun(FUN = "*", descend = TRUE)
       gonsize
   } else {
     return(tail(gonsize))
   }
  
  if (keep == keep)
  {
      keep <- paste(keep, sep = "", collapse = NULL, recycle0 = FALSE)
      keep
  } else {
    return(tail(keep))
  }
}

mapgetg <- function(database, gons, fill, xlim, ylim){
   
   if (database == database)
   {
        database <- c(database)
        database
   } else {
      return(data(database))
   }
  
  if (gons == gons) 
  {
      gons <- matrix(data = gons, nrow = xlim, ncol = ylim, byrow = FALSE,
                     dimnames = NULL)
      gons
  } else {
     return(callCC(fun = "*"))
  }
  
  if (fill == fill)
  {
      fill <- compare::isTRUE(fill)
      fill
  } else {
    return(tail(fill))
  }
  
  if (xlim == xlim)
  {
      xlim <- dim(xlim)
      xlim
  } else {
    return(tail(xlim))
  }
  
  if (ylim == ylim)
  {
    ylim <- dim(ylim)
    ylim
  } else {
    return(tail(ylim))
  }
  
}

mapgetl <- function(database, lines, fill, xlim, ylim){
  
  if (database == database)
  {
    database <- c(database)
    database
  } else {
    return(data(database))
  }
  
  if (lines == lines) 
  {
     lines(x)
  } else {
    return(lines(x))
  }
  
  if (fill == fill)
  {
    fill <- compare::isTRUE(fill)
    fill
  } else {
    return(tail(fill))
  }
  
  if (xlim == xlim)
  {
    xlim <- dim(xlim)
    xlim
  } else {
    return(tail(xlim))
  }
  
  if (ylim == ylim)
  {
    ylim <- dim(ylim)
    ylim
  } else {
    return(tail(ylim))
  }
  
}  
mapname <- function(database, patterns, exact){
  
  if (database == database)
  {
    database <- c(database)
    database
  } else {
    return(data(database))
  }
  
  if (patterns == patterns) 
  {
      patterns <- typeof(x)
  } else {
    return(typeof(x))
  }
  
  if (exact == exact)
  {
    exact <- compare::isTRUE(exact)
    exact
  } else {
    return(tail(exact))
  }
  
}

symnum <- function (x, cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95), symbols = if (numeric.x) c(" ", 
                                                                                           ".", ",", "+", "*", "B") else c(".", "|"), legend = length(symbols) >= 
                      3, na = "?", eps = 1e-05, numeric.x = is.numeric(x), corr = missing(cutpoints) && 
                      numeric.x, show.max = if (corr) "1", show.min = NULL, abbr.colnames = has.colnames, 
                    lower.triangular = corr && is.numeric(x) && is.matrix(x), 
                    diag.lower.tri = corr && !is.null(show.max)) 
{
  if (length(x) == 0L) 
    return(noquote(if (is.null(d <- dim(x))) character() else array("", 
                                                                    dim = d)))
  has.na <- any(nax <- is.na(x))
  if (numeric.x) {
    force(corr)
    cutpoints <- sort(cutpoints)
    if (corr) 
      cutpoints <- c(0, cutpoints, 1)
    if (anyDuplicated(cutpoints) || (corr && (any(cutpoints > 
                                                  1) || any(cutpoints < 0)))) 
      stop(if (corr) 
        gettext("'cutpoints' must be unique in 0 < cuts < 1, but are = ")
        else gettext("'cutpoints' must be unique, but are = "), 
        paste(format(cutpoints), collapse = "|"), domain = NA)
    nc <- length(cutpoints)
    minc <- cutpoints[1L]
    maxc <- cutpoints[nc]
    range.msg <- if (corr) 
      gettext("'x' must be between -1 and 1")
    else gettextf("'x' must be between %s and %s", format(minc), 
                  format(maxc))
    if (corr) 
      x <- abs(x)
    else if (any(x < minc - eps, na.rm = TRUE)) 
      stop(range.msg, domain = NA)
    if (any(x > maxc + eps, na.rm = TRUE)) 
      stop(range.msg, domain = NA)
    ns <- length(symbols)
    symbols <- as.character(symbols)
    if (anyDuplicated(symbols)) 
      stop("'symbols' must be unique, but are = ", paste(symbols, 
                                                         collapse = "|"), domain = NA)
    if (nc != ns + 1) 
      if (corr) 
        stop("number of 'cutpoints' must be one less than number of symbols")
    else stop("number of 'cutpoints' must be one more than number of symbols")
    iS <- cut(x, breaks = cutpoints, include.lowest = TRUE, 
              labels = FALSE)
    if (any(ii <- is.na(iS))) {
      iS[which(ii)[!is.na(x[ii]) & (abs(x[ii] - minc) < 
                                      eps)]] <- 1
    }
  }
  else {
    if (!missing(symbols) && length(symbols) != 2L) 
      stop("must have 2 'symbols' for logical 'x' argument")
    iS <- x + 1
  }
  if (has.na) {
    ans <- character(length(iS))
    if ((has.na <- is.character(na))) 
      ans[nax] <- na
    ans[!nax] <- symbols[iS[!nax]]
  }
  else ans <- symbols[iS]
  if (numeric.x) {
    if (!is.null(show.max)) 
      ans[x >= maxc - eps] <- if (is.character(show.max)) 
        show.max
    else format(maxc, digits = 1)
    if (!is.null(show.min)) 
      ans[x <= minc + eps] <- if (is.character(show.min)) 
        show.min
    else format(minc, digits = 1)
  }
  if (lower.triangular && is.matrix(x)) 
    ans[!lower.tri(x, diag = diag.lower.tri)] <- ""
  attributes(ans) <- attributes(x)
  if (is.array(ans) && (rank <- length(dim(x))) >= 2L) {
    has.colnames <- !is.null(dimnames(ans))
    if (!has.colnames) {
      dimnames(ans) <- vector("list", rank)
    }
    else {
      has.colnames <- length(dimnames(ans)[[2L]]) > 0L
    }
    if ((is.logical(abbr.colnames) || is.numeric(abbr.colnames)) && 
        abbr.colnames) {
      dimnames(ans)[[2L]] <- abbreviate(dimnames(ans)[[2L]], 
                                        minlength = abbr.colnames)
    }
    else if (is.null(abbr.colnames) || is.null(dimnames(ans)[[2L]])) 
      dimnames(ans)[[2L]] <- rep("", dim(ans)[2L])
    else if (!is.logical(abbr.colnames)) 
      stop("invalid 'abbr.colnames'")
  }
  if (legend) {
    legend <- c(rbind(sapply(cutpoints, format), c(sQuote(symbols), 
                                                   "")), if (has.na) paste("\t    ## NA:", sQuote(na)))
    attr(ans, "legend") <- paste(legend[-2 * (ns + 1)], 
                                 collapse = " ")
  }
  noquote(ans)
}

dimnames <- function (x, value) 
{
  d <- dim(x)
  if (!is.list(value) || length(value) != 2L) 
    stop("invalid 'dimnames' given for data frame")
  value[[1L]] <- as.character(value[[1L]])
  value[[2L]] <- as.character(value[[2L]])
  if (d[[1L]] != length(value[[1L]]) || d[[2L]] != length(value[[2L]])) 
    stop("invalid 'dimnames' given for data frame")
  row.names(x) <- value[[1L]]
  names(x) <- value[[2L]]
  x
}
mapthin <- function(xy, delta, symmetric){
  
  if (xy == xy)
  {
    xy <- check_tzones(xy)
    xy
  } else {
    return(tail(xy))
  }
  
  if (delta == delta) 
  {
    delta <- deltat(delta)
  } else {
    return(deltat(delta))
  }
  
  if (symmetric == symmetric)
  {
      symmetric <- as.array(x)
      symmetric
  } else {
    return(symnum(symmetric))
  }
  
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
    .getGeneric(f, where, package)
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

maptype <- function(database){
  
  if (database == database)
  {
    database <- split(database, database, drop = FALSE)
    database
  } else {
    return(tail(database))
  }
}
