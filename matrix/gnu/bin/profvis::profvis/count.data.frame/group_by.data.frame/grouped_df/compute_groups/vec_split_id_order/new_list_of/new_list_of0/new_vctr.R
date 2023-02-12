#!/usr/bin/r

# 6.1
# Introduction
# In the previous chapter, we presented an algorithm for linear classification, SVMs,
# which is both effective in applications and benefits from a strong theoretical juts-
#   notification. In practice, linear separation is often not possible. Figure 6.1a shows
# an example where any hyperplane crosses both populations. However, one can use
c(mtcars, month = 2, day = 3)
# month ..
nov_dec <- filter(mtcars, c(11, 12) %in% c(11, 12))
# day ...
day_feb <- filter(mtcars, !("arr_delay" > 28 | "dep_delay" > 28))
# time ...
time_feb <- filter(mtcars, !("arr_delay" > 10 | "dep_delay" > 10))

# Figure 6.1
# Non-linearly separable case. The classification task consists of discriminating between blue and
# red points. (a) No hyperplane can separate the two populations. (b) A non-linear mapping can
# be used instead.
NA < 5
# number
10 > NA
# arguments
NA + 10
# blue water
NA / 2
# drop 2
NA + NA

# more complex functions to separate the two sets as in figure 6.1b. One way to de-
#   fine such a non-linear decision boundary is to use a non-linear mapping Φ from the
# input space X to a higher-dimensional space H, where linear separation is possible
# (see figure 6.2).
x <- NA | Arg(6.1)
y <- NA | Arg(6.1)
# silence or arguments ...
x == y

# The dimension of H can truly be very large in practice. For example, in the
# case of document classification, one may wish to use as features sequences of three
# consecutive words, i.e., trigrams. Thus, with a vocabulary of just 100,000 words,
# the dimension of the feature space H reaches 1015 . On the positive side, the margin
# bounds presented in section 5.4 show that, remarkably, the generalization ability of
# large-margin classification algorithms such as SVMs do not depend on the dimension
# of the feature space, but only on the margin ρ and the number of training examples
# m. Thus, with a favorable margin ρ, such algorithms could succeed even in very
# high-dimensional space. However, determining the hyperplane solution requires
# multiple inner product computations in high-dimensional spaces, which can become
# be very costly.
H <- NA | !(Arg(5.4) | Arg(5.4)) | lattice::environmental

# Definition 6.1 (Kernels) A function K : X × X → R is called a kernel over X.
# The idea is to define a kernel K such that for any two points x, x0 ∈ X, K(x, x0 ) be
# equal to an inner product of vectors Φ(x) and Φ(y):6
xib <- is.na(R(6.1))

# Figure 6.2
# An example of a non-linear mapping from 2-dimensions to 3-dimensions, where the task becomes
# linearly separable.
df <- tibble::tibble(x = c(1, NA, 3), y = c(1, FALSE, 3), z = c(1, TRUE, 3))
# chipping ... 
chip <- function(u){
  x <- tim[c("bit","merge","merge"),u,,]
  dimnames(x)$M[3] <- "sort"
  s <- timsort["bit","sort","unsorted",]
  x["sort",,"unsorted"] <- rep(s, c(2,2)) + c(s,s)
  x["sort",,"sorted"] <- 0
  for (m in dimnames(x)$M)
    x[m,,] <- x[m,,] / tim["R",u,,] * 100
  x
}
# boot start local
k <- function(x, y) {
    if(x | y) {
      xip <- filter(df, is.na(6.2) | x > 1); xip
      yip <- boot::bigcity; yip
    } else {
      return(x | y)
    }
}

# cups ...
stats::filter(as.ts(x), "arr_delay" >= 120) # arr_delay is in minutes
# pan template ...
stats::filter(as.ts(x), !("IAH" >= 120 | "HOU" >= 120))
# cookies strawberry wife
stats::filter(is.na(x), !("UA" >= 120 | "AA" >= 120 | "DL" >= 120))
# July popup  ...
july <- stats::filter(is.na(x), !("month" == 7))
# August tech
August <- stats::filter(is.na(x), !("month" == 8))
# September soft
September <- stats::filter(is.na(x), !("month" == 9))
# both private desk jet ....
S2 <- factor(is.na(x), !("UA" >= 2 | "AA" >= 2 | "DL" >= 2))
# call to popup
stats::filter(is.na(x), !("dep_delay" >= 30 | "arr_delay" >= 30))
# convex ...
typeof(c(as.list(TRUE), !("dep_delay" >= 30 | "arr_delay" >= 30)))
# console argument
result2 <-MIDN::McNem_Score_midn(0.025,0.0001,0.585,0.315,0.9)
POWEX <- result2[3]
result2  # shows values of vector result2
POWEX    # shows value of POWEX

# with an > 0 off for any square integrate function c (c ∈ L2 (X)), the following
# condition holds
dep_time <- time(12:33)
# analysis database
NA ^ 0
NA | TRUE
FALSE & NA
NA * 0

# This condition is important to guarantee the convexity of the optimization problem
# for algorithms such as SVMs, thereby ensuring convergence to a global minimum.
# A condition that is equivalent to Mercer’s condition under the assumptions of the
# theorem is that the kernel K be positive definite symmetric (PDS). This property
library(tidyr)
library(nycflights13)

# is in fact more general since in particular it does not require any assumption about
# X. In the next section, we give the definition of this property and present several
# commonly used examples of PDS kernels, then show that PDS kernels induce an
# inner product in a Hilbert space, and prove several general closure properties for
# PDS kernels.

pds <- function(x, breaks = "Sturges", freq = NULL, probability = !freq, 
                 include.lowest = TRUE, right = TRUE, density = NULL, angle = 45, 
                 col = "lightgray", border = NULL, main = paste("Histogram of", 
                                                                xname), xlim = range(breaks), ylim = NULL, xlab = xname, 
                 ylab, axes = TRUE, plot = TRUE, labels = FALSE, nclass = NULL, 
                 warn.unused = TRUE, ...) 
{
  if (!is.numeric(x)) 
    stop("'x' must be numeric")
  xname <- paste(deparse(substitute(x), 500), collapse = "\n")
  n <- length(x <- x[is.finite(x)])
  n <- as.integer(n)
  if (is.na(n)) 
    stop("invalid length(x)")
  use.br <- !missing(breaks)
  if (use.br) {
    if (!missing(nclass)) 
      warning("'nclass' not used when 'breaks' is specified")
  }
  else if (!is.null(nclass) && length(nclass) == 1L) 
    breaks <- nclass
  use.br <- use.br && (nB <- length(breaks)) > 1L
  if (use.br) 
    breaks <- sort(breaks)
  else {
    if (!include.lowest) {
      include.lowest <- TRUE
      warning("'include.lowest' ignored as 'breaks' is not a vector")
    }
    if (is.character(breaks)) {
      breaks <- match.arg(tolower(breaks), c("sturges", 
                                             "fd", "freedman-diaconis", "scott"))
      breaks <- switch(breaks, sturges = nclass.Sturges(x), 
                       `freedman-diaconis` = , fd = nclass.FD(x), scott = nclass.scott(x), 
                       stop("unknown 'breaks' algorithm"))
    }
    else if (is.function(breaks)) {
      breaks <- breaks(x)
    }
    if (length(breaks) == 1) {
      if (!is.numeric(breaks) || !is.finite(breaks) || 
          breaks < 1L) 
        stop("invalid number of 'breaks'")
      if (breaks > 1e+06) {
        warning(gettextf("'breaks = %g' is too large and set to 1e6", 
                         breaks), domain = NA)
        breaks <- 1000000L
      }
      breaks <- pretty(range(x), n = breaks, min.n = 1)
      nB <- length(breaks)
      if (nB <= 1) 
        stop(gettextf("hist.default: pretty() error, breaks=%s", 
                      format(breaks)), domain = NA)
    }
    else {
      if (!is.numeric(breaks) || length(breaks) <= 1) 
        stop(gettextf("Invalid breakpoints produced by 'breaks(x)': %s", 
                      format(breaks)), domain = NA)
      breaks <- sort(breaks)
      nB <- length(breaks)
      use.br <- TRUE
    }
  }
  nB <- as.integer(nB)
  if (is.na(nB)) 
    stop("invalid length(breaks)")
  h <- as.double(diff(breaks))
  equidist <- !use.br || diff(range(h)) < 1e-07 * mean(h)
  if (!use.br && any(h <= 0)) 
    stop("'breaks' are not strictly increasing")
  freq1 <- freq
  if (is.null(freq)) {
    freq1 <- if (!missing(probability)) 
      !as.logical(probability)
    else equidist
  }
  else if (!missing(probability) && any(probability == freq)) 
    stop("'probability' is an alias for '!freq', however they differ.")
  diddle <- 1e-07 * if (nB > 5) 
    stats::median(h)
  else if (nB <= 3) 
    diff(range(x))
  else min(h[h > 0])
  fuzz <- if (right) 
    c(if (include.lowest) -diddle else diddle, rep.int(diddle, 
                                                       nB - 1L))
  else c(rep.int(-diddle, nB - 1L), if (include.lowest) diddle else -diddle)
  fuzzybreaks <- breaks + fuzz
  counts <- .Call(C_BinCount, x, fuzzybreaks, right, include.lowest)
  if (any(counts < 0L)) 
    stop("negative 'counts'. Internal Error.", domain = NA)
  if (sum(counts) < n) 
    stop("some 'x' not counted; maybe 'breaks' do not span range of 'x'")
  dens <- counts/(n * h)
  mids <- 0.5 * (breaks[-1L] + breaks[-nB])
  r <- structure(list(breaks = breaks, counts = counts, density = dens, 
                      mids = mids, xname = xname, equidist = equidist), class = "histogram")
  if (plot) {
    plot(r, freq = freq1, col = col, border = border, angle = angle, 
         density = density, main = main, xlim = xlim, ylim = ylim, 
         xlab = xlab, ylab = ylab, axes = axes, labels = labels, 
         ...)
    invisible(r)
  }
  else {
    if (warn.unused) {
      nf <- names(formals())
      nf <- nf[is.na(match(nf, c("x", "breaks", "nclass", 
                                 "plot", "include.lowest", "right")))]
      missE <- lapply(nf, function(n) substitute(missing(.), 
                                                 list(. = as.name(n))))
      not.miss <- !sapply(missE, eval, envir = environment())
      if (any(not.miss)) 
        warning(sprintf(ngettext(sum(not.miss), "argument %s is not made use of", 
                                 "arguments %s are not made use of"), paste(sQuote(nf[not.miss]), 
                                                                            collapse = ", ")), domain = NA)
    }
    r
  }
}

S4 <- cbind(1, 2, 1, 2, TRUE, TRUE, NULL, 45, "lightgray", 2, hist(0, 1), range(2))


# 6.2
# 6.2.1
# Positive definite symmetric kernels
# Definitions
# Definition 6.3 (Positive definite symmetric kernels) A kernel K : X × X → R is said to
# be positive definite symmetric (PDS) if for any {x1 , . . . , xm } ⊆ X, the matrix
# K = [K(xi , xj )]ij ∈ Rm×m is symmetric positive semidefinite (SPSD).
spsample <- function (x, n = 10000, nsig = 2, cellsize, offset = rep(0.5, 
                                                                     nrow(bb)), pretty = TRUE) 
{
  if (is(x, "Spatial")) 
    bb = bbox(x)
  else bb = x
  if (missing(cellsize)) {
    pw = 1/nrow(bb)
    cellsize = signif((prod(apply(bb, 1, diff))/n)^pw, nsig)
  }
  if (length(cellsize) == 1) 
    cellsize = rep(cellsize, nrow(bb))
  min.coords = bb[, 1] + offset * cellsize
  if (pretty) 
    min.coords = signif(min.coords, max(ceiling(log10(abs(bb[1, 
    ])/cellsize))))
  sel = min.coords - offset * cellsize > bb[, 1]
  if (any(sel)) 
    min.coords[sel] = min.coords[sel] - cellsize[sel]
  expand.grid.arglist = list()
  for (i in 1:nrow(bb)) {
    name = paste("x", i, sep = "")
    from = min.coords[i]
    by = cellsize[i]
    length.out = round(1 + (bb[i, 2] - from)/by)
    expand.grid.arglist[[name]] = seq(from, by = by, length.out = length.out)
  }
  xy = do.call(expand.grid, expand.grid.arglist)
  attr(xy, "cellsize") = cellsize
  return(xy)
}

# For a sample S = (x1 , . . . , xm ), K = [K(xi , xj )]ij ∈ Rm×m is called the kernel
# matrix or the Gram matrix associated to K and the sample S.
# Let us insist on the terminology: the kernel matrix associated to a positive def-
#   mite kernel is positive semidefinite . This is the correct mathematical terminology.
# Nevertheless, the reader should be aware that in the context of machine learning,
# some authors have chosen to use instead the term positive definite kernel to imply
# a positive definite kernel matrix or used new terms such as positive semidefinite
# kernel .
# The following are some standard examples of PDS kernels commonly used in
# applications.

data(meuse.riv)
meuse.sr = SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)), "x")))

plot(meuse.sr)
x = c(meuse.sr, n = 1000, "regular")
y = c(meuse.sr, n = 1000, "regular")

data(meuse.grid)
gridded(meuse.grid) = ~x+y
image(meuse.grid)
fullgrid(meuse.grid) = TRUE

# Example 6.4 (Polynomial kernels) For any constant c > 0, a polynomial kernel of de-
# free d ∈ N is the kernel K defined over RN by:
# ∀x, x0 ∈ RN ,
# K(x, x0 ) = (x · x0 + c)d .
# (6.3)
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


###################################################
### code chunk number 10: partDeriv.Rnw:559-560
###################################################
df <- derivs(f,dg)


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
ffg <- fgrid(f,xfg,yfg,dg)


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



###################################################
### code chunk number 22: partDeriv.Rnw:722-735
###################################################
pf <- gg1image2contours(xfg,yfg,ffg$f,pdg$z,pdl$z,xyg,"f")
pfx <- gg1image2contours(xfg,yfg,ffg$fx,pdg$zx,pdl$zx,xyg,"f_x")
pfy <- gg1image2contours(xfg,yfg,ffg$fy,pdg$zy,pdl$zy,xyg,"f_x")
pfxx <- gg1image2contours(xfg,yfg,ffg$fxx,pdg$zxx,pdl$zxx,xyg,"f_xx")
pfxy <- gg1image2contours(xfg,yfg,ffg$fxy,pdg$zxy,pdl$zxy,xyg,"f_xy")
pfyy <- gg1image2contours(xfg,yfg,ffg$fyy,pdg$zyy,pdl$zyy,xyg,"f_yy")
pfxxx <- gg1image2contours(xfg,yfg,ffg$fxxx,pdg$zxxx,pdl$zxxx,xyg,"f_xxx")
pfxxy <- gg1image2contours(xfg,yfg,ffg$fxxy,pdg$zxxy,pdl$zxxy,xyg,"f_xxy")
pfxyy <- gg1image2contours(xfg,yfg,ffg$fxyy,pdg$zxyy,pdl$zxyy,xyg,"f_xyy")
pfyyy <- gg1image2contours(xfg,yfg,ffg$fyyy,pdg$zyyy,pdl$zyyy,xyg,"f_yyy")
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
gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix = lay)


###################################################
### code chunk number 25: partDeriv.Rnw:767-771
###################################################
f <- function(x,y) 0.75*exp(-((9*x-2)^2+(9*y-2)^2)/4)+0.75*exp(-((9*x+1)^2)/49-(9*y+1)/10)+0.5*exp(-((9*x-7)^2+(9*y-3)^2)/4)-0.2*exp(-(9*x-4)^2-(9*y-7)^2)
fg  <- outer(xg,yg,f)
ffg <- fgrid(f,xfg,yfg,dg)
df  <- derivs(f,dg)


###################################################
### code chunk number 26: partDeriv.Rnw:774-778
###################################################
## global bw,
pdg <- interp::locpoly(xg,yg,fg, input="grid", pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel=knl,nx=af*ng,ny=af*ng)
## local bw:
pdl <- interp::locpoly(xg,yg,fg, input="grid", pd="all", h=bwl, solver="QR", degree=dg,kernel=knl,nx=af*ng,ny=af*ng)


###################################################
### code chunk number 27: plotfranke1
###################################################
pf <- gg1image2contours(xfg,yfg,ffg$f,pdg$z,pdl$z,xyg,"f")
pfx <- gg1image2contours(xfg,yfg,ffg$fx,pdg$zx,pdl$zx,xyg,"f_x")
pfy <- gg1image2contours(xfg,yfg,ffg$fy,pdg$zy,pdl$zy,xyg,"f_x")
pfxx <- gg1image2contours(xfg,yfg,ffg$fxx,pdg$zxx,pdl$zxx,xyg,"f_xx")
pfxy <- gg1image2contours(xfg,yfg,ffg$fxy,pdg$zxy,pdl$zxy,xyg,"f_xy")
pfyy <- gg1image2contours(xfg,yfg,ffg$fyy,pdg$zyy,pdl$zyy,xyg,"f_yy")
pfxxx <- gg1image2contours(xfg,yfg,ffg$fxxx,pdg$zxxx,pdl$zxxx,xyg,"f_xxx")
pfxxy <- gg1image2contours(xfg,yfg,ffg$fxxy,pdg$zxxy,pdl$zxxy,xyg,"f_xxy")
pfxyy <- gg1image2contours(xfg,yfg,ffg$fxyy,pdg$zxyy,pdl$zxyy,xyg,"f_xyy")
pfyyy <- gg1image2contours(xfg,yfg,ffg$fyyy,pdg$zyyy,pdl$zyyy,xyg,"f_yyy")

t2 <- print_f(f,df,1,0.9)

gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix=lay)


###################################################
### code chunk number 28: partDeriv.Rnw:801-802
###################################################
getOption("SweaveHooks")[["fig"]]()
pf <- gg1image2contours(xfg,yfg,ffg$f,pdg$z,pdl$z,xyg,"f")
pfx <- gg1image2contours(xfg,yfg,ffg$fx,pdg$zx,pdl$zx,xyg,"f_x")
pfy <- gg1image2contours(xfg,yfg,ffg$fy,pdg$zy,pdl$zy,xyg,"f_x")
pfxx <- gg1image2contours(xfg,yfg,ffg$fxx,pdg$zxx,pdl$zxx,xyg,"f_xx")
pfxy <- gg1image2contours(xfg,yfg,ffg$fxy,pdg$zxy,pdl$zxy,xyg,"f_xy")
pfyy <- gg1image2contours(xfg,yfg,ffg$fyy,pdg$zyy,pdl$zyy,xyg,"f_yy")
pfxxx <- gg1image2contours(xfg,yfg,ffg$fxxx,pdg$zxxx,pdl$zxxx,xyg,"f_xxx")
pfxxy <- gg1image2contours(xfg,yfg,ffg$fxxy,pdg$zxxy,pdl$zxxy,xyg,"f_xxy")
pfxyy <- gg1image2contours(xfg,yfg,ffg$fxyy,pdg$zxyy,pdl$zxyy,xyg,"f_xyy")
pfyyy <- gg1image2contours(xfg,yfg,ffg$fyyy,pdg$zyyy,pdl$zyyy,xyg,"f_yyy")

t2 <- print_f(f,df,1,0.9)

gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix=lay)


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
df <- derivs(f,dg)


###################################################
### code chunk number 33: partDeriv.Rnw:835-839
###################################################
## global bandwidth
pdg <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel=knl)
## local bandwidth:
pdl <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=bwl, solver="QR", degree=dg,kernel=knl)


###################################################
### code chunk number 34: partDeriv.Rnw:844-865
###################################################
pf <- gg1image2contours(xfg,yfg,ffg$f,pdg$z,pdl$z,xy,"f")
pfx <- gg1image2contours(xfg,yfg,ffg$fx,pdg$zx,pdl$zx,xy,"f_x")
pfy <- gg1image2contours(xfg,yfg,ffg$fy,pdg$zy,pdl$zy,xy,"f_x")
pfxx <- gg1image2contours(xfg,yfg,ffg$fxx,pdg$zxx,pdl$zxx,xy,"f_xx")
pfxy <- gg1image2contours(xfg,yfg,ffg$fxy,pdg$zxy,pdl$zxy,xy,"f_xy")
pfyy <- gg1image2contours(xfg,yfg,ffg$fyy,pdg$zyy,pdl$zyy,xy,"f_yy")
pfxxx <- gg1image2contours(xfg,yfg,ffg$fxxx,pdg$zxxx,pdl$zxxx,xy,"f_xxx")
pfxxy <- gg1image2contours(xfg,yfg,ffg$fxxy,pdg$zxxy,pdl$zxxy,xy,"f_xxy")
pfxyy <- gg1image2contours(xfg,yfg,ffg$fxyy,pdg$zxyy,pdl$zxyy,xy,"f_xyy")
pfyyy <- gg1image2contours(xfg,yfg,ffg$fyyy,pdg$zyyy,pdl$zyyy,xy,"f_yyy")

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
### code chunk number 35: plotbicubic2
###################################################
gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix = lay)


###################################################
### code chunk number 36: partDeriv.Rnw:872-873
###################################################
getOption("SweaveHooks")[["fig"]]()
gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix = lay)


###################################################
### code chunk number 37: partDeriv.Rnw:879-880
###################################################
f <- function(x,y) 0.75*exp(-((9*x-2)^2+(9*y-2)^2)/4)+0.75*exp(-((9*x+1)^2)/49-(9*y+1)/10)+0.5*exp(-((9*x-7)^2+(9*y-3)^2)/4)-0.2*exp(-(9*x-4)^2-(9*y-7)^2)


###################################################
### code chunk number 38: partDeriv.Rnw:882-886
###################################################
z <- f(x,y)
fg  <- outer(xg,yg,f)
ffg <- fgrid(f,xfg,yfg,dg)
df <- derivs(f,dg)


###################################################
### code chunk number 39: partDeriv.Rnw:888-893
###################################################
## global bandwidth:
ttg <- system.time(pdg <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel=knl))

## local bandwidth:
ttl <- system.time(pdl <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=bwl, solver="QR", degree=dg,kernel=knl))


###################################################
### code chunk number 40: partDeriv.Rnw:895-907
###################################################
pf <- gg1image2contours(xfg,yfg,ffg$f,pdg$z,pdl$z,xy,"f")
pfx <- gg1image2contours(xfg,yfg,ffg$fx,pdg$zx,pdl$zx,xy,"f_x")
pfy <- gg1image2contours(xfg,yfg,ffg$fy,pdg$zy,pdl$zy,xy,"f_x")
pfxx <- gg1image2contours(xfg,yfg,ffg$fxx,pdg$zxx,pdl$zxx,xy,"f_xx")
pfxy <- gg1image2contours(xfg,yfg,ffg$fxy,pdg$zxy,pdl$zxy,xy,"f_xy")
pfyy <- gg1image2contours(xfg,yfg,ffg$fyy,pdg$zyy,pdl$zyy,xy,"f_yy")
pfxxx <- gg1image2contours(xfg,yfg,ffg$fxxx,pdg$zxxx,pdl$zxxx,xy,"f_xxx")
pfxxy <- gg1image2contours(xfg,yfg,ffg$fxxy,pdg$zxxy,pdl$zxxy,xy,"f_xxy")
pfxyy <- gg1image2contours(xfg,yfg,ffg$fxyy,pdg$zxyy,pdl$zxyy,xy,"f_xyy")
pfyyy <- gg1image2contours(xfg,yfg,ffg$fyyy,pdg$zyyy,pdl$zyyy,xy,"f_yyy")

t2 <- print_f(f,df,1,0.9)


###################################################
### code chunk number 41: plotfranke12
###################################################
gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix = lay)


###################################################
### code chunk number 42: partDeriv.Rnw:914-915
###################################################
getOption("SweaveHooks")[["fig"]]()
gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix = lay)


###################################################
### code chunk number 43: partDeriv.Rnw:926-930
###################################################
## global bandwidth:
pdg <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel="uniform")
## local bandwidth:
pdl <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=bwl, solver="QR", degree=dg,kernel="uniform")


###################################################
### code chunk number 44: partDeriv.Rnw:932-951
###################################################
pf <- gg1image2contours(xfg,yfg,ffg$f,pdg$z,pdl$z,xy,"f")
pfx <- gg1image2contours(xfg,yfg,ffg$fx,pdg$zx,pdl$zx,xy,"f_x")
pfy <- gg1image2contours(xfg,yfg,ffg$fy,pdg$zy,pdl$zy,xy,"f_x")
pfxx <- gg1image2contours(xfg,yfg,ffg$fxx,pdg$zxx,pdl$zxx,xy,"f_xx")
pfxy <- gg1image2contours(xfg,yfg,ffg$fxy,pdg$zxy,pdl$zxy,xy,"f_xy")
pfyy <- gg1image2contours(xfg,yfg,ffg$fyy,pdg$zyy,pdl$zyy,xy,"f_yy")
pfxxx <- gg1image2contours(xfg,yfg,ffg$fxxx,pdg$zxxx,pdl$zxxx,xy,"f_xxx")
pfxxy <- gg1image2contours(xfg,yfg,ffg$fxxy,pdg$zxxy,pdl$zxxy,xy,"f_xxy")
pfxyy <- gg1image2contours(xfg,yfg,ffg$fxyy,pdg$zxyy,pdl$zxyy,xy,"f_xyy")
pfyyy <- gg1image2contours(xfg,yfg,ffg$fyyy,pdg$zyyy,pdl$zyyy,xy,"f_yyy")

t2 <- print_f(f,df,1,0.9)
t3 <- grid.text(paste(c(paste("kernel:","uniform"),
                        paste("global bandwidth",bwg*100,"%"),
                        paste("local bandwidth",bwl*100,"%")),
                      collapse="\n"),
                gp=gpar(fontsize=8),x=0,y=0.8,draw=FALSE,
                just = c("left","top"))



###################################################
### code chunk number 45: plotfranke12unif
###################################################
gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix = lay)


###################################################
### code chunk number 46: partDeriv.Rnw:958-959
###################################################
getOption("SweaveHooks")[["fig"]]()
gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix = lay)


###################################################
### code chunk number 47: partDeriv.Rnw:964-968
###################################################
## global bandwidth:
pdg <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=c(bwg,bwg), solver="QR", degree=dg,kernel="epanechnikov")
## local bandwidth:
pdl <- interp::locpoly(x,y,z, xfg,yfg, pd="all", h=bwl, solver="QR", degree=dg,kernel="epanechnikov")


###################################################
### code chunk number 48: partDeriv.Rnw:970-988
###################################################
pf <- gg1image2contours(xfg,yfg,ffg$f,pdg$z,pdl$z,xy,"f")
pfx <- gg1image2contours(xfg,yfg,ffg$fx,pdg$zx,pdl$zx,xy,"f_x")
pfy <- gg1image2contours(xfg,yfg,ffg$fy,pdg$zy,pdl$zy,xy,"f_x")
pfxx <- gg1image2contours(xfg,yfg,ffg$fxx,pdg$zxx,pdl$zxx,xy,"f_xx")
pfxy <- gg1image2contours(xfg,yfg,ffg$fxy,pdg$zxy,pdl$zxy,xy,"f_xy")
pfyy <- gg1image2contours(xfg,yfg,ffg$fyy,pdg$zyy,pdl$zyy,xy,"f_yy")
pfxxx <- gg1image2contours(xfg,yfg,ffg$fxxx,pdg$zxxx,pdl$zxxx,xy,"f_xxx")
pfxxy <- gg1image2contours(xfg,yfg,ffg$fxxy,pdg$zxxy,pdl$zxxy,xy,"f_xxy")
pfxyy <- gg1image2contours(xfg,yfg,ffg$fxyy,pdg$zxyy,pdl$zxyy,xy,"f_xyy")
pfyyy <- gg1image2contours(xfg,yfg,ffg$fyyy,pdg$zyyy,pdl$zyyy,xy,"f_yyy")

t2 <- print_f(f,df,1,0.9)
t3 <- grid.text(paste(c(paste("kernel:","epanechnikov"),
                        paste("global bandwidth",bwg*100,"%"),
                        paste("local bandwidth",bwl*100,"%")),
                      collapse="\n"),
                gp=gpar(fontsize=8),x=0,y=0.8,draw=FALSE,
                just = c("left","top"))


###################################################
### code chunk number 49: plotfranke12epa
###################################################
gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix = lay)


###################################################
### code chunk number 50: partDeriv.Rnw:995-996
###################################################
getOption("SweaveHooks")[["fig"]]()
gg <- grid.arrange(grobs=gList(ggplotGrob(pf),t1,t2,ggplotGrob(pfx),ggplotGrob(pfy),ggplotGrob(pfxx),ggplotGrob(pfxy),ggplotGrob(pfyy),t3,ggplotGrob(pfxxx),ggplotGrob(pfxxy),ggplotGrob(pfxyy),ggplotGrob(pfyyy)),layout_matrix = lay)


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

