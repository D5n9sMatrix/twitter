#!/usr/bin/r
# The loss or error of h at point (x, y) ∈ X × {−1, +1} is defined as the binary
# classification error of hf :
require(stats) # for rnorm
plot(-4:4, -4:4, type = "n")  # setting up coord. system
points(rnorm(200), rnorm(200), col = "red")
points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)

op <- par(bg = "light blue")
x <- seq(0, 2*pi, length.out = 51)
## something "between type='b' and type='o'":
plot(x, sin(x), type = "o", pch = 21, bg = par("bg"), col = "blue", cex = .6,
     main = 'plot(..., type="o", pch=21, bg=par("bg"))')
par(op)

## Not run: 
## The figure was produced by calls like
png("pch.png", height = 0.7, width = 7, res = 100, units = "in")
par(mar = rep(0,4))
plot(c(-1, 26), 0:1, type = "n", axes = FALSE)
text(0:25, 0.6, 0:25, cex = 0.5)
points(0:25, rep(0.3, 26), pch = 0:25, bg = "grey")

## End(Not run)

##-------- Showing all the extra & some char graphics symbols ---------
pchShow <-
  function(extras = c("*",".", "o","O","0","+","-","|","%","#"),
           cex = 3, ## good for both .Device=="postscript" and "x11"
           col = "red3", bg = "gold", coltext = "brown", cextext = 1.2,
           main = paste("plot symbols :  points (...  pch = *, cex =",
                        cex,")"))
  {
    nex <- length(extras)
    np  <- 26 + nex
    ipch <- 0:(np-1)
    k <- floor(sqrt(np))
    dd <- c(-1,1)/2
    rx <- dd + range(ix <- ipch %/% k)
    ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
    pch <- as.list(ipch) # list with integers & strings
    if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
    plot(rx, ry, type = "n", axes  =  FALSE, xlab = "", ylab = "", main = main)
    abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
    for(i in 1:np) {
      pc <- pch[[i]]
      ## 'col' symbols with a 'bg'-colored interior (where available) :
      points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
      if(cextext > 0)
        text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
    }
  }

pchShow()
pchShow(c("o","O","0"), cex = 2.5)
pchShow(NULL, cex = 4, cextext = 0, main = NULL)


## ------------ test code for various pch specifications -------------
# Try this in various font families (including Hershey)
# and locales.  Use sign = -1 asserts we want Latin-1.
# Standard cases in a MBCS locale will not plot the top half.
TestChars <- function(sign = 1, font = 1, ...)
{
  MB <- l10n_info()$MBCS
  r <- if(font == 5) { sign <- 1; c(32:126, 160:254)
  } else if(MB) 32:126 else 32:255
  if (sign == -1) r <- c(32:126, 160:255)
  par(pty = "s")
  plot(c(-1,16), c(-1,16), type = "n", xlab = "", ylab = "",
       xaxs = "i", yaxs = "i",
       main = sprintf("sign = %d, font = %d", sign, font))
  grid(17, 17, lty = 1) ; mtext(paste("MBCS:", MB))
  for(i in r) try(points(i%%16, i%/%16, pch = sign*i, font = font,...))
}
TestChars()
try(TestChars(sign = -1))
TestChars(font = 5)  # Euro might be at 160 (0+10*16).
# macOS has apple at 240 (0+15*16).
try(TestChars(-1, font = 2))  # bold

# We will denote by R(h) the expected error of h: R(h) = E(x,y)∼D 1hf (x)6=y . For
# any x ∈ X, let η(x) denote η(x) = P[y = +1|x] and let DX denote the marginal
# distribution over X. Then, for any h, we can write
## ----setup, echo=FALSE,message=FALSE-------------------------------------
require(knitr)
opts_chunk$set(fig.width=3,fig.height=3, tidy=FALSE, cache=FALSE, fig.path='gtable/')
require(gridExtra)
require(grid)
require(gtable)
require(ggplot2)

## ---- eval=FALSE---------------------------------------------------------
#  gtable(unit(1:3, c("cm")), unit(5, "cm"))

## ----matrix--------------------------------------------------------------
a <- rectGrob(gp = gpar(fill = "red"))
b <- grobTree(rectGrob(), textGrob("new\ncell"))
c <- ggplotGrob(qplot(1:10,1:10))
d <- linesGrob()
mat <- matrix(list(a, b, c, d), nrow = 2)
g <- data.frame(name = c("demo"), grobs = c("mat"), 
                widths = c("cm"), 
                heights =  c("lines"))
g

## ----gtable_arrange------------------------------------------------------
dummy_grob <- function(id)  {
  grobTree(rectGrob(gp=gpar(fill=id, alpha=0.5)), textGrob(id))
}
gs <- lapply(1:9, dummy_grob)
c(ncol=4, grobs=gs, 
  top="top\nlabel", bottom="bottom\nlabel", 
  left="left\nlabel", right="right\nlabel")
c(gp=gpar(fill=NA))

## ----gtable_from_layout--------------------------------------------------

gt <- arrangeGrob(grobs=gs, layout_matrix=rbind(c(1,1,1,2,3),
                                                c(1,1,1,4,5),
                                                c(6,7,8,9,9)))
c(gt)
c(gp=gpar(fill=NA))

## ------------------------------------------------------------------------
print(g)
names(g)

## ------------------------------------------------------------------------
length(g); nrow(g); ncol(g)

## ------------------------------------------------------------------------
length(g$grobs)

## ------------------------------------------------------------------------
g$layout

## ------------------------------------------------------------------------
g$widths; g$heights


### R code from vignette source 'overview.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
library(proxy)
x <- summary(pr_DB, "long")
FUN <- function(index) {
  for (i in which(index)) {
    writeLines(sprintf("Aliases: %s", paste(x$names[[i]], collapse = ", ")))
    writeLines(sprintf("Type   : %s", x$type[i]))
    writeLines(sprintf("Formula: %s\n", x$formula[i]))
  }
}


###################################################
### code chunk number 2: overview.Rnw:24-25
###################################################
FUN(x$distance == FALSE)


###################################################
### code chunk number 3: overview.Rnw:29-30
###################################################
FUN(x$distance == TRUE)


## ----setup, include=FALSE-----------------------------------------------------
library(sass)
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  fig.align = "center",
  out.width = "80%",
  class.output = "css",
  comment = ""
)

### R code from vignette source 'over.Rnw'

###################################################
### code chunk number 1: over.Rnw:105-107 (eval = FALSE)
###################################################
## A %over% B
## over(A, B)


###################################################
### code chunk number 2: over.Rnw:117-118 (eval = FALSE)
###################################################
## A[B,]


###################################################
### code chunk number 3: over.Rnw:121-122 (eval = FALSE)
###################################################
## A[!is.na(over(A,B)),]


###################################################
### code chunk number 4: over.Rnw:125-141
###################################################
library(sp)
x = c(0.5, 0.5, 1.0, 1.5)
y = c(1.5, 0.5, 0.5, 0.5)
xy = cbind(x,y)
dimnames(xy)[[1]] = c("a", "b", "c", "d")
pts = SpatialPoints(xy)

xpol = c(0,1,1,0,0)
ypol = c(0,0,1,1,0)
pol = SpatialPolygons(list(
  Polygons(list(Polygon(cbind(xpol-1.05,ypol))), ID="x1"),
  Polygons(list(Polygon(cbind(xpol,ypol))), ID="x2"),
  Polygons(list(Polygon(cbind(xpol,ypol - 1.0))), ID="x3"),
  Polygons(list(Polygon(cbind(xpol + 1.0, ypol))), ID="x4"),
  Polygons(list(Polygon(cbind(xpol+.4, ypol+.1))), ID="x5")
))


###################################################
### code chunk number 5: over.Rnw:146-158
###################################################
if (require(RColorBrewer, quietly = TRUE)) {
  pal = brewer.pal(5, "Set2")
  col = paste0(pal, "4D")
} else {
  pal = 1:5
  col = pal
}
plot.new()
plot(pol, xlim = c(-1.1, 2.1), ylim = c(-1.1, 1.6), border=pal, axes=TRUE, col = col)
points(pts, col='red')
text(c(-1,0.1,0.1,1.9,0.45), c(0.05,0.05,-.95,0.05,0.15), 
     c("x1", "x2", "x3", "x4", "x5"))
text(coordinates(pts), pos=1, row.names(pts))


###################################################
### code chunk number 6: over.Rnw:165-166
###################################################
over(pts, pol)


###################################################
### code chunk number 7: over.Rnw:173-174
###################################################
over(pts, pol, returnList = TRUE)


###################################################
### code chunk number 8: over.Rnw:180-181
###################################################
pts[pol]


###################################################
### code chunk number 9: over.Rnw:186-189
###################################################
over(pol, pts)
over(pol, pts, returnList = TRUE)
row.names(pol[pts])


###################################################
### code chunk number 10: over.Rnw:194-197
###################################################
if (require(rgeos, quietly = TRUE)) {
  over(pol, pol, returnList = TRUE)
}


###################################################
### code chunk number 11: over.Rnw:216-225
###################################################
zdf = data.frame(z1 = 1:4, z2=4:1, f = c("a", "a", "b", "b"),
                 row.names = c("a", "b", "c", "d"))
zdf
ptsdf = SpatialPointsDataFrame(pts, zdf)

zpl = data.frame(z = c(10, 15, 25, 3, 0), zz=1:5, 
                 f = c("z", "q", "r", "z", "q"), row.names = c("x1", "x2", "x3", "x4", "x5"))
zpl
poldf = SpatialPolygonsDataFrame(pol, zpl)


###################################################
### code chunk number 12: over.Rnw:229-230
###################################################
over(pts, poldf)


###################################################
### code chunk number 13: over.Rnw:238-239
###################################################
over(pts, poldf[1:2], fn = mean)


###################################################
### code chunk number 14: over.Rnw:244-245
###################################################
over(pts, poldf, returnList = TRUE)


###################################################
### code chunk number 15: over.Rnw:249-251
###################################################
over(pol, ptsdf)
over(pol, ptsdf[1:2], fn = mean)


###################################################
### code chunk number 16: over.Rnw:269-272
###################################################
l1 = Lines(Line(coordinates(pts)), "L1")
l2 = Lines(Line(rbind(c(1,1.5), c(1.5,1.5))), "L2")
L = SpatialLines(list(l1,l2))


###################################################
### code chunk number 17: over.Rnw:296-300
###################################################
plot(pol, xlim = c(-1.1, 2.1), ylim = c(-1.1, 1.6), border=2:6, axes=TRUE)
text(c(-1,0.1,0.1,1.1,0.45), c(0,0,-1.05,0,0.1), c("x1", "x2", "x3", "x4", "x5"))
lines(L, col = 'green')
text(c(0.52, 1.52), c(1.5, 1.5), c("L1", "L2"))


###################################################
### code chunk number 18: over.Rnw:308-318
###################################################
if (require(rgeos, quietly = TRUE)) {
  over(pol, pol)
  over(pol, pol,returnList = TRUE)
  over(pol, L)
  over(L, pol)
  over(L, pol, returnList = TRUE)
  over(L, L)
  over(pts, L)
  over(L, pts)
}


###################################################
### code chunk number 19: over.Rnw:323-333
###################################################
data(meuse.grid)
gridded(meuse.grid) = ~x+y
Pt = list(x = c(178274.9,181639.6), y = c(329760.4,333343.7))
sl = SpatialLines(list(Lines(Line(cbind(Pt$x,Pt$y)), "L1")))
image(meuse.grid)
if (require(rgeos, quietly = TRUE)) {
  xo = over(sl, geometry(meuse.grid), returnList = TRUE)
  image(meuse.grid[xo[[1]], ],col=grey(0.5),add=T)
  lines(sl)
}


###################################################
### code chunk number 20: over.Rnw:344-349
###################################################
g = SpatialGrid(GridTopology(c(0,0), c(1,1), c(3,3)))
p = as(g, "SpatialPolygons")
px = as(g, "SpatialPixels")
plot(g)
text(coordinates(g), labels = 1:9)


###################################################
### code chunk number 21: over.Rnw:353-359
###################################################
if (require(rgeos, quietly = TRUE)) {
  over(g,g)
  over(p,p)
  over(p,g)
  over(g,p)
}


###################################################
### code chunk number 22: over.Rnw:364-368
###################################################
if (require(rgeos, quietly = TRUE)) {
  over(px[5], g, returnList = TRUE)
  over(p[c(1,5)], p, returnList = TRUE)
}


###################################################
### code chunk number 23: over.Rnw:384-388
###################################################
if (require(rgeos, quietly = TRUE)) {
  over(px[5], g, returnList = TRUE, minDimension = 0)
  over(p[c(1,5)], p, returnList = TRUE, minDimension = 0)
}


###################################################
### code chunk number 24: over.Rnw:408-411
###################################################
if (require(rgeos, quietly = TRUE)) {
  over(p, p, minDimension = 0)
}


###################################################
### code chunk number 25: over.Rnw:415-426
###################################################
x2 = x1 = cbind(c(0,1,1,0,0), c(0,0,1,1,0))
x1[,1] = x1[,1]+0.5
x1[,2] = x1[,2]+0.25
sp = SpatialPolygons(list(
  Polygons(list(Polygon(x1)), "x1"),
  Polygons(list(Polygon(x2)), "x2")))
pt = SpatialPoints(cbind(0.5,0.5)) # on border of x1
row.names(pt) = "pt1"
plot(sp, axes = TRUE)
text(c(0.05, 0.55, 0.55), c(0.9, 1.15, 0.5), c("x1","x2", "pt"))
plot(pt, add=TRUE, col='red', pch=16)


###################################################
### code chunk number 26: over.Rnw:432-440
###################################################
if (require(rgeos, quietly = TRUE)) {
  over(pt,sp)
  over(pt,sp,minDimension=0)
  over(pt,sp,returnList=TRUE)
  rgeos::overGeomGeom(pt,sp)
  rgeos::overGeomGeom(pt,sp,returnList=TRUE)
  rgeos::overGeomGeom(pt,sp,returnList=TRUE,minDimension=0)
}


###################################################
### code chunk number 27: over.Rnw:456-464
###################################################
if (require(rgeos, quietly = TRUE)) {
  over(p[5], p, returnList=TRUE, minDimension=0)
  over(p[5], p, returnList=TRUE, minDimension=1)
  over(p[5], p, returnList=TRUE, minDimension=2)
  rgeos::overGeomGeom(pt, pt, minDimension=2) # empty
  rgeos::overGeomGeom(pt, pt, minDimension=1) # empty
  rgeos::overGeomGeom(pt, pt, minDimension=0)
}


###################################################
### code chunk number 28: over.Rnw:470-476
###################################################
data(meuse.grid)
gridded(meuse.grid) = ~x+y
off = gridparameters(meuse.grid)$cellcentre.offset + 20
gt = GridTopology(off, c(400,400), c(8,11))
SG = SpatialGrid(gt)
agg = aggregate(meuse.grid[3], SG, mean)


###################################################
### code chunk number 29: over.Rnw:486-488
###################################################
image(agg)
points(meuse.grid, pch = 3, cex=.2, col = "#80808080")


###################################################
### code chunk number 30: over.Rnw:497-502
###################################################
if (require(rgeos, quietly = TRUE)) {
  sl.agg = aggregate(meuse.grid[,1:3], sl, mean)
  class(sl.agg)
  as.data.frame(sl.agg)
}


###################################################
### code chunk number 31: over.Rnw:515-526
###################################################
if (require(rgeos, quietly = TRUE)) {
  g = SpatialGrid(GridTopology(c(5,5), c(10,10), c(3,3)))
  p = as(g, "SpatialPolygons")
  p$z = c(1,0,1,0,1,0,1,0,1)
  cc = coordinates(g)
  p$ag1 = aggregate(p, p, mean)[[1]]
  p$ag1a = aggregate(p, p, mean, minDimension = 0)[[1]]
  p$ag2 = aggregate(p, p, mean, minDimension = 1)[[1]]
  p$ag3 = aggregate(p, p, mean, minDimension = 2)[[1]]
  p$ag4 = aggregate(p, p, areaWeighted=TRUE)[[1]]
}


###################################################
### code chunk number 32: over.Rnw:530-549
###################################################
if (require(rgeos, quietly = TRUE)) {
  pts = cbind(c(9,21,21,9,9),c(9,9,21,21,9))
  sq = SpatialPolygons(list(Polygons(list(Polygon(pts)), "ID")))
  rnd2 = function(x) round(x, 2)
  l = list(
    list("sp.text", cc, rnd2(p$z), which = 1),
    list("sp.text", cc, rnd2(p$ag1), which = 2),
    list("sp.text", cc, rnd2(p$ag1a), which = 3),
    list("sp.text", cc, rnd2(p$ag2), which = 4),
    list("sp.text", cc, rnd2(p$ag3), which = 5),
    list("sp.text", cc, rnd2(p$ag4), which = 6),
    list(sq, col = 'green', which = 6, first = FALSE, lwd = 2)
  )
  spplot(p, names.attr = c("source", "default aggregate", "minDimension=0", 
                           "minDimension=1", "minDimension=2", "areaWeighted=TRUE"), layout = c(3,2), 
         as.table=TRUE, col.regions=bpy.colors(151)[50:151], cuts=100, 
         sp.layout = l, scales = list(draw = TRUE))
} else
  plot(1)


###################################################
### code chunk number 33: over.Rnw:566-574
###################################################
if (require(rgeos, quietly = TRUE)) {
  round(c(
    aggDefault = aggregate(p, sq, mean)[[1]],
    aggMinDim0 = aggregate(p, sq, mean, minDimension = 0)[[1]],
    aggMinDim1 = aggregate(p, sq, mean, minDimension = 1)[[1]],
    aggMinDim2 = aggregate(p, sq, mean, minDimension = bb2)[[1]],
    areaWeighted = aggregate(p, sq, areaWeighted=TRUE)[[1]]), 3)
}

# Lemma 4.5 The excess error of any hypothesis h : X → R can be expressed as follows
# in terms of η and the Bayes scoring function h∗ :
## ----include=FALSE-------------------------------------------------------
library(knitr)

## ------------------------------------------------------------------------
library(xtable)
x <- matrix(rnorm(6), ncol = 2)
x.small <- xtable(x, label = 'tabsmall', caption = 'A margin table')

## ----results='asis'------------------------------------------------------
print(x.small,floating.environment='margintable',
      latex.environments = "",
      table.placement = NULL)
# Proof:
# For any h, we can write
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
## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#  renv::settings$snapshot.type("explicit")

# where we used for the last step equation (4.9). In view of that, for any h, the
# following holds:
tmat <- c(c(3,2,2,4), 
          c(22,1,2,21,3,20,19,4,18,17,5,16,15,6,7, 8,14,9,10,13,11,12),
          matrix(c(1,0,1,1,0,0,1,1,0,1,0,10,0,
                   0,1,1,0,1,1,0,1,1,0,1,0,10), ncol=2))
dim(tmat)

# which completes the proof, since R(h∗ ) = R∗ .
# Let Φ : R → R be a convex and non-decreasing function so that for any u ∈ R,
# 1u≤0 ≤ Φ(−u). The Φ-loss of a function h : X → R at point (x, y) ∈ X × {−1, +1}
# is defined as Φ(−uh(x)) and its expected loss given by
data(iris)
bc1 <- c(iris[,1:4], 3, base.centers=5)
boxplot(bc1)

# Notice that since 1uh(x)≤0 ≤ Φ(−uh(x)), we have R(h) ≤ LΦ (h). For any x ∈ X,
# let u 7→ LΦ (x, u) be the function defined for all u ∈ R by
## DST Rules for Zurich:
head(mtcars)
tail(mtcars)

# Then, LΦ (h) = Ex∼DX [LΦ (x, h(x))]. Since Φ is convex, u 7→ LΦ (x, u) is convex as
# a sum of two convex functions. Define h∗Φ : X → [−∞, +∞] as the Bayes solution
# for the loss function LΦ . That is, for any x, h∗Φ (x) is a solution of the following
# convex optimization problem:
x <- time(x = c(2023, 01, 30))

# The number of days since 1970-01-01 UTC
as.data.frame(x)

x <- x + as.data.frame(1)
x

# The number of seconds since 1970-01-01 00:00:00 UTC
as.data.frame(x)

# The solution of this optimization is in general not unique. When η(x) = 0, h∗Φ (x) is a
# minimize of u 7→ Φ(u) and since Φ is non-decreasing, we can choose h∗Φ (x) = −∞ in
# that case. Similarly, when η(x) = 1, we can choose h∗Φ (x) = +∞. When η(x) = 21 ,
# LΦ (x, u) = 12 [Φ(−u) + Φ(u)], thus, by convexity, LΦ (x, u) ≥ Φ(− u2 + u2 ) = Φ(0).
# Thus, we can choose h∗Φ (x) = 0 in that case. For all other values of η(x), in case of
# non-uniqueness, an arbitrary minimize is chosen in th
promax(x, m = 4)

# Proposition 4.6 Let Φ be a convex and non-decreasing function that is differential
# at 0 with Φ0 (0) > 0. Then, the minimize of Φ defines the Bayes classifier: for
# any x ∈ X, h∗Φ (x) > 0 off h∗ (x) > 0 and h∗ (x) = 0 off h∗Φ (x) = 0, which implies
# L∗Φ = R∗ .
## ---- echo=FALSE--------------------------------------------------------------
library(diffobj)
old.opt <- options(
  diffobj.disp.width=80, diffobj.pager="off", diffobj.format="html"
)

## ---- results="asis"----------------------------------------------------------
a <- b <- matrix(1:100, ncol=2)
a <- a[-20,]
b <- b[-45,]
b[c(18, 44)] <- 999
diffPrint(target=a, current=b)

## ---- results="asis", echo=FALSE----------------------------------------------
diffPrint(target=a, current=b)[1]

## ---- results="asis", echo=FALSE----------------------------------------------
diffPrint(target=a, current=b)[2:10]

## ---- results="asis", echo=FALSE----------------------------------------------
diffPrint(target=a, current=b)[3]

## ---- results="asis", echo=FALSE----------------------------------------------
diffPrint(target=a, current=b)[6:9]

## ---- results="asis", echo=FALSE----------------------------------------------
diffPrint(target=a, current=b)[8:9]

## ---- results="asis"----------------------------------------------------------
state.abb2 <- state.abb[-16]
state.abb2[37] <- "Pennsylvania"
diffPrint(state.abb, state.abb2)

## ---- results="asis"----------------------------------------------------------
mdl1 <- lm(Sepal.Length ~ Sepal.Width, iris)
mdl2 <- lm(Sepal.Length ~ Sepal.Width + Species, iris)
diffStr(mdl1$qr, mdl2$qr, line.limit=15)

## ---- results="asis"----------------------------------------------------------
diffChr(letters[1:3], c("a", "B", "c"))

## ---- eval=FALSE--------------------------------------------------------------
#  x <- diffPrint(letters, LETTERS)
#  x   # or equivalently: `show(x)`

## ---- results="asis"----------------------------------------------------------
summary(diffStr(mdl1, mdl2))

## ---- results="asis", eval=FALSE----------------------------------------------
#  x <- y <- letters[24:26]
#  y[2] <- "GREMLINS"
#  diffChr(x, y)

## ---- results="asis", echo=FALSE----------------------------------------------
x <- y <- letters[24:26]
y[2] <- "GREMLINS"
diffChr(x, y, mode="sidebyside")

## ---- results="asis", echo=FALSE----------------------------------------------
x <- y <- letters[24:26]
y[2] <- "GREMLINS"
diffChr(x, y, mode="unified")

## ---- results="asis", echo=FALSE----------------------------------------------
x <- y <- letters[24:26]
y[2] <- "GREMLINS"
diffChr(x, y, mode="context")

## ---- results="asis"----------------------------------------------------------
diffChr(x, y, color.mode="rgb")

## ---- eval=FALSE--------------------------------------------------------------
#  v1 <- 1:5e4
#  v2 <- v1[-sample(v1, 100)]
#  diffChr(v1, v2, word.diff=FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  diffPrint(v1, v2)

## -----------------------------------------------------------------------------
ses(letters[1:5], letters[c(2:3, 5)])

## ---- echo=FALSE--------------------------------------------------------------
options(old.opt)


## ----echo=FALSE---------------------------------------------------------------
library(diffobj)

## ----results='asis'-----------------------------------------------------------
cat(
  as.character(
    diffPrint(
      1:5, 2:6,
      format="html",
      style=list(html.output="diff.w.style")
    ) ) )

## ----results='asis'-----------------------------------------------------------
cat(
  as.character(
    diffPrint(
      1:5, 2:6,
      format="html",
      style=list(html.output="diff.only")   # notice this changed
    ) ) )

## ----eval=FALSE---------------------------------------------------------------
#  options(
#    diffobj.format="html",
#    diffobj.style=list(html.output="diff.only")
#  )

## ----echo=FALSE---------------------------------------------------------------
old.opts <- options(
  diffobj.format="html",
  diffobj.style=list(html.output="diff.only")
)

## ----results='asis'-----------------------------------------------------------
cat(as.character(diffPrint(1:5, 2:6)))

## ----echo=FALSE---------------------------------------------------------------
options(old.opts)

## ---- eval=FALSE--------------------------------------------------------------
#  library(shiny)
#  shinyApp(
#    ui=fluidPage(htmlOutput('diffobj_element')),
#    server=function(input, output) {
#      output$diffobj_element <- renderUI({
#        HTML(
#          as.character(
#            diffPrint(
#              1:5, 2:6,
#              format="html",
#              style=list(html.output="diff.w.style")
#  ) ) )}) } )

## ---- eval=FALSE--------------------------------------------------------------
#  options(
#    diffobj.format="html",
#    diffobj.style=list(html.output="diff.only")
#  )
#  shinyApp(
#    ui=fluidPage(
#      includeCSS(diffobj_css()),
#      htmlOutput('diffobj_element')
#    ),
#    server=function(input, output) {
#      output$diffobj_element <- renderUI({
#        HTML(as.character(diffPrint(1:5, 2:6,)))
#  }) } )

# Fix x ∈ X. If η(x) = 0, then h∗ (x) = − 21 and h∗Φ (x) = −∞, thus h∗ (x)
# and h∗Φ (x) admit the same sign. Similarly, if η(x) = 1, then h∗ (x) = + 21 and
# h∗Φ (x) = +∞, and h∗ (x) and h∗Φ (x) admit the same sign.
# Let u∗ denote the minimize defining h∗Φ (x). u∗ is a minimize of u 7→ LΦ (x, u)
# off the sub differential of that function at u∗ contains 0, that is, since ∂LΦ (x, u∗ ) =
#   −η(x)∂Φ(−u∗ ) + (1 − η(x))∂Φ(u∗ ), off there exist v1 ∈ ∂Φ(−u∗ ) and v2 ∈ ∂Φ(u∗ )
# such that
## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

library(rlang)
fail <- function() "\u274c"
pass <- function() "\u2705"

## -----------------------------------------------------------------------------
#  bar(foo(x))

## -----------------------------------------------------------------------------
#  local({
#    . <- x
#    . <- foo(.)
#    bar(.)
#  })

## -----------------------------------------------------------------------------
#  local({
#    ...1 <- x
#    ...2 <- foo(...1)
#    bar(...2)
#  })

## -----------------------------------------------------------------------------
#  with_dot_cleanup <- function(expr) {
#    # Initialises `.` in the caller environment and resets it on exit.
#    # (We use `:=` instead of `=` to avoid partial matching.)
#    rlang::local_bindings(. := NULL, .env = parent.frame())
#    expr
#  }
#  with_dot_cleanup({
#    . <- x
#    . <- foo(.)
#    bar(.)
#  })

## -----------------------------------------------------------------------------
#  mask1 <- new.env(parent = env)
#  mask2 <- new.env(parent = env)
#  
#  delayedAssign(".", x, mask1)
#  delayedAssign(".", foo(.), mask2)
#  with(mask2, bar(.))

## -----------------------------------------------------------------------------
#  local({
#    delayedAssign("...1", x)
#    delayedAssign("...2", foo(...1))
#    bar(...2)
#  })

## -----------------------------------------------------------------------------
#  delayedAssign("...1", x)
#  delayedAssign("...2", foo(.))
#  bar(...2)

## -----------------------------------------------------------------------------
#  sample(10) %>% list(., .)
#  
#  # Becomes
#  list(sample(10), sample(10))

## -----------------------------------------------------------------------------
#  sample(10) %>% foo(., .)
#  foo(. <- sample(10), .)

## ---- eval = TRUE-------------------------------------------------------------
`%|>%` <- magrittr::pipe_nested

## ---- eval = TRUE, error = TRUE-----------------------------------------------
"foo" %|>% list(., .)

## ---- eval = TRUE-------------------------------------------------------------
{
  stop("oh no") %|>% try(silent = TRUE)
  "success"
}

## ---- eval = TRUE-------------------------------------------------------------
factory <- function(x) function() x
fn <- factory(TRUE)
fn()

## -----------------------------------------------------------------------------
#  fn <- TRUE %|>% factory()
#  fn()

## -----------------------------------------------------------------------------
#  faulty <- function() stop("tilt")
#  f <- function(x) x + 1
#  g <- function(x) x + 2
#  h <- function(x) x + 3
#  
#  faulty() %|>% f() %|>% g() %|>% h()
#  #> Error in faulty() : tilt
#  
#  traceback()
#  #> 7: stop("tilt")
#  #> 6: faulty()
#  #> 5: f(faulty())
#  #> 4: g(f(faulty()))
#  #> 3: h(g(f(faulty())))
#  #> 2: .External2(magrittr_pipe) at pipe.R#181
#  #> 1: faulty() %|>% f() %|>% g() %|>% h()

## ---- eval = TRUE-------------------------------------------------------------
foo <- FALSE
TRUE %|>% assign("foo", .)
foo

## ---- eval = TRUE-------------------------------------------------------------
fn <- function() {
  TRUE %|>% return()
  FALSE
}
fn()

## -----------------------------------------------------------------------------
#  options(error = rlang::entrace)

## -----------------------------------------------------------------------------
#  foobar <- function(x) x %|>% quux()
#  quux <- function(x) x %|>% stop()
#  
#  "tilt" %|>% foobar()
#  #> Error in x %|>% stop() : tilt
#  
#  rlang::last_trace()
#  #> <error/rlang_error>
#  #> tilt
#  #> Backtrace:
#  #>     █
#  #>  1. ├─"tilt" %|>% foobar()
#  #>  2. └─global::foobar("tilt")
#  #>  3.   ├─x %|>% quux()
#  #>  4.   └─global::quux(x)
#  #>  5.     └─x %|>% stop()

## ---- eval = TRUE-------------------------------------------------------------
`%!>%` <- magrittr::pipe_eager_lexical


## ---- eval = TRUE-------------------------------------------------------------
fn <- TRUE %!>% factory() %!>% { .() }
fn()

## ---- eval = TRUE-------------------------------------------------------------
. <- "wrong"
fn <- TRUE %!>% factory()
fn()

## -----------------------------------------------------------------------------
#  faulty <- function() stop("tilt")
#  f <- function(x) x + 1
#  g <- function(x) x + 2
#  h <- function(x) x + 3
#  
#  faulty() %!>% f() %!>% g() %!>% h()
#  #> Error in faulty() : tilt
#  
#  traceback()
#  #> 4: stop("tilt")
#  #> 3: faulty()
#  #> 2: .External2(magrittr_pipe) at pipe.R#163
#  #> 1: faulty() %!>% f() %!>% g() %!>% h()

## ---- eval = TRUE-------------------------------------------------------------
foo <- FALSE
NA %!>% { foo <- TRUE; . }

foo

## ---- eval = TRUE-------------------------------------------------------------
fn <- function() {
  TRUE %!>% return()
  
  FALSE
}
fn()

## ---- eval = TRUE-------------------------------------------------------------
`%?>%` <- magrittr::pipe_lazy_masking

## ---- eval = TRUE-------------------------------------------------------------
"foo" %?>% list(., .)

## ---- eval = TRUE-------------------------------------------------------------
{
  stop("oh no") %?>% try(silent = TRUE)
  "success"
}

## ---- eval = TRUE-------------------------------------------------------------
fn <- TRUE %?>% factory()
fn()

## -----------------------------------------------------------------------------
#  faulty <- function() stop("tilt")
#  f <- function(x) x + 1
#  g <- function(x) x + 2
#  h <- function(x) x + 3
#  
#  faulty() %?>% f() %?>% g() %?>% h()
#  #> Error in faulty() : tilt
#  
#  traceback()
#  #> 7: stop("tilt")
#  #> 6: faulty()
#  #> 5: f(.)
#  #> 4: g(.)
#  #> 3: h(.)
#  #> 2: .External2(magrittr_pipe) at pipe.R#174
#  #> 1: faulty() %?>% f() %?>% g() %?>% h()

## ---- eval = TRUE-------------------------------------------------------------
foo <- FALSE
TRUE %?>% assign("foo", .)
foo

## ---- eval = TRUE, error = TRUE-----------------------------------------------
fn <- function() {
  TRUE %?>% return()
  FALSE
}
fn()

## -----------------------------------------------------------------------------
#  foobar <- function(x) x %?>% quux()
#  quux <- function(x) x %?>% stop()
#  
#  "tilt" %?>% foobar()
#  #> Error in x %?>% stop() : tilt
#  
#  rlang::last_trace()
#  #> <error/rlang_error>
#  #> tilt
#  #> Backtrace:
#  #>     █
#  #>  1. ├─"tilt" %?>% foobar()
#  #>  2. ├─global::foobar(.)
#  #>  3. │ └─x %?>% quux()
#  #>  4. └─global::quux(.)
#  #>  5.   └─x %?>% stop()

