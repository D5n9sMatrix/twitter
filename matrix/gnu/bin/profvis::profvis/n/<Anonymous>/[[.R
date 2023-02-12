#!/usr/bin/r
# If u∗ = 0, by the differentiation of Φ at 0 we have v1 = v2 = Φ0 (0) > 0 and thus
# η(x) = 12 , that is h∗ (x) = 0. Conversely, If h∗ (x) = 0, that is η(x) = 21 , then, by
# definition, we have h∗Φ (x) = 0. Thus, h∗ (x) = 0 off h∗Φ (x) = 0 off η(x) = 21 .

# construct an R function for the Burr probability density
# function (PDF) given the Burr cumulative distribution function (CDF)
BurrCDF <- function(x, c = 1, k = 1) 1-(1+x^c)^-k

# transfer CDF to Caracas
c(BurrCDF)

# create a template for the PDF from the CDF
BurrPDF <- BurrCDF

# differentiate CDF and place resulting expression in body
body(BurrPDF) <- c(expression(deriv(BurrCDF(x,c,k))))[[1]]

# Summing up these inequalities yields v2 (u2 − u1 ) ≥ v1 (u2 − u1 ) and thus v2 ≥ v1 ,
# since u1 < u2 .
# Now, if u∗ > 0, then we have −u∗ < u∗ . By the property shown above, this
# implies v1 ≤ v2 . We cannot have v1 = v2 6= 0 since (4.11) would then imply
# η(x) = 21 . We also cannot have v1 = v2 = 0 since by the property shown above, we
# must have Φ0 (0) ≤ v2 and thus v2 > 0. Thus, we must have v1 < v2 with v2 > 0,
# which, by (4.11), implies η(x) > 1 − η(x), that is h∗ (x) > 0.
# Conversely, if h∗ (x) > 0 then η(x) > 1 − η(x). We cannot have v1 = v2 = 0
# or v1 = v2 6= 0 as already shown. Thus, since η(x) 6= 1, by (4.11), this implies
# v1 < v2 . We cannot have u∗ < −u∗ since, by the property shown above, this would
# imply v2 ≤ v1 . Thus, we must have −u∗ ≤ u∗ , that is u∗ ≥ 0, and more specifically
# u∗ > 0 since, as already shown above, u∗ = 0 implies h∗ (x)
con <- c(RSQLite::SQLite(), ":memory:")

c(con, "mtcars", mtcars)
c(con, "mtcars")

c(con)

# Theorem 4.7 Let Φ be a convex and non-decreasing function. Assume that there
# exists s ≥ 1 and c > 0 such that the following holds for all x ∈ X:
require(sp)
crs = CRS("+init=epsg:28992")
data("meuse")
coordinates(meuse) <- ~x+y
proj4string(meuse) <- crs
data("meuse.grid")
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
proj4string(meuse.grid) <- crs
data("meuse.riv")
meuse.riv <- SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)),"meuse.riv")))
proj4string(meuse.riv) <- crs
data("meuse.area")
meuse.area = SpatialPolygons(list(Polygons(list(Polygon(meuse.area)), "area")))
proj4string(meuse.area) <- crs  

# Proof:
# We will use the following inequality which holds by the convexity of Φ:
sort(x, decreasing = FALSE)

# which completes the proof, since Ex∼DX [LΦ (x, h∗Φ (x))] = L∗Φ .
# The theorem shows that, when the assumption holds, the excess error of h can
# be upper bounded in terms of the excess Φ-loss. The assumption of the theorem
# holds in particular for the following convex loss functions:
#   Hinge loss, where Φ(u) = max(0, 1 + u), with s = 1 and c = 21 .
# • Exponential loss, where Φ(u) = exp(u), with s = 2 and c = √ .
# • Logistic loss, where Φ(u) = log (1 + e ), with s = 2 and c = √ .
# They also hold for the square loss and the squared Hinge loss (see Exercises 
# 4.2 and 4.3).
## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(clock)
library(magrittr)

## ---- eval=FALSE--------------------------------------------------------------
#  zoned_time_now("")
#  #> <zoned_time<nanosecond><America/New_York (current)>[1]>
#  #> [1] "2021-02-10T15:54:29.875011000-05:00"

## ---- eval=FALSE--------------------------------------------------------------
#  zoned_time_now("Asia/Shanghai")
#  #> <zoned_time<nanosecond><Asia/Shanghai>[1]>
#  #> [1] "2021-02-11T04:54:29.875011000+08:00"

## -----------------------------------------------------------------------------
my_time <- year_month_day(2023, 01, 31, 1:02) %>%
  as_naive_time() %>%
  as_zoned_time("America/New_York")

my_time

their_time <- zoned_time_set_zone(my_time, "Asia/Shanghai")

their_time

## -----------------------------------------------------------------------------
my_time <- as.POSIXct("2023-01-31 01:02:00", "America/New_York")

date_set_zone(my_time, "Asia/Shanghai")

## -----------------------------------------------------------------------------
my_time <- year_month_day(2023, 01, 31, 01:02) %>%
  as_naive_time() %>%
  as_zoned_time("America/New_York")

my_time

# Drop the time zone information, retaining the printed time
my_time %>%
  as_naive_time()

# Add the correct time zone name back on,
# again retaining the printed time
their_9am <- my_time %>%
  as_naive_time() %>%
  as_zoned_time("Asia/Shanghai")

their_9am

## -----------------------------------------------------------------------------
zoned_time_set_zone(their_9am, "America/New_York")

## -----------------------------------------------------------------------------
my_time <- as.POSIXct("2023-01-31 01:05:00", "America/New_York")

my_time %>%
  as_naive_time() %>%
  as.POSIXct("Asia/Shanghai") %>%
  date_set_zone("America/New_York")

## -----------------------------------------------------------------------------
days <- as_naive_time(year_month_day(2023, c(1, 2), 1))

# A Tuesday and a Friday
as_weekday(days)

monday <- weekday(clock_weekdays$monday)

time_point_shift(days, monday)

as_weekday(time_point_shift(days, monday))

## -----------------------------------------------------------------------------
time_point_shift(days, monday, which = "previous")

## -----------------------------------------------------------------------------
tuesday <- weekday(clock_weekdays$tuesday)

time_point_shift(days, tuesday)
time_point_shift(days, tuesday, boundary = "advance")

## -----------------------------------------------------------------------------
next_weekday <- function(x, target) {
  x + (target - as_weekday(x))
}

next_weekday(days, monday)

as_weekday(next_weekday(days, monday))

## -----------------------------------------------------------------------------
monday - as_weekday(days)

## -----------------------------------------------------------------------------
days + (monday - as_weekday(days))

## -----------------------------------------------------------------------------
next_weekday2 <- function(x, target) {
  x <- x + duration_days(1L)
  x + (target - as_weekday(x))
}

a_monday <- as_naive_time(year_month_day(2023, 01, 31))
as_weekday(a_monday)

next_weekday2(a_monday, monday)

## -----------------------------------------------------------------------------
monday <- weekday(clock_weekdays$monday)

x <- as.Date(c("2023-01-31", "2023-02-01"))

date_shift(x, monday)

# With a date-time
y <- as.POSIXct(
  c("2023-01-31 01:09:00", "2023-02-01 06:20:22"), 
  "America/New_York"
)

date_shift(y, monday)

## -----------------------------------------------------------------------------
ym <- seq(year_month_day(2023, 1), by = 2, length.out = 10)
ym

## -----------------------------------------------------------------------------
yq <- seq(year_quarter_day(2023, 1), by = 2, length.out = 10)

## -----------------------------------------------------------------------------
set_day(ym, "last")

set_day(yq, "last")

## -----------------------------------------------------------------------------
from <- as_naive_time(year_month_day(2023, 1, 1))
to <- as_naive_time(year_month_day(2023, 5, 15))

seq(from, to, by = 20)

## -----------------------------------------------------------------------------
from <- as_naive_time(year_month_day(2023, 1, 1, 2, 30, 00))
to <- as_naive_time(year_month_day(2023, 1, 1, 12, 30, 00))

seq(from, to, by = duration_minutes(90))

## -----------------------------------------------------------------------------
date_seq(date_build(2023, 1), by = 2, total_size = 10)

## -----------------------------------------------------------------------------
date_seq(date_build(2023, 1), by = duration_months(2), total_size = 10)


## ---- error=TRUE--------------------------------------------------------------
jan31 <- date_build(2023, 1, 31)
dec31 <- date_build(2023, 12, 31)


## -----------------------------------------------------------------------------
date_seq(jan31, to = dec31, by = duration_months(1), invalid = "previous")

## -----------------------------------------------------------------------------
seq(jan31, to = dec31, by = "1 month")

## -----------------------------------------------------------------------------
from <- as_naive_time(year_month_day(2023, 1, 1))
to <- as_naive_time(year_month_day(2023, 12, 31))

x <- seq(from, to, by = duration_days(20))

x

## -----------------------------------------------------------------------------
ymd <- as_year_month_day(x)

head(ymd)

calendar_group(ymd, "month")

## -----------------------------------------------------------------------------
yqd <- as_year_quarter_day(x)

head(yqd)

calendar_group(yqd, "quarter")

## -----------------------------------------------------------------------------
calendar_group(ymd, "month", n = 2)

calendar_group(yqd, "quarter", n = 2)

## -----------------------------------------------------------------------------
x <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = 20)

date_group(x, "month")

## -----------------------------------------------------------------------------
x %>%
  as_year_quarter_day() %>%
  calendar_group("quarter") %>%
  set_day(1) %>%
  as.Date()

## -----------------------------------------------------------------------------
x %>%
  as_year_quarter_day(start = clock_months$june) %>%
  calendar_group("quarter") %>%
  set_day(1) %>%
  as.Date()

## -----------------------------------------------------------------------------
from <- as_naive_time(year_month_day(2023, 1, 1))
to <- as_naive_time(year_month_day(2023, 12, 31))

x <- seq(from, to, by = duration_days(20))

## -----------------------------------------------------------------------------
time_point_floor(x, "day", n = 60)

## -----------------------------------------------------------------------------
unclass(x[1])

## -----------------------------------------------------------------------------
x <- seq(as_naive_time(year_month_day(2023, 1, 1)), by = 3, length.out = 10)
x

thursdays <- time_point_floor(x, "day", n = 14)
thursdays

as_weekday(thursdays)

## -----------------------------------------------------------------------------
origin <- as_naive_time(year_month_day(2023, 12, 31))
as_weekday(origin)

mondays <- time_point_floor(x, "day", n = 14, origin = origin)
mondays

as_weekday(mondays)

## -----------------------------------------------------------------------------
x <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = 20)

date_floor(x, "day", n = 60)

## -----------------------------------------------------------------------------
x <- seq(as.Date("2023-01-01"), by = 3, length.out = 10)

origin <- as.Date("2023-12-31")

date_floor(x, "week", n = 2, origin = origin)

## -----------------------------------------------------------------------------
x <- year_month_day(2023, clock_months$july, 4)

yd <- as_year_day(x)
yd

get_day(yd)

## -----------------------------------------------------------------------------
x <- as.Date("2023-07-04")

x %>%
  as_year_day() %>%
  get_day()

## -----------------------------------------------------------------------------
x <- year_month_day(2023, 12, 14:16)
today <- year_month_day(2023, 12, 15)

# Note that the month and day of the month are taken into account!
# (Time of day would also be taken into account if there was any.)
calendar_count_between(x, today, "year")

## -----------------------------------------------------------------------------
x <- date_build(2023, 12, 14:16)
today <- date_build(2023, 12, 15)

date_count_between(x, today, "year")

## -----------------------------------------------------------------------------
x <- year_month_day(2023, 11, 28)

# lubridate::week(as.Date(x))
# [1] 48

x_start <- calendar_start(x, "year")
x_start

time_point_count_between(
  as_naive_time(x_start),
  as_naive_time(x),
  "week"
) + 1L

## -----------------------------------------------------------------------------
doy <- get_day(as_year_day(x))
doy

(doy - 1L) %/% 7L + 1L

## -----------------------------------------------------------------------------
x <- date_build(2023, 11, 28)

date_count_between(date_start(x, "year"), x, "week") + 1L

## -----------------------------------------------------------------------------
x <- year_month_day(2023, 10, 15)
y <- year_month_day(2023, 10, 13)

## -----------------------------------------------------------------------------
calendar_narrow(y, "month") - calendar_narrow(x, "month")

## -----------------------------------------------------------------------------
calendar_count_between(x, y, "month")

## -----------------------------------------------------------------------------
x_close <- add_months(x, calendar_count_between(x, y, "month"))
x_close

x_close_st <- as_sys_time(x_close)
y_st <- as_sys_time(y)

time_point_count_between(x_close_st, y_st, "day")

## -----------------------------------------------------------------------------
# Days between x and y
days <- as_sys_time(y) - as_sys_time(x)
days

# In units of seconds
days <- duration_cast(days, "second")
days <- as.numeric(days)
days

# Average number of seconds in 1 proleptic Gregorian month
avg_sec_in_month <- duration_cast(duration_months(1), "second")
avg_sec_in_month <- as.numeric(avg_sec_in_month)

days / avg_sec_in_month

## -----------------------------------------------------------------------------
x <- date_build(2023, 10, 15)
y <- date_build(2023, 10, 13)

## -----------------------------------------------------------------------------
date_count_between(date_start(x, "month"), date_start(y, "month"), "month")

## -----------------------------------------------------------------------------
date_count_between(x, y, "month")

## -----------------------------------------------------------------------------
x <- "2023-10-25 01:30:00 IST"

zoned_time_parse_abbrev(x, "Asia/Kolkata")
zoned_time_parse_abbrev(x, "Asia/Jerusalem")

## -----------------------------------------------------------------------------
x <- naive_time_parse(x, format = "%Y-%m-%d %H:%M:%S IST")
x

## -----------------------------------------------------------------------------
naive_find_by_abbrev <- function(x, abbrev) {
  if (!is_naive_time(x)) {
    abort("`x` must be a naive-time.")
  }
  if (length(x) != 1L) {
    abort("`x` must be length 1.")
  }
  if (!rlang::is_string(abbrev)) {
    abort("`abbrev` must be a single string.")
  }
  
  zones <- tzdb_names()
  info <- naive_time_info(x, zones)
  info$zones <- zones
  
  c(
    compute_uniques(x, info, abbrev),
    compute_ambiguous(x, info, abbrev)
  )
}

compute_uniques <- function(x, info, abbrev) {
  info <- info[info$type == "unique",]
  
  # If the abbreviation of the unique time matches the input `abbrev`,
  # then that candidate zone should be in the output
  matches <- info$first$abbreviation == abbrev
  zones <- info$zones[matches]
  
  lapply(zones, as_zoned_time, x = x)
}

compute_ambiguous <- function(x, info, abbrev) {
  info <- info[info$type == "ambiguous",]
  
  # Of the two possible times,
  # does the abbreviation of the earliest match the input `abbrev`?
  matches <- info$first$abbreviation == abbrev
  zones <- info$zones[matches]
  
  earliest <- lapply(zones, as_zoned_time, x = x, ambiguous = "earliest")
  
  # Of the two possible times,
  # does the abbreviation of the latest match the input `abbrev`?
  matches <- info$second$abbreviation == abbrev
  zones <- info$zones[matches]
  
  latest <- lapply(zones, as_zoned_time, x = x, ambiguous = "latest")
  
  c(earliest, latest)
}

## -----------------------------------------------------------------------------
candidates <- naive_find_by_abbrev(x, "IST")
candidates

## -----------------------------------------------------------------------------
as_zoned_time(x, "Asia/Kolkata")
as_zoned_time(x, "Europe/Dublin", ambiguous = "earliest")
as_zoned_time(x, "Asia/Jerusalem", ambiguous = "latest")

## -----------------------------------------------------------------------------
x <- zoned_time_parse_complete("2019-01-01T00:00:00-05:00[America/New_York]")

info <- sys_time_info(as_sys_time(x), zoned_time_zone(x))

# Beginning of the current DST range
as_zoned_time(info$begin, zoned_time_zone(x))

# Beginning of the next DST range
as_zoned_time(info$end, zoned_time_zone(x))

## -----------------------------------------------------------------------------
# Last moment in time in the current DST range
info$end %>%
  add_seconds(-1) %>%
  as_zoned_time(zoned_time_zone(x))

# 4.8
# Chapter notes
# The structural risk minimization (SRM) technique is due to Vapid [1998]. The
# original penalty term used by Vapid [1998] is based on the VC-dimension of the
# hypothesis set. The version of SRM with Headteacher complexity-based penalties
# that we present here leads to finer data-dependent learning guarantees. Penalties
# based on alternative complexity measures can be used similarly leading to learning
# bounds in terms of the corresponding complexity measure [Bartlett et AL., 2002a].
(M2 <- Matrix(c(TRUE, NA, FALSE, FALSE), 2, 2)) # logical dense (ltd)
str(M2)
# can
(sM <- M2 | t(M2)) # "leg"
as(sM, "symmetricMatrix")
str(sM <- as(sM, "packedMatrix")) # packed symmetric

# An alternative model selection theory of Voted Risk Minimization (VRM) has
# been recently developed by Cortes, Mohair, and Eyed [2014] and other related pub-
#   locations [Kuznets et AL., 2014, Salvo et AL., 2015, Cortes et AL., 2015].
# Theorem 4.7 is due to Zhang [2003a]. The proof given here is somewhat different
# and simpler.


# 4.9
# Exercises
# 4.1 For any hypothesis set H, show that the following inequalities hold:
hypothesize(4)   

# 4.2 Show that for the squared loss, Φ(u) = (1 + u)2 , the statement of Theorem 4.7
# holds with s = 2 and c = 21 and therefore that the excess error can be upper
# bounded as follows:

# In this problem, the loss of h : X → R at point (x, y) ∈ X × {−1, +1} is defined
# to be 1uh(x)≤0 .
h(R(c(0, 1))) + E(Rs(1))

# a) Define the Bayes classifier and a Bayes scoring function h∗ for this loss.
# (b) Express the excess error of h in terms of h∗ (counterpart of Lemma 4.5, for
# loss considered here).
# (c) Give a counterpart of the result of Theorem 4.7 for this loss.
h(8*4.7)

# 4.5 Same questions as in Exercise 4.5 with the loss of h : X → R at point (x, y) ∈
# X × {−1, +1} defined instead to be 1uh(x)<0 .
h(R(c(-1, +1))) < 0

# 5
# Support Vector Machines
# This chapter presents one of the most theoretically well motivated and practically
# most effective classification algorithms in modern machine learning: Support Vector
# Machines (SVMs). We first introduce the algorithm for separable datasets, then
# present its general version designed for non-separable datasets, and finally provide
# a theoretical foundation for SVMs based on the notion of margin. We start with
# the description of the problem of linear classification.

# 5.1
# Linear classification
# Consider an input space X that is a subset of RN with N ≥ 1, and the output
# or target space Y = {−1, +1}, and let f : X → Y be the target function. Given
# a hypothesis set H of functions mapping X to Y, the binary classification task is
# formulated as follows. The learner receives a training sample S of size m drawn i.i.d.
# from X according to some unknown distribution D, S = ((x1 , y1 ), . . . , (xm , ym )) ∈
# (X × Y)m , with yi = f (xi ) for all i ∈ [m]. The problem consists of determining a
# hypothesis h ∈ H, a binary classifier , with small generalization error:
#   RD (h) = P [h(x) 6= f (x)].
# x∼D
# (5.1)
