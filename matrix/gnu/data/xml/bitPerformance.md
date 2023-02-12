Bit Performance
================
Dr. Jens Oehlschlägel
2023-02-02

------------------------------------------------------------------------

## A performance example

Before we measure performance of the main functionality of the package,
note that something simple as ‘(a:b)\[-i\]’ can and has been accelerated
in this package:

``` r
a <- 1L
b <- 1e7L
i <- sample(a:b,1e3)
x <- c(
  R = median(microbenchmark((a:b)[-i], times=times)$time)
, bit = median(microbenchmark(bit_rangediff(c(a,b), i), times=times)$time)
, merge = median(microbenchmark(merge_rangediff(c(a,b), bit_sort(i)), times=times)$time)
)
knitr::kable(as.data.frame(as.list(x/x["R"]*100)), caption="% of time relative to R", digits=1)
```

|   R | bit | merge |
|----:|----:|------:|
| 100 | 6.6 |   6.7 |

% of time relative to R

The vignette is compiled with the following performance settings: 5
replications with domain size small 1000 and big 10^{6}, sample size
small 1000 and big 10^{6}.

## Boolean data types

> “A designer knows he has achieved perfection not when there is nothing
> left to add, but when there is nothing left to take away.”  
> “Il semble que la perfection soit atteinte non quand il n’y a plus
> rien à ajouter, mais quand il n’y a plus rien à retrancher”  
> (Antoine de St. Exupery, Terre des Hommes (Gallimard, 1939), p. 60.)

We compare memory consumption (n=1e+06) and runtime (median of 5
replications) of the different `booltype`s for the following filter
scenarios:

| coin       | often      | rare      | chunk                  |
|:-----------|:-----------|:----------|:-----------------------|
| random 50% | random 99% | random 1% | contiguous chunk of 5% |

selection characteristic

There are substantial savings in skewed filter situations:

![% size and execution time for bit (b) and bitwhich (w) relative to
logical (R) in the ‘rare’
scenario](bitPerformance_files/figure-gfm/unnamed-chunk-5-1.png)

![% size and execution time for bit (b) and bitwhich (w) relative to
logical (R) in the ‘often’
scenario](bitPerformance_files/figure-gfm/unnamed-chunk-6-1.png)

Even in non-skewed situations the new booltypes are competitive:

![% size and execution time for bit (b) and bitwhich (w) relative to
logical (R) in the ‘coin’
scenario](bitPerformance_files/figure-gfm/unnamed-chunk-7-1.png)

Detailed tables follow.

<div style="page-break-before: always;" />


### % memory consumption of filter

|          |  coin | often |  rare | chunk |
|:---------|------:|------:|------:|------:|
| logical  | 100.0 | 100.0 | 100.0 | 100.0 |
| bit      |   3.2 |   3.2 |   3.2 |   3.2 |
| bitwhich |  50.0 |   1.0 |   1.0 |   5.0 |
| which    |  50.0 |  99.0 |   1.0 |   5.0 |
| ri       |    NA |    NA |    NA |   0.0 |

% bytes of logical

### % time extracting

|          | coin | often | rare | chunk |
|:---------|-----:|------:|-----:|------:|
| logical  | 39.1 |  10.6 | 10.3 |    NA |
| bit      | 50.9 |  23.8 | 24.9 |    NA |
| bitwhich | 46.6 | 117.4 |  7.7 |    NA |
| which    |   NA |    NA |   NA |    NA |
| ri       |   NA |    NA |   NA |    NA |

% time of logical

### % time assigning

|          |  coin | often | rare | chunk |
|:---------|------:|------:|-----:|------:|
| logical  |  86.2 |  61.9 | 62.7 |    NA |
| bit      |  90.2 |  23.4 | 14.9 |    NA |
| bitwhich | 201.3 |  59.0 | 56.9 |    NA |
| which    |    NA |    NA |   NA |    NA |
| ri       |    NA |    NA |   NA |    NA |

% time of logical

### % time subscripting with ‘which’

|          | coin | often | rare | chunk |
|:---------|-----:|------:|-----:|------:|
| logical  | 40.1 |  53.3 |  1.0 |    NA |
| bit      | 60.5 |  59.5 |  0.8 |    NA |
| bitwhich | 83.7 | 127.5 |  1.4 |    NA |
| which    |   NA |    NA |   NA |    NA |
| ri       |   NA |    NA |   NA |    NA |

% time of logical

### % time assigning with ‘which’

|          | coin | often | rare | chunk |
|:---------|-----:|------:|-----:|------:|
| logical  | 26.6 |  49.9 |  1.0 |    NA |
| bit      | 25.4 |  45.2 |  0.7 |    NA |
| bitwhich | 85.0 |  32.0 |  3.5 |    NA |
| which    |   NA |    NA |   NA |    NA |
| ri       |   NA |    NA |   NA |    NA |

% time of logical

### % time Boolean NOT

|          | coin | often | rare | chunk |
|:---------|-----:|------:|-----:|------:|
| logical  | 26.9 |  26.9 | 28.0 |  28.2 |
| bit      |  1.2 |   1.0 |  0.8 |   0.8 |
| bitwhich | 21.2 |   0.8 |  1.0 |   1.7 |
| which    |   NA |    NA |   NA |    NA |
| ri       |   NA |    NA |   NA |    NA |

% time for Boolean NOT

### % time Boolean AND

|          | coin | often | rare | chunk |
|:---------|-----:|------:|-----:|------:|
| logical  | 92.1 |    28 | 23.3 |  22.3 |
| bit      |  4.9 |     4 |  5.5 |   4.3 |
| bitwhich | 28.3 |     5 |  4.6 |   6.1 |
| which    |   NA |    NA |   NA |    NA |
| ri       |   NA |    NA |   NA |    NA |

% time for Boolean &

### % time Boolean OR

|          |  coin | often | rare | chunk |
|:---------|------:|------:|-----:|------:|
| logical  | 100.0 |  24.7 | 35.4 |  40.7 |
| bit      |   6.1 |   4.6 |  4.0 |   4.1 |
| bitwhich |  39.2 |   4.5 |  4.8 |   6.5 |
| which    |    NA |    NA |   NA |    NA |
| ri       |    NA |    NA |   NA |    NA |

% time for Boolean \|

### % time Boolean EQUALITY

|          | coin | often | rare | chunk |
|:---------|-----:|------:|-----:|------:|
| logical  | 26.7 |  28.3 | 24.7 |  25.6 |
| bit      |  4.0 |   9.5 |  5.7 |   4.0 |
| bitwhich | 26.4 |   4.5 |  4.4 |   6.5 |
| which    |   NA |    NA |   NA |    NA |
| ri       |   NA |    NA |   NA |    NA |

% time for Boolean ==

### % time Boolean XOR

|          | coin | often | rare | chunk |
|:---------|-----:|------:|-----:|------:|
| logical  | 35.3 |  33.7 | 34.5 |  33.8 |
| bit      |  4.1 |   3.9 |  4.3 |   6.2 |
| bitwhich | 20.9 |   4.2 |  4.3 |   5.6 |
| which    |   NA |    NA |   NA |    NA |
| ri       |   NA |    NA |   NA |    NA |

% time for Boolean !=

### % time Boolean SUMMARY

|         | coin | often |
|:--------|-----:|------:|
| logical | 99.4 |  32.8 |
| bit     | 11.9 |  10.4 |

% time for Boolean summary

------------------------------------------------------------------------

## Fast methods for `integer` set operations

> “The space-efficient structure of bitmaps dramatically reduced the run
> time of sorting”  
> (Jon Bently, Programming Pearls, Cracking the oyster, p. 7)

![Execution time for R (R) and bit
(b)](bitPerformance_files/figure-gfm/unnamed-chunk-20-1.png)

<div style="page-break-before: always;" />


![Execution time for R, bit and merge relative to most expensive R in
‘unsorted bigbig’
scenario](bitPerformance_files/figure-gfm/unnamed-chunk-22-1.png)

![Execution time for R, bit and merge in ‘sorted bigbig’
scenario](bitPerformance_files/figure-gfm/unnamed-chunk-23-1.png)

<div style="page-break-before: always;" />


### % time for sorting

|            | small |   big |
|:-----------|------:|------:|
| sort       | 121.1 | 188.1 |
| sortunique |  72.0 |  19.1 |

sorted data relative to R’s sort

|            | small |  big |
|:-----------|------:|-----:|
| sort       |  24.4 | 36.6 |
| sortunique |  17.5 |  7.1 |

unsorted data relative to R’s sort

### % time for unique

|       | small |  big |
|:------|------:|-----:|
| bit   | 121.5 | 10.7 |
| merge |  32.8 | 10.2 |
| sort  |   0.0 |  0.0 |

sorted data relative to R

|       | small |  big |
|:------|------:|-----:|
| bit   | 246.0 | 11.5 |
| merge | 157.7 | 41.2 |
| sort  | 122.7 | 35.7 |

unsorted data relative to R

### % time for duplicated

|       | small |  big |
|:------|------:|-----:|
| bit   | 236.9 | 11.7 |
| merge |  26.6 |  5.6 |
| sort  |   0.0 |  0.0 |

sorted data relative to R

|       | small |  big |
|:------|------:|-----:|
| bit   | 127.3 |  6.2 |
| merge |  91.6 | 42.1 |
| sort  |  77.0 | 39.4 |

unsorted data relative to R

### % time for anyDuplicated

|       | small |  big |
|:------|------:|-----:|
| bit   |  82.2 | 14.3 |
| merge |   8.4 |  3.4 |
| sort  |   0.0 |  0.0 |

sorted data relative to R

|       | small |  big |
|:------|------:|-----:|
| bit   | 155.4 |  6.6 |
| merge | 164.3 | 41.8 |
| sort  | 148.7 | 40.4 |

unsorted data relative to R

### % time for sumDuplicated

|       | small |  big |
|:------|------:|-----:|
| bit   |  58.9 | 11.1 |
| merge |  10.5 |  6.3 |
| sort  |   0.0 |  0.0 |

sorted data relative to R

|       | small |  big |
|:------|------:|-----:|
| bit   | 166.2 |  5.8 |
| merge | 123.4 | 41.4 |
| sort  | 104.6 | 38.3 |

unsorted data relative to R

### % time for match

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |         NA |       NA |       NA |     NA |
| merge |       34.3 |        0 |     14.7 |    5.8 |
| sort  |        0.0 |        0 |      0.0 |    0.0 |

sorted data relative to R

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |         NA |       NA |       NA |     NA |
| merge |      250.0 |     38.4 |     91.7 |   38.6 |
| sort  |      215.2 |     38.4 |     82.7 |   36.3 |

unsorted data relative to R

### % time for in

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |      263.4 |      3.6 |      7.9 |   10.8 |
| merge |       21.8 |      0.0 |     11.5 |    5.2 |
| sort  |        0.0 |      0.0 |      0.0 |    0.0 |

sorted data relative to R

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |      254.9 |      1.7 |      5.8 |    4.8 |
| merge |      222.6 |     36.9 |     78.3 |   38.3 |
| sort  |      193.3 |     36.9 |     71.1 |   36.1 |

unsorted data relative to R

### % time for notin

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |      251.3 |      3.3 |      7.8 |   10.4 |
| merge |       22.1 |      0.0 |     11.3 |    3.8 |
| sort  |        0.0 |      0.0 |      0.0 |    0.0 |

sorted data relative to R

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |      148.4 |      1.7 |      5.2 |    4.6 |
| merge |      100.4 |     38.4 |     71.8 |   36.3 |
| sort  |       88.8 |     38.4 |     64.3 |   34.7 |

unsorted data relative to R

### % time for union

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |       71.5 |     15.6 |     13.5 |   10.7 |
| merge |       44.2 |      8.9 |      7.6 |    4.4 |
| sort  |        0.0 |      0.0 |      0.0 |    0.0 |

sorted data relative to R

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |       68.7 |      6.4 |     11.0 |    4.5 |
| merge |      133.7 |     38.3 |     38.4 |   31.0 |
| sort  |       90.7 |     33.4 |     33.7 |   28.6 |

unsorted data relative to R

### % time for intersect

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |       62.2 |      6.5 |      8.7 |    6.6 |
| merge |       26.2 |      0.0 |      0.0 |    6.6 |
| sort  |        0.0 |      0.0 |      0.0 |    0.0 |

sorted data relative to R

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |       64.6 |      2.9 |      5.9 |    5.9 |
| merge |      119.4 |     39.8 |     69.5 |   25.3 |
| sort  |       92.3 |     39.8 |     69.5 |   22.5 |

unsorted data relative to R

### % time for setdiff

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |      146.0 |      3.5 |     10.3 |   10.6 |
| merge |       34.1 |      0.0 |      1.8 |    8.7 |
| sort  |        0.0 |      0.0 |      0.0 |    0.0 |

sorted data relative to R

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |       77.8 |      2.2 |      6.9 |    4.1 |
| merge |      166.3 |     39.3 |     23.4 |   38.8 |
| sort  |      132.6 |     39.3 |     22.3 |   35.1 |

unsorted data relative to R

### % time for symdiff

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |       60.1 |      7.9 |      5.8 |    9.4 |
| merge |       12.7 |      3.2 |      2.1 |    3.9 |
| sort  |        0.0 |      0.0 |      0.0 |    0.0 |

sorted data relative to R

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |       59.0 |      3.9 |      3.9 |    4.3 |
| merge |       66.8 |     12.6 |     12.4 |   19.8 |
| sort  |       54.5 |     11.0 |     11.0 |   18.1 |

unsorted data relative to R

### % time for setequal

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |       84.6 |     51.7 |      6.0 |    5.9 |
| merge |       27.2 |     14.2 |      4.9 |    4.5 |
| sort  |        0.0 |      0.0 |      0.0 |    0.0 |

sorted data relative to R

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |       93.8 |     34.3 |      3.0 |    2.9 |
| merge |      120.9 |  30653.0 |     13.4 |   24.5 |
| sort  |       93.8 |  30643.7 |     11.0 |   22.2 |

unsorted data relative to R

### % time for setearly

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |       82.9 |      1.9 |      5.3 |    5.9 |
| merge |       27.8 |      0.0 |      0.0 |    4.4 |
| sort  |        0.0 |      0.0 |      0.0 |    0.0 |

sorted data relative to R

|       | smallsmall | smallbig | bigsmall | bigbig |
|:------|-----------:|---------:|---------:|-------:|
| bit   |      136.3 |      0.9 |      2.8 |    2.5 |
| merge |       72.8 |     26.4 |     80.0 |   19.3 |
| sort  |       56.1 |     26.4 |     79.9 |   17.6 |

unsorted data relative to R
