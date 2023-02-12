Highr
================

<!--
%\VignetteEngine{knitr::docco_classic}
%\VignetteIndexEntry{Internals of the highr package}
-->

# Internals of the `highr` package

The **highr** package is based on the function `getParseData()`, which
was introduced in R 3.0.0. This function gives detailed information of
the symbols in a code fragment. A simple example:

``` r
p = parse(text = "   xx = 1 + 1  # a comment", keep.source = TRUE)
(d = getParseData(p))
```

    ##    line1 col1 line2 col2 id parent                  token terminal        text
    ## 14     1    4     1   13 14      0 expr_or_assign_or_help    FALSE            
    ## 1      1    4     1    5  1      3                 SYMBOL     TRUE          xx
    ## 3      1    4     1    5  3     14                   expr    FALSE            
    ## 2      1    7     1    7  2     14              EQ_ASSIGN     TRUE           =
    ## 12     1    9     1   13 12     14                   expr    FALSE            
    ## 5      1    9     1    9  5      6              NUM_CONST     TRUE           1
    ## 6      1    9     1    9  6     12                   expr    FALSE            
    ## 7      1   11     1   11  7     12                    '+'     TRUE           +
    ## 8      1   13     1   13  8      9              NUM_CONST     TRUE           1
    ## 9      1   13     1   13  9     12                   expr    FALSE            
    ## 10     1   16     1   26 10    -14                COMMENT     TRUE # a comment

The first step is to filter out the rows that we do not need:

``` r
(d = d[d$terminal, ])
```

    ##    line1 col1 line2 col2 id parent     token terminal        text
    ## 1      1    4     1    5  1      3    SYMBOL     TRUE          xx
    ## 2      1    7     1    7  2     14 EQ_ASSIGN     TRUE           =
    ## 5      1    9     1    9  5      6 NUM_CONST     TRUE           1
    ## 7      1   11     1   11  7     12       '+'     TRUE           +
    ## 8      1   13     1   13  8      9 NUM_CONST     TRUE           1
    ## 10     1   16     1   26 10    -14   COMMENT     TRUE # a comment

There is a column `token` in the data frame, and we will wrap this
column with markup commands, e.g. `\hlnum{1}` for the numeric constant
`1`. We defined the markup commands in `cmd_latex` and `cmd_html`:

``` r
head(highr:::cmd_latex)
```

    ##              cmd1 cmd2
    ## COMMENT  \\hlcom{    }
    ## FUNCTION \\hlkwa{    }
    ## IF       \\hlkwa{    }
    ## ELSE     \\hlkwa{    }
    ## WHILE    \\hlkwa{    }
    ## FOR      \\hlkwa{    }

``` r
tail(highr:::cmd_html)
```

    ##                             cmd1    cmd2
    ## OR         <span class="hl opt"> </span>
    ## OR2        <span class="hl opt"> </span>
    ## NS_GET     <span class="hl opt"> </span>
    ## NS_GET_INT <span class="hl opt"> </span>
    ## STANDARD   <span class="hl std"> </span>
    ## STR_CONST  <span class="hl str"> </span>

These command data frames are connected to the tokens in the R code via
their row names:

``` r
d$token
```

    ## [1] "SYMBOL"    "EQ_ASSIGN" "NUM_CONST" "'+'"       "NUM_CONST" "COMMENT"

``` r
rownames(highr:::cmd_latex)
```

    ##  [1] "COMMENT"              "FUNCTION"             "IF"                  
    ##  [4] "ELSE"                 "WHILE"                "FOR"                 
    ##  [7] "IN"                   "BREAK"                "REPEAT"              
    ## [10] "NEXT"                 "NULL_CONST"           "LEFT_ASSIGN"         
    ## [13] "EQ_ASSIGN"            "RIGHT_ASSIGN"         "SYMBOL_FORMALS"      
    ## [16] "SYMBOL_SUB"           "SLOT"                 "SYMBOL_FUNCTION_CALL"
    ## [19] "NUM_CONST"            "'+'"                  "'-'"                 
    ## [22] "'*'"                  "'/'"                  "'^'"                 
    ## [25] "'$'"                  "'@'"                  "':'"                 
    ## [28] "'?'"                  "'~'"                  "'!'"                 
    ## [31] "SPECIAL"              "GT"                   "GE"                  
    ## [34] "LT"                   "LE"                   "EQ"                  
    ## [37] "NE"                   "AND"                  "AND2"                
    ## [40] "OR"                   "OR2"                  "NS_GET"              
    ## [43] "NS_GET_INT"           "STANDARD"             "STR_CONST"

Now we know how to wrap up the R tokens. The next big question is how to
restore the white spaces in the source code, since they were not
directly available in the parsed data, but the parsed data contains
column numbers, and we can derive the positions of white spaces from
them. For example, `col2 = 5` for the first row, and `col1 = 7` for the
next row, and that indicates there must be one space after the token in
the first row, otherwise the next row will start at the position `6`
instead of `7`.

A small trick is used to fill in the gaps of white spaces:

``` r
(z = d[, c('col1', 'col2')])  # take out the column positions
```

    ##    col1 col2
    ## 1     4    5
    ## 2     7    7
    ## 5     9    9
    ## 7    11   11
    ## 8    13   13
    ## 10   16   26

``` r
(z = t(z)) # transpose the matrix
```

    ##      1 2 5  7  8 10
    ## col1 4 7 9 11 13 16
    ## col2 5 7 9 11 13 26

``` r
(z = c(z)) # turn it into a vector
```

    ##  [1]  4  5  7  7  9  9 11 11 13 13 16 26

``` r
(z = c(0, head(z, -1))) # append 0 in the beginning, and remove the last element
```

    ##  [1]  0  4  5  7  7  9  9 11 11 13 13 16

``` r
(z = matrix(z, ncol = 2, byrow = TRUE))
```

    ##      [,1] [,2]
    ## [1,]    0    4
    ## [2,]    5    7
    ## [3,]    7    9
    ## [4,]    9   11
    ## [5,]   11   13
    ## [6,]   13   16

Now the two columns indicate the starting and ending positions of
spaces, and we can easily figure out how many white spaces are needed
for each row:

``` r
(s = z[, 2] - z[, 1] - 1)
```

    ## [1] 3 1 1 1 1 2

``` r
(s = strrep(' ', s))
```

    ## [1] "   " " "   " "   " "   " "   "  "

``` r
paste(s, d$text, sep = '')
```

    ## [1] "   xx"         " ="            " 1"            " +"           
    ## [5] " 1"            "  # a comment"

So we have successfully restored the white spaces in the source code.
Let’s paste all pieces together (suppose we highlight for LaTeX):

``` r
m = highr:::cmd_latex[d$token, ]
cbind(d, m)
```

    ##    line1 col1 line2 col2 id parent     token terminal        text     cmd1 cmd2
    ## 1      1    4     1    5  1      3    SYMBOL     TRUE          xx     <NA> <NA>
    ## 2      1    7     1    7  2     14 EQ_ASSIGN     TRUE           = \\hlkwb{    }
    ## 5      1    9     1    9  5      6 NUM_CONST     TRUE           1 \\hlnum{    }
    ## 7      1   11     1   11  7     12       '+'     TRUE           + \\hlopt{    }
    ## 8      1   13     1   13  8      9 NUM_CONST     TRUE           1 \\hlnum{    }
    ## 10     1   16     1   26 10    -14   COMMENT     TRUE # a comment \\hlcom{    }

``` r
# use standard markup if tokens do not exist in the table
m[is.na(m[, 1]), ] = highr:::cmd_latex['STANDARD', ]
paste(s, m[, 1], d$text, m[, 2], sep = '', collapse = '')
```

    ## [1] "   \\hlstd{xx} \\hlkwb{=} \\hlnum{1} \\hlopt{+} \\hlnum{1}  \\hlcom{# a comment}"

So far so simple. That is one line of code, after all. A next challenge
comes when there are multiple lines, and a token spans across multiple
lines:

``` r
d = getParseData(parse(text = "x = \"a character\nstring\" #hi", keep.source = TRUE))
(d = d[d$terminal, ])
```

    ##   line1 col1 line2 col2 id parent     token terminal                  text
    ## 1     1    1     1    1  1      3    SYMBOL     TRUE                     x
    ## 2     1    3     1    3  2     10 EQ_ASSIGN     TRUE                     =
    ## 5     1    5     2    7  5      8 STR_CONST     TRUE "a character\nstring"
    ## 6     2    9     2   11  6    -10   COMMENT     TRUE                   #hi

Take a look at the third row. It says that the character string starts
from line 1, and ends on line 2. In this case, we just pretend as if
everything on line 1 were on line 2. Then for each line, we append the
missing spaces and apply markup commands to text symbols.

``` r
d$line1[d$line1 == 1] = 2
d
```

    ##   line1 col1 line2 col2 id parent     token terminal                  text
    ## 1     2    1     1    1  1      3    SYMBOL     TRUE                     x
    ## 2     2    3     1    3  2     10 EQ_ASSIGN     TRUE                     =
    ## 5     2    5     2    7  5      8 STR_CONST     TRUE "a character\nstring"
    ## 6     2    9     2   11  6    -10   COMMENT     TRUE                   #hi

Do not worry about the column `line2`. It does not matter. Only `line1`
is needed to indicate the line number here.

Why do we need to highlight line by line instead of applying
highlighting commands to all text symbols (a.k.a vectorization)? Well,
the margin of this paper is too small to write down the answer.
