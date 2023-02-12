Diff Obj
================

## Introduction

`diffobj` uses the same comparison mechanism used by `git diff` and
`diff` to highlight differences between *rendered* R objects:

``` r
a <- b <- matrix(1:100, ncol=2)
a <- a[-20,]
b <- b[-45,]
b[c(18, 44)] <- 999
diffPrint(target=a, current=b)
```

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>a</span></div></div></div></div><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>b</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 17,6 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 17,7 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-guide'><div class='diffobj-gutter'><div class='diffobj-guide'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-guide'><span class='diffobj-trim'></span>      [,1] [,2]<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-guide'><div class='diffobj-gutter'><div class='diffobj-guide'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-guide'><span class='diffobj-trim'></span>      [,1] [,2]<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[16,] </span>  16   66<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[16,] </span>  16   66<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[17,] </span>  17   67<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[17,] </span>  17   67<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>[18,] </span>  <span class='diffobj-word delete'>18</span>   68<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[18,] </span> <span class='diffobj-word insert'>999</span>   68<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[19,] </span>  19   69<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[19,] </span>  19   69<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-fill'><div class='diffobj-gutter'><div class='diffobj-fill'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-fill'></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[20,] </span>  <span class='diffobj-word insert'>20</span>   <span class='diffobj-word insert'>70</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[20,] </span>  21   71<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[21,] </span>  21   71<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[21,] </span>  22   72<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[22,] </span>  22   72<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 42,6 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 43,5 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[41,] </span>  42   92<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[42,] </span>  42   92<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[42,] </span>  43   93<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[43,] </span>  43   93<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>[43,] </span>  <span class='diffobj-word delete'>44</span>   94<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[44,] </span> <span class='diffobj-word insert'>999</span>   94<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>[44,] </span>  <span class='diffobj-word delete'>45</span>   <span class='diffobj-word delete'>95</span><span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-fill'><div class='diffobj-gutter'><div class='diffobj-fill'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-fill'></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[45,] </span>  46   96<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[45,] </span>  46   96<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[46,] </span>  47   97<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[46,] </span>  47   97<span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

`diffobj` comparisons work best when objects have some similarities, or
when they are relatively small. The package was originally developed to
help diagnose failed unit tests by comparing test results to reference
objects in a human-friendly manner.

If your terminal supports formatting through ANSI escape sequences,
`diffobj` will output colored diffs to the terminal. If not, it will
output colored diffs to your IDE viewport if it is supported, or to your
browser otherwise.

## Interpreting Diffs

### Shortest Edit Script

The output from `diffobj` is a visual representation of the Shortest
Edit Script (SES). An SES is the shortest set of deletion and insertion
instructions for converting one sequence of elements into another. In
our case, the elements are lines of text. We encode the instructions to
convert `a` to `b` by deleting lines from `a` (in yellow) and inserting
new ones from `b` (in blue).

### Diff Structure

The first line of our diff output acts as a legend to the diff by
associating the colors and symbols used to represent differences present
in each object with the name of the object:

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>a</span></div></div></div></div><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>b</span></div></div></div></div></div></pre>

</div>

After the legend come the hunks, which are portions of the objects that
have differences with nearby matching lines provided for context:

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 17,6 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 17,7 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-guide'><div class='diffobj-gutter'><div class='diffobj-guide'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-guide'><span class='diffobj-trim'></span>      [,1] [,2]<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-guide'><div class='diffobj-gutter'><div class='diffobj-guide'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-guide'><span class='diffobj-trim'></span>      [,1] [,2]<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[16,] </span>  16   66<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[16,] </span>  16   66<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[17,] </span>  17   67<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[17,] </span>  17   67<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>[18,] </span>  <span class='diffobj-word delete'>18</span>   68<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[18,] </span> <span class='diffobj-word insert'>999</span>   68<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[19,] </span>  19   69<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[19,] </span>  19   69<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-fill'><div class='diffobj-gutter'><div class='diffobj-fill'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-fill'></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[20,] </span>  <span class='diffobj-word insert'>20</span>   <span class='diffobj-word insert'>70</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[20,] </span>  21   71<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[21,] </span>  21   71<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[21,] </span>  22   72<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[22,] </span>  22   72<span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

At the top of the hunk is the hunk header: this tells us that the first
displayed hunk (including context lines), starts at line 17 and spans 6
lines for `a` and 7 for `b`. These are display lines, not object row
indices, which is why the first row shown of the matrix is row 16. You
might have also noticed that the line after the hunk header is out of
place:

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-guide'><div class='diffobj-gutter'><div class='diffobj-guide'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-guide'><span class='diffobj-trim'></span>      [,1] [,2]<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-guide'><div class='diffobj-gutter'><div class='diffobj-guide'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-guide'><span class='diffobj-trim'></span>      [,1] [,2]<span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

This is a special context line that is not technically part of the hunk,
but is shown nonetheless because it is useful in helping understand the
data. The line is styled differently to highlight that it is not part of
the hunk. Since it is not part of the hunk, it is not accounted for in
the hunk header. See `?guideLines` for more details.

The actual mismatched lines are highlighted in the colors of the legend,
with additional visual cues in the gutters:

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>[18,] </span>  <span class='diffobj-word delete'>18</span>   68<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[18,] </span> <span class='diffobj-word insert'>999</span>   68<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[19,] </span>  19   69<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[19,] </span>  19   69<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-fill'><div class='diffobj-gutter'><div class='diffobj-fill'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-fill'></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[20,] </span>  <span class='diffobj-word insert'>20</span>   <span class='diffobj-word insert'>70</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[20,] </span>  21   71<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[21,] </span>  21   71<span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

`diffobj` uses a line by line diff to identify which portions of each of
the objects are mismatches, so even if only part of a line mismatches it
will be considered different. `diffobj` then runs a word diff within the
hunks and further highlights mismatching words.

Let’s examine the last two lines from the previous hunk more closely:

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-fill'><div class='diffobj-gutter'><div class='diffobj-fill'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-fill'></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[20,] </span>  <span class='diffobj-word insert'>20</span>   <span class='diffobj-word insert'>70</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[20,] </span>  21   71<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[21,] </span>  21   71<span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

Here `b` has an extra line so `diffobj` adds an empty line to `a` to
maintain the alignment for subsequent matching lines. This additional
line is marked with a tilde in the gutter and is shown in a different
color to indicate it is not part of the original text.

If you look closely at the next matching line you will notice that the
`a` and `b` values are not exactly the same. The row indices are
different, but `diffobj` excludes row indices from the diff so that rows
that are identical otherwise are shown as matching. `diffobj` indicates
this is happening by showing the portions of a line that are ignored in
the diff in grey.

See `?guides` and `?trim` for details and limitations on guideline
detection and unsemantic meta data trimming.

### Atomic Vectors

Since R can display multiple elements in an atomic vector on the same
line, and `diffPrint` is fundamentally a line diff, we use specialized
logic when diffing atomic vectors. Consider:

``` r
state.abb2 <- state.abb[-16]
state.abb2[37] <- "Pennsylvania"
diffPrint(state.abb, state.abb2)
```

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>state.abb</span></div></div></div></div><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>state.abb2</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 1,5 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 6,5 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'> [1] </span>"AL" "AK" "AZ" "AR" "CA" "CO"<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[11] </span>"HI"           "ID"          <span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'> [7] </span>"CT" "DE" "FL" "GA" "HI" "ID"<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[13] </span>"IL"           "IN"          <span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>[13] </span>"IL" "IN" "IA" <span class='diffobj-word delete'>"KS"</span> "KY" "LA"<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[15] </span>"IA"           "KY"          <span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[19] </span>"ME" "MD" "MA" "MI" "MN" "MS"<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[17] </span>"LA"           "ME"          <span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[25] </span>"MO" "MT" "NE" "NV" "NH" "NJ"<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[19] </span>"MD"           "MA"          <span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 6,4 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 17,5 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-fill'><div class='diffobj-gutter'><div class='diffobj-fill'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-fill'>&nbsp;</div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[33] </span>"ND"           "OH"          <span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[31] </span>"NM" "NY" "NC" "ND" "OH" "OK"<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[35] </span>"OK"           "OR"          <span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>[37] </span>"OR" <span class='diffobj-word delete'>"PA"</span> "RI" "SC" "SD" "TN"<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[37] </span><span class='diffobj-word insert'>"Pennsylvania"</span> "RI"          <span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[43] </span>"TX" "UT" "VT" "VA" "WA" "WV"<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[39] </span>"SC"           "SD"          <span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[49] </span>"WI" "WY"<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>[41] </span>"TN"           "TX"          <span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

Due to the different wrapping frequency no line in the text display of
our two vectors matches. Despite this, `diffPrint` only highlights the
lines that actually contain differences. The side effect is that lines
that only contain matching elements are shown as matching even though
the actual lines may be different. You can turn off this behavior in
favor of a normal line diff with the `unwrap.atomic` argument to
`diffPrint`.

Currently this only works for *unnamed* vectors, and even for them some
inputs may produce sub-optimal results. Nested vectors inside lists will
not be unwrapped. You can also use `diffChr` (see below) to do a direct
element by element comparison.

## Other Diff Functions

### Method Overview

`diffobj` defines several S4 generics and default methods to go along
with them. Each of them uses a different text representation of the
inputs:

- `diffPrint`: use the `print`/`show` output and is the one used in the
  examples so far
- `diffStr`: use the output of `str`
- `diffObj`: picks between `print`/`show` and `str` depending on which
  provides the “best” overview of differences
- `diffChr`: coerces the inputs to atomic character vectors with
  `as.character`, and runs the diff on the character vector
- `diffFile`: compares the text content of two files
- `diffCsv`: loads two CSV files into data frames and compares the data
  frames with `diffPrint`
- `diffDeparse`: deparses and compares the character vectors produced by
  the deparsing
- `ses`: computes the element by element shortest edit script on two
  character vectors

Note the `diff*` functions use lowerCamelCase in keeping with S4 method
name convention, whereas the package name itself is all lower case.

### Compare Structure with `diffStr`

For complex objects it is often useful to compare structures:

``` r
mdl1 <- lm(Sepal.Length ~ Sepal.Width, iris)
mdl2 <- lm(Sepal.Length ~ Sepal.Width + Species, iris)
diffStr(mdl1$qr, mdl2$qr, line.limit=15)
```

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>str(mdl1$qr, max.level = 2L)</span></div></div></div></div><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>str(mdl2$qr, max.level = 2L)</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 1,9 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 1,10 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>List of 5<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>List of 5<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'> $ </span>qr   : num [1:150, <span class='diffobj-word delete'>1:2]</span> -12.2474 0.0816 0.0816 0.0816 0.0816 ...<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'> $ </span>qr   : num [1:150, <span class='diffobj-word insert'>1:4]</span> -12.2474 0.0816 0.0816 0.0816 0.0816 ...<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>  ..- </span>attr(*, "dimnames")=List of 2<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'>  ..- </span>attr(*, "dimnames")=List of 2<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>  ..- </span>attr(*, "assign")= int <span class='diffobj-word delete'>[1:2]</span> 0 1<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>  ..- </span>attr(*, "assign")= int <span class='diffobj-word insert'>[1:4]</span> 0 <span class='diffobj-word insert'>1</span> <span class='diffobj-word insert'>2</span> <span class='diffobj-word insert'>2</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-fill'><div class='diffobj-gutter'><div class='diffobj-fill'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-fill'></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>  ..- </span><span class='diffobj-word insert'>attr</span><span class='diffobj-word insert'>(*,</span> <span class='diffobj-word insert'>"contrasts"</span><span class='diffobj-word insert'>)=List</span> <span class='diffobj-word insert'>of</span> 1<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'> $ </span>qraux: num <span class='diffobj-word delete'>[1:2]</span> 1.08 1.02<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'> $ </span>qraux: num <span class='diffobj-word insert'>[1:4]</span> 1.08 1.02 <span class='diffobj-word insert'>1.05</span> <span class='diffobj-word insert'>1.11</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'> $ </span>pivot: int <span class='diffobj-word delete'>[1:2]</span> 1 2<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'> $ </span>pivot: int <span class='diffobj-word insert'>[1:4]</span> 1 2 <span class='diffobj-word insert'>3</span> <span class='diffobj-word insert'>4</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'> $ </span>tol  : num 1e-07<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'> $ </span>tol  : num 1e-07<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'> $ </span>rank : int <span class='diffobj-word delete'>2</span><span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'> $ </span>rank : int <span class='diffobj-word insert'>4</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'> - </span>attr(*, "class")= chr "qr"<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'> - </span>attr(*, "class")= chr "qr"<span class='diffobj-trim'></span></div></div></div></div></div>3 differences are hidden by our use of `max.level`</pre>

</div>

If you specify a `line.limit` with `diffStr` it will fold nested levels
in order to fit under `line.limit` so long as there remain visible
differences. If you prefer to see all the differences you can leave
`line.limit` unspecified.

### Compare Vectors Elements with `diffChr`

Sometimes it is useful to do a direct element by element comparison:

``` r
diffChr(letters[1:3], c("a", "B", "c"))
```

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>letters[1:3]</span></div></div></div></div><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>c("a", "B", "c")</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 1,3 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 1,3 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>a<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>a<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'></span><span class='diffobj-word delete'>b</span><span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'></span><span class='diffobj-word insert'>B</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>c<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>c<span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

Notice how we are comparing the contents of the vectors with one line
per element.

### Why S4?

The `diff*` functions are defined as S4 generics with default methods
(signature `c("ANY", "ANY")`) so that users can customize behavior for
their own objects. For example, a custom method could set many of the
default parameters to values more suitable for a particular object. If
the objects in question are S3 objects the S3 class will have to be
registered with `setOldClass`.

### Return Value

All the `diff*` methods return a `Diff` S4 object. It has a `show`
method which is responsible for rendering the `Diff` and displaying it
to the screen. Because of this you can compute and render diffs in two
steps:

``` r
x <- diffPrint(letters, LETTERS)
x   # or equivalently: `show(x)`
```

This may cause the diff to render funny if you change screen widths,
etc., between the two steps.

There are also `summary`, `any`, and `as.character` methods. The
`summary` method provides a high level overview of where the differences
are, which can be helpful for large diffs:

``` r
summary(diffStr(mdl1, mdl2))
```

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-summary'><div class='body'>Found differences in 12 hunks:<div class='detail'>45 insertions, 39 deletions, 18 matches (lines)</div><br />Diff map (line:char scale is 1:1 for single chars, 1-2:1 for char seqs):</div><div class='map'><span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span>.<span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span>.<span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span>.<span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span>..<span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span>.<span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span>.<span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span>.<span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span>..<span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span>..<span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span>.<span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span>..<span class='diffobj-word delete'>D</span><span class='diffobj-word delete'>D</span><span class='diffobj-word insert'>I</span><span class='diffobj-word insert'>I</span>.</div></div></pre>

</div>

`any` returns TRUE if there are differences, and `as.character` returns
the character representation of the diff.

## Controlling Diffs and Their Appearance

### Parameters

The `diff*` family of methods has an extensive set of parameters that
allow you to fine tune how the diff is applied and displayed. We will
review some of the major ones in this section. For a full description
see `?diffPrint`.

While the parameter list is extensive, only the objects being compared
are required. All the other parameters have default values, and most of
them are for advanced use only. The defaults can all be adjusted via the
`diffobj.*` options.

### Display Mode

There are three built-in display modes that are similar to those found
in GNU `diff`: “sidebyside”, “unified”, and “context”. For example, by
varying the `mode` parameter with:

``` r
x <- y <- letters[24:26]
y[2] <- "GREMLINS"
diffChr(x, y)
```

we get:

<table>
<tr>
<th>
mode=“sidebyside”
<th>
mode=“unified”
<th>
mode=“context”
<tr style="vertical-align: top">
<td>

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>x</span></div></div></div></div><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>y</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 1,3 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 1,3 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>x<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>x<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'></span><span class='diffobj-word delete'>y</span><span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'></span><span class='diffobj-word insert'>GREMLINS</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>z<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>z<span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

<td>

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>x</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>y</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 1,3 / 1,3 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>x<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'></span><span class='diffobj-word delete'>y</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'></span><span class='diffobj-word insert'>GREMLINS</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>z<span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

<td>

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>x</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>y</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 1,3 / 1,3 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>x<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'></span><span class='diffobj-word delete'>y</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>z<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-fill'><div class='diffobj-gutter'><div class='context_sep ctd'>~</div><div class='pad'> </div></div><div class='diffobj-text'><div class='context_sep'>----------</div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>x<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'></span><span class='diffobj-word insert'>GREMLINS</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>z<span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

</table>

By default `diffobj` will try to use `mode="sidebyside"` if reasonable
given display width, and otherwise will switch to `mode="unified"`. You
can always force a particular display style by specifying it with the
`mode` argument.

### Color Mode

The default color mode uses yellow and blue to symbolize deletions and
insertions for accessibility to dichromats. If you prefer the more
traditional color mode you can specify `color.mode="rgb"` in the
parameter list, or use `options(diffobj.color.mode="rgb")`:

``` r
diffChr(x, y, color.mode="rgb")
```

<div class="diffobj-container light rgb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>x</span></div></div></div></div><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>y</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 1,3 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 1,3 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>x<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>x<span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'></span><span class='diffobj-word delete'>y</span><span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'></span><span class='diffobj-word insert'>GREMLINS</span><span class='diffobj-trim'></span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>z<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='diffobj-match'><div class='diffobj-gutter'><div class='diffobj-match'>&nbsp;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='diffobj-match'><span class='diffobj-trim'></span>z<span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

### Output Formats

If your terminal supports it `diffobj` will format the output with ANSI
escape sequences. `diffobj` uses Gábor Csárdi’s
[`crayon`](https://github.com/r-lib/crayon) package to detect ANSI
support and to apply ANSI based formatting. If you are using RStudio or
another IDE that supports `getOption("viewer")`, `diffobj` will output
an HTML/CSS formatted diff to the viewport. In other terminals that do
not support ANSI colors, `diffobj` will attempt to output to an HTML/CSS
formatted diff to your browser using `browseURL`.

You can explicitly specify the output format with the `format`
parameter:

- `format="raw"` for unformatted diffs
- `format="ansi8"` for standard ANSI 8 color formatting
- `format="ansi256"` for ANSI 256 color formatting
- `format="html"` for HTML/CSS output and styling

See [Pagers](#pagers) for more details.

### Brightness

The `brightness` parameter allows you to pick a color scheme compatible
with the background color of your terminal. The options are:

- “light”: for use with light tone terminals
- “dark”: for use with dark tone terminals
- “neutral”: for use with either light or dark terminals

Here are examples of terminal screen renderings for both “rgb” and “yb”
`color.mode` for the three `brightness` levels.

<img src="ansi256brightness.png"></img>

The examples for “light” and “dark” have the backgrounds forcefully set
to a color compatible with the scheme. In actual use the base background
and foreground colors are left unchanged, which will look bad if you use
“dark” with light colored backgrounds or vice versa. Since we do not
know of a good cross platform way of detecting terminal background color
the default `brightness` value is “neutral”.

At this time the only `format` that is affected by this parameter is
“ansi256”. If you want to specify your own light/dark/neutral schemes
you may do so either by specifying a [style](#styles) directly or with
[Palette of Styles](#styles).

### Pagers

In interactive mode, if the diff output is very long or if your terminal
does not support ANSI colors, `diff*` methods will pipe output to a
pager. This is done by writing the output to a temporary file and
passing the file reference to the pager. The default action is to invoke
the pager with `file.show` if your terminal supports ANSI colors and the
pager is known to support ANSI colors as well (as of this writing, only
`less` is assumed to support ANSI colors), or if not to use
`getOption("viewer")` if available (this outputs to the viewport in
RStudio), or if not to use `browseURL`.

You can fine tune when, how, and if a pager is used with the `pager`
parameter. See `?diffPrint` and `?Pager` for more details.

### Styles

You can control almost all aspects of the diff output formatting via the
`style` parameter. To do so, pass an appropriately configured `Style`
object. See `?Style` for more details on how to do this.

The default is to auto pick a style based on the values of the `format`,
`color.mode`, and `brightness` parameters. This is done by using the
computed values for each of those parameters to subset the
`PaletteOfStyles` object passed as the `palette.of.styles` parameter.
This `PaletteOfStyles` object contains a `Style` object for all the
possible permutations of the `style`, `format`, and `color.mode`
parameters. See `?PaletteOfStyles`.

If you specify the `style` parameter the values of the `format`,
`brightness`, and `color.mode` parameters will be ignored.

## Diff Algorithm

The primary diff algorithm is Myer’s solution to the shortest edit
script / longest common sequence problem with the Hirschberg linear
space refinement as described in:

> E. Myers, “An O(ND) Difference Algorithm and Its Variations”,
> Algorithmica 1, 2 (1986), 251-266.

and should be the same algorithm used by GNU diff. The implementation
used here is a heavily modified version of Michael B. Allen’s diff
program from the
[`libmba`](http://www.ioplex.com/~miallen/libmba/dl/libmba-0.9.1.tar.gz)
`C` library. Any and all bugs in the C code in this package were most
likely introduced by yours truly. Please note that the resulting C code
is incompatible with the original `libmba` library.

## Performance Considerations

### Diff

The diff algorithm scales with the *square* of the number of
*differences*. For reasonably small diffs (\< 10K differences), the diff
itself is unlikely to be the bottleneck.

### Capture and Processing

Capture of inputs for `diffPrint` and `diffStr`, and processing of
output for all `diff*` methods will account for most of the execution
time unless you have large numbers of differences. This input and output
processing scales mostly linearly with the input size.

You can improve performance somewhat by using `diffChr` since that skips
the capture part, and by turning off `word.diff`:

``` r
v1 <- 1:5e4
v2 <- v1[-sample(v1, 100)]
diffChr(v1, v2, word.diff=FALSE)
```

will be \~2x as fast as:

``` r
diffPrint(v1, v2)
```

*Note*: turning off `word.diff` when using `diffPrint` with unnamed
atomic vectors can actually *slow down* the diff because there may well
be fewer element by element differences than line differences as
displayed. For example, when comparing `1:1e6` to `2:1e6` there is only
one element difference, but every line as displayed is different because
of the shift. Using `word.diff=TRUE` (and `unwrap.atomic=TRUE`) allows
`diffPrint` to compare element by element rather than line by line.
`diffChr` always compares element by element.

### Minimal Diff

If you are looking for the fastest possible diff you can use `ses` and
completely bypass most input and output processing. Inputs will be
coerced to character if they are not character.

``` r
ses(letters[1:5], letters[c(2:3, 5)])
```

    ## [1] "1d0" "4d2"

This will be 10-20x faster than `diffChr`, at the cost of less useful
output.
