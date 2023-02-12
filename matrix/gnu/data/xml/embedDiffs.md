Embed Diffs
================

## Rmarkdown

### Basic Requirements

Any R chunks that produce diffs should include the `results='asis'`
option, e.g.:

    ```{r, comment="", results="asis"}
    # R code here
    ```

### Embedded CSS

This is what a basic code block should look like:

    ```{r, comment="", results="asis"}
    cat(                                 # output to screen
      as.character(                      # convert to diff to character vector
        diffPrint(                       # run diff
          1:5, 2:6,
          format="html",                 # specify html output
          style=list(
            html.output="diff.w.style"   # configure html style
          )
    ) ) )
    ```

Here we use this same code as an actual markdown R code block:

``` r
cat(
  as.character(
    diffPrint(
      1:5, 2:6,
      format="html",
      style=list(html.output="diff.w.style")
) ) )
```

<style type='text/css'>
/* Structural CSS ------------------------------------------------------------*/
/*
 * TBD whether we want a more fully table like structure; some of the visual
 * cues provided by the current set-up are useful (line wraps, etc.)
 */

DIV.diffobj-container PRE.diffobj-content {
  white-space: pre-wrap;
  margin: 0;
}
DIV.diffobj-container DIV.diffobj-row {
  width: 100%;
  font-family: monospace;
  display: table;
  table-layout: fixed;
}
DIV.diffobj-container DIV.diffobj-line {
  width: auto;
  display: table-cell;
  overflow: hidden;
}
DIV.diffobj-container DIV.diffobj-line>DIV {
  width: 100%;
  display: table;
  table-layout: auto;
}
DIV.diffobj-container DIV.diffobj-line.banner>DIV {
  display: table;
  table-layout: auto; /* set to fixed in JS */
}
DIV.diffobj-container DIV.diffobj-text {
  display: table-cell;
  width: 100%;
}
DIV.diffobj-container DIV.diffobj-gutter {
  display: table-cell;
  padding: 0 0.2em;
}
DIV.diffobj-container DIV.diffobj-gutter DIV {
  display: table-cell;
}
#diffobj_content_meta DIV.diffobj-container DIV.diffobj-row {
  width: auto;
}
#diffobj_banner_meta DIV.diffobj-container DIV.diffobj-line.banner>DIV {
  table-layout: auto;
}
#diffobj_outer {
  overflow: hidden;
}
/* Summary -------------------------------------------------------------------*/ 

DIV.diffobj-container DIV.diffobj-summary DIV.map {
  word-wrap: break-word;
  padding-left: 1em;
}
DIV.diffobj-container DIV.diffobj-summary DIV.detail {
  padding-left: 1em;
}

/* Common elements -----------------------------------------------------------*/

DIV.diffobj-container DIV.diffobj-line.banner {
  font-size: 1.2em;
  font-weight: bold;
  overflow: hidden;
}
/* truncate banners */
DIV.diffobj-container DIV.diffobj-line.banner DIV.diffobj-text DIV{
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  width: 100%;             /* need to compute and set in JS */
}
DIV.diffobj-container DIV.diffobj-gutter,
DIV.diffobj-container DIV.diffobj-guide,
DIV.diffobj-container DIV.diffobj-fill,
DIV.diffobj-container DIV.context_sep,
DIV.diffobj-container SPAN.diffobj-trim {
  color: #999;
}
DIV.diffobj-container DIV.diffobj-header {
  font-size: 1.1em;
}
DIV.diffobj-container DIV.diffobj-text>DIV.diffobj-match,
DIV.diffobj-container DIV.diffobj-text>DIV.diffobj-guide {
  background-color: #ffffff;
}
DIV.diffobj-container DIV.diffobj-text>DIV.diffobj-fill {
  background-color: transparent;
}
DIV.diffobj-container DIV.diffobj-text>DIV {
  padding-right: 3px;
}
DIV.diffobj-container DIV.diffobj-text>DIV {
  border-left: 1px solid #888888;
}
DIV.diffobj-container DIV.diffobj-line {
  background-color: #eeeeee;
}
DIV.diffobj-container DIV.diffobj-text>DIV,
DIV.diffobj-container DIV.diffobj-header {
  padding-left: 0.5em;
}
DIV.diffobj-container DIV.diffobj-line>DIV.diffobj-match,
DIV.diffobj-container DIV.diffobj-line>DIV.diffobj-fill,
DIV.diffobj-container DIV.diffobj-line>DIV.diffobj-guide {
  border-left: 1px solid #888888;
}
/* github inspired color scheme - default ------------------------------------*/

DIV.diffobj-container.light.rgb SPAN.diffobj-word.insert,
DIV.diffobj-container.light.rgb DIV.diffobj-line>DIV.insert {
  background-color: #a6f3a6;
}
DIV.diffobj-container.light.rgb SPAN.diffobj-word.delete,
DIV.diffobj-container.light.rgb DIV.diffobj-line>DIV.delete {
  background-color: #f8c2c2;
}
DIV.diffobj-container.light.rgb DIV.diffobj-text>DIV.insert {
  background-color: #efffef;
}
DIV.diffobj-container.light.rgb DIV.diffobj-text>DIV.insert,
DIV.diffobj-container.light.rgb DIV.diffobj-line>DIV.insert {
  border-left: 1px solid #33bb33;
}
DIV.diffobj-container.light.rgb DIV.diffobj-text>DIV.delete {
  background-color: #ffefef;
}
DIV.diffobj-container.light.rgb DIV.diffobj-text>DIV.delete,
DIV.diffobj-container.light.rgb DIV.diffobj-line>DIV.delete {
  border-left: 1px solid #cc6666;
}
DIV.diffobj-container.light.rgb DIV.diffobj-header {
  background-color: #e0e6fa;
  border-left: 1px solid #9894b6;
}
/* Yellow Blue variation -----------------------------------------------------*/

DIV.diffobj-container.light.yb SPAN.diffobj-word.insert,
DIV.diffobj-container.light.yb DIV.diffobj-line>DIV.insert {
  background-color: #c0cfff;
}
DIV.diffobj-container.light.yb SPAN.diffobj-word.delete,
DIV.diffobj-container.light.yb DIV.diffobj-line>DIV.delete {
  background-color: #e7e780;
}
DIV.diffobj-container.light.yb DIV.diffobj-text>DIV.insert {
  background-color: #efefff;
}
DIV.diffobj-container.light.yb DIV.diffobj-text>DIV.insert,
DIV.diffobj-container.light.yb DIV.diffobj-line>DIV.insert {
  border-left: 1px solid #3333bb;
}
DIV.diffobj-container.light.yb DIV.diffobj-text>DIV.delete {
  background-color: #fefee5;
}
DIV.diffobj-container.light.yb DIV.diffobj-text>DIV.delete,
DIV.diffobj-container.light.yb DIV.diffobj-line>DIV.delete {
  border-left: 1px solid #aaaa55;
}
DIV.diffobj-container.light.yb DIV.diffobj-header {
  background-color: #afafaf;
  border-left: 1px solid #e3e3e3;
  color: #e9e9e9;
}
DIV.diffobj-container.light.yb DIV.diffobj-line {
  background-color: #eeeeee;
}
</style>

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>1:5</span></div></div></div></div><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>2:6</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 1 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 1 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>[1] </span><span class='diffobj-word delete'>1</span> 2 3 4 5<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[1] </span>2 3 4 5 <span class='diffobj-word insert'>6</span><span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

This is an ugly implementation because it produces illegal HTML. The
styles are directly embedded in the body of the document, outside of the
HEAD tags. Although this is illegal HTML, it seems to work in most
browsers. Another problem is that every diff you use in your document
will inject the same CSS code over and over.

### External CSS

A better option is to provide the CSS directly by modifying the `output`
portion of the [YAML
header](https://bookdown.org/yihui/rmarkdown/r-package-vignette.html):

    ---
    output:
        rmarkdown::html_vignette:
            toc: true
            css: !expr diffobj::diffobj_css()
    ---

In reality you will probably want to specify multiple CSS files,
including the original `rmarkdown` one:

    ---
    output:
        rmarkdown::html_vignette:
            toc: true
            css:
              - !expr diffobj::diffobj_css()
              - !expr system.file("rmarkdown", "templates", "html_vignette", "resources", "vignette.css", package = "rmarkdown")
    ---

Once you set this up then you can use:

``` r
cat(
  as.character(
    diffPrint(
      1:5, 2:6,
      format="html",
      style=list(html.output="diff.only")   # notice this changed
) ) )
```

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>1:5</span></div></div></div></div><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>2:6</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 1 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 1 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>[1] </span><span class='diffobj-word delete'>1</span> 2 3 4 5<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[1] </span>2 3 4 5 <span class='diffobj-word insert'>6</span><span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

This will omit the CSS, but since we include it via the YAML everything
should work as expected.

### Use Options

Almost all `diffobj` parameters can be specified via options:

``` r
options(
  diffobj.format="html",
  diffobj.style=list(html.output="diff.only")
)
```

Then you can just run the diff as normal:

``` r
cat(as.character(diffPrint(1:5, 2:6)))
```

<div class="diffobj-container light yb">

<pre class='diffobj-content'><div class='diffobj-row'><div class='diffobj-line banner'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-word delete'>1:5</span></div></div></div></div><div class='diffobj-line banner'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-word insert'>2:6</span></div></div></div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='diffobj-header'>@@ 1 @@</div></div><div class='diffobj-line'><div class='diffobj-header'>@@ 1 @@</div></div></div><div class='diffobj-row'><div class='diffobj-line'><div class='delete'><div class='diffobj-gutter'><div class='delete'>&lt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='delete'><span class='diffobj-trim'>[1] </span><span class='diffobj-word delete'>1</span> 2 3 4 5<span class='diffobj-trim'></span></div></div></div></div><div class='diffobj-line'><div class='insert'><div class='diffobj-gutter'><div class='insert'>&gt;</div><div class='pad'> </div></div><div class='diffobj-text'><div class='insert'><span class='diffobj-trim'>[1] </span>2 3 4 5 <span class='diffobj-word insert'>6</span><span class='diffobj-trim'></span></div></div></div></div></div></pre>

</div>

## Shiny

Shiny usage is very similar to `rmarkdown`. In both cases we want to get
`diffobj` to produce HTML output to embed in our document. If we are
willing to embed the CSS with each diff, we can use:

``` r
library(shiny)
shinyApp(
  ui=fluidPage(htmlOutput('diffobj_element')),
  server=function(input, output) {
    output$diffobj_element <- renderUI({
      HTML(
        as.character(
          diffPrint(
            1:5, 2:6,
            format="html",
            style=list(html.output="diff.w.style")
) ) )}) } )
```

If we have many diffs, it may be preferable to use options and external
style sheet:

``` r
options(
  diffobj.format="html",
  diffobj.style=list(html.output="diff.only")
)
shinyApp(
  ui=fluidPage(
    includeCSS(diffobj_css()),
    htmlOutput('diffobj_element')
  ),
  server=function(input, output) {
    output$diffobj_element <- renderUI({
      HTML(as.character(diffPrint(1:5, 2:6,)))
}) } )
```

Unlike with our [rmarkdown example](#external-css), this CSS is included
in the body of the HTML document instead of in the header, so it is
technically illegal like in our [embedded css example](#embedded-css).
