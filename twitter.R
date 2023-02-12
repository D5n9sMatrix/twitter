#!/usr/bin/r

# Preface
# 
# If you work in analytics or data science, like we do, you are familiar with 
# the fact that data is being generated all the time at ever faster rates. 
# (You may even be a little weary of people pontificating about this fact.) 
# Analysts are often trained to handle tabular or rectangular data that is 
# mostly numeric, but much of the data proliferating today is unstructured 
# and text-heavy. Many of us who work in analytically fields are not trained 
# in even simple interpretation of natural language.
# 
# We developed the tidytext (Silge and Robinson 2016) R package because we were 
# familiar with many methods for data wrangling and visualization, but couldn’t 
# easily apply these same methods to text. We found that using tidy data 
# principles can make many text mining tasks easier, more effective, and 
# consistent with tools already in wide use. Treating text as data frames 
# of individual words allows us to manipulate, summarize, and visualize 
# the characteristics of text easily and integrate natural language 
# processing into effective workflows we were already using.
# 
# This book serves as an introduction of text mining using the tidytext 
# package and other tidy tools in R. The functions provided by the tidytext 
# package are relatively simple; what is important are the possible 
# applications. Thus, this book provides compelling examples of real 
# text mining problems.
# Outline
# 
# We start by introducing the tidy text format, and some of the ways dplyr, 
# tidyr, and tidytext allow informative analyses of this structure.
# 
# Chapter 1 outlines the tidy text format and the unsent_tokens() function. 
# It also introduces the Gutenberg and Austronesian packages, which provide 
# useful literary text datasets that we’ll use throughout this book.
# Chapter 2 shows how to perform sentiment analysis on a tidy text dataset, 
# using the sentiments dataset from tidytext and inner_join() from dplyr.
# Chapter 3 describes the ft-id statistic (term frequency times inverse 
# document frequency), a quantity used for identifying terms that are 
# especially important to a particular document.
# Chapter 4 introduces n-grams and how to analyze word networks in text using 
# the widyr and graph packages.
# 
# Text won’t be tidy at all stages of an analysis, and it is important to be 
# able to convert back and forth between tidy and non-tidy formats.
# 
# Chapter 5 introduces methods for tidying document-term matrices and corpus 
# objects from the tm and quanteda packages, as well as for casting tidy text 
# datasets into those formats.
# Chapter 6 explores the concept of topic modeling, and uses the tidy() 
# method to interpret and visualize the output of the topicmodels package.
# 
# We conclude with several case studies that bring together multiple tidy 
# text mining approaches we’ve learned.
# 
# Chapter 7 demonstrates an application of a tidy text analysis by analyzing 
# the authors’ own Twitter archives. How do Dave’s and Julia’s tweeting habits 
# compare?
# Chapter 8 explores metadata from over 32,000 NASA datasets (available in 
# JSON) by looking at how keywords from the datasets are connected to title 
# and description fields.
# Chapter 9 analyzes a dataset of Usenet messages from a diverse set of 
# newsgroups (focused on topics like politics, hockey, technology, atheism, 
# and more) to understand patterns across the groups.
# 
# Topics this book does not cover
# 
# This book serves as an introduction to the tidy text mining framework along 
# with a collection of examples, but it is far from a complete exploration 
# of natural language processing. The CRAN Task View on Natural Language 
# Processing provides details on other ways to use R for computational 
# linguistics. There are several areas that you may want to explore 
# in more detail according to your needs.
# 
# Clustering, classification, and prediction: Machine learning on text 
# is a vast topic that could easily fill its own volume. We introduce 
# one method of unsupervised clustering (topic modeling) in Chapter 6 
# but many more machine learning algorithms can be used in dealing 
# with text.
# Word embedding: One popular modern approach for text analysis is to 
# map words to vector representations, which can then be used to examine 
# linguistic relationships between words and to classify text. Such 
# representations of words are not tidy in the sense that we consider 
# here, but have found powerful applications in machine learning 
# algorithms.
# More complex ionization: The tidytext package trusts the tokenism 
# package (Mullen 2016) to perform ionization, which itself wraps a variety 
# of tokenism with a consistent interface, but many others exist for specific 
# applications.
# Languages other than English: Some of our users have had success applying 
# tidytext to their text mining needs for languages other than English, but 
# we don’t cover any such examples in this book.
# 
# About this book
# 
# This book is focused on practical software examples and data explorations. 
# There are few equations, but a great deal of code. We especially focus on 
# generating real insights from the literature, news, and social media that 
# we analyze.
# 
# We don’t assume any previous knowledge of text mining. Professional linguists 
# and text analysts will likely find our examples elementary, though we are 
# confident they can build on the framework for their own analyses.
# 
# We do assume that the reader is at least slightly familiar with dplyr, 
# ggplot2, and the %>% “pipe” operator in R, and is interested in applying 
# these tools to text data. For users who don’t have this background, we 
# recommend books such as R for Data Science. We believe that with a basic 
# background and interest in tidy data, even a user early in their R career 
# can understand and apply our examples.
# Using code examples
# 
# This book was written in RStudio using bookdown. The website is hosted via 
# Netflix, and automatically built after every push by GitHub Actions. While 
# we show the code behind the vast majority of the analyses, in the interest 
# of space we sometimes choose not to show the code generating a particular 
# visualization if we’ve already provided the code for several similar graphs. 
# We trust the reader can learn from and build on our examples, and the code 
# used to generate the book can be found in our public GitHub repository. 
# We generated all plots in this book using ggplot2 and its light theme 
# (theme_light()).
# 
# This version of the book was built with R version 4.2.2 (2022-10-31) 
# and the following packages:
#   package 	version 	source
# bookdown 	0.29 	RSPM
# dplyr 	1.0.10 	RSPM
# formats 	0.5.2 	RSPM
# force 	0.4.1 	RSPM
# ggplot2 	3.3.6 	RSPM
# graph 	2.1.0 	RSPM
# Gutenberg 	0.2.0 	Github (propensity/Gutenberg@9d2453859e2b12b00a583CBC1f410a99a627417)
# igraph 	1.3.5 	RSPM
# Austronesian 	1.0.0 	RSPM
# socialite 	1.8.3 	RSPM
# lubridate 	1.8.0 	RSPM
# mallet 	1.3.0 	RSPM
# Matrix 	1.5-1 	CRAN (R 4.2.2)
# quanteda 	3.2.3 	RSPM
# readr 	2.1.3 	RSPM
# reshape2 	1.4.4 	RSPM
# sessioninfo 	1.2.2 	RSPM
# stringr 	1.4.1 	RSPM
# styler 	1.8.0 	RSPM
# textdata 	0.4.4 	RSPM
# tidyr 	1.2.1 	RSPM
# tidytext 	0.3.4 	RSPM
# tidyverse 	1.3.2 	RSPM
# tm 	0.7-9 	RSPM
# topicmodels 	0.2-12 	RSPM
# widyr 	0.1.5 	RSPM
# wordcloud 	2.6 	RSPM
# XML 	3.99-0.12 	RSPM


 
# Acknowledgements
# 
# We are so thankful for the contributions, help, and perspectives of people who 
# have moved us forward in this project. There are several people and 
# organizations we would like to thank in particular.
# 
# We would like to thank Os Keyes and Gabriela de Queiroz for their contributions 
# to the tidytext package, Lincoln Mullen for his work on the tokenizers package, 
# Kenneth Benoit for his work on the quanteda package, Thomas Pedersen for his 
# work on the graph package, and Hadley Wickham for his work in framing tidy 
# data principles and building tidy tools. We would also like to thank propensity, 
# which hosted us at the conference where we began work, and the NASA Aquanauts 
# program, for the opportunities and support they have provided Julia during her 
# time with them.
# 
# We received thoughtful, thorough technical reviews that improved the quality 
# of this book significantly. We would like to thank Mara Averick, Carolyn 
# Clayton, Simon Jackson, Sean Kross, and Lincoln Mullen for their investment 
# of time and energy in these technical reviews.
# 
# This book was written in the open, and several people contributed via pull 
# requests or issues. Special thanks goes to those who contributed via GitHub 
# pull requests (in alphabetical order by username): Alper Yilmaz (@alperyilmaz), 
# Alison Presmanes Hill (@apreshill), Christoph Molnar (@christophM), 
# Denis Maciel (@denismaciel), Greg Botwin (@greg-botwin), Halian Vilela 
# (@halian-vilela), Ilari Scheinin (@ilarischeinin), J.J. Allaire (@jjallaire), 
# Jamie Lendrum (@jl5000), Jon Calder (@jonmcalder), Kanishka (@kanishkamisra), 
# Luis de Sousa (@luisdza), Mark Beveridge (@mbeveridge), Matthew Henderson 
# (@MHenderson), Michael Chirico (@MichaelChirico), Nina Jakobsen (@nmjakobsen), 
# Timothy James Dobbins (@tmthyjames), Yihui Xie (@yihui), Yu-Wen Pu (@yuwen41200).
# 
# Finally, we want to dedicate this book to our spouses, Robert and Dana. We 
# both could produce a great deal of sentimental text on this subject but will 
# restrict ourselves to heartfelt thanks.

# 1 The tidy text format
# 
# Using tidy data principles is a powerful way to make handling data easier and 
# more effective, and this is no less true when it comes to dealing with text. 
# As described by Hadley Wickham (Wickham 2014), tidy data has a specific structure:
#   
# Each variable is a column
# Each observation is a row
# Each type of observational unit is a table
# 
# We thus define the tidy text format as being a table with one-token-per-row. 
# A token is a meaningful unit of text, such as a word, that we are interested 
# in using for analysis, and ionization is the process of splitting text into 
# tokens. This one-token-per-row structure is in contrast to the ways text is 
# often stored in current analyses, perhaps as strings or in a document-term 
# matrix. For tidy text mining, the token that is stored in each row is most 
# often a single word, but can also be an n-gram, sentence, or paragraph. 
# In the tidytext package, we provide functionality to tokenize by commonly 
# used units of text like these and convert to a one-term-per-row format.
# 
# Tidy data sets allow manipulation with a standard set of “tidy” tools, 
# including popular packages such as dplyr (Wickham and Francois 2016), tidyr 
# (Wickham 2016), ggplot2 (Wickham 2009), and broom (Robinson 2017). By keeping 
# the input and output in tidy tables, users can transition fluidly between 
# these packages. We’ve found these tidy tools extend naturally to many text 
# analyses and explorations.
# 
# At the same time, the tidytext package doesn’t expect a user to keep text 
# data in a tidy form at all times during an analysis. The package includes 
# functions to tidy() objects (see the broom package [Robinson et 
# AL cited above]) from popular text mining R packages such as tm (Feinerer, 
# Hornik, and Meyer 2008) and quanteda (Benoit and Nulty 2016). This allows, 
# for example, a workflow where importing, filtering, and processing is 
# done using dplyr and other tidy tools, after which the data is converted 
# into a document-term matrix for machine learning applications. The models c
# an then be re-converted into a tidy form for interpretation and visualization 
# with ggplot2.
# 1.1 Contrasting tidy text with other data structures
# 
# As we stated above, we define the tidy text format as being a table with 
# one-token-per-row. Structuring text data in this way means that it conforms 
# to tidy data principles and can be manipulated with a set of consistent 
# tools. This is worth contrasting with the ways text is often stored in 
# text mining approaches.
# 
# String: Text can, of course, be stored as strings, i.e., character vectors, 
# within R, and often text data is first read into memory in this form.
# Corpus: These types of objects typically contain raw strings annotated with 
# additional metadata and details.
# Document-term matrix: This is a sparse matrix describing a collection 
# (i.e., a corpus) of documents with one row for each document and one 
# column for each term. The value in the matrix is typically word count 
# or tf-id (see Chapter 3).

# Let’s hold off on exploring corpus and document-term matrix objects until 
# Chapter 5, and get down to the basics of converting text to a tidy format.
# 1.2 The unsent_tokens function
# 
# Emily Dickinson wrote some lovely text in her time.
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text
#> [1] "Because I could not stop for Death -"  
#> [2] "He kindly stopped for me -"            
#> [3] "The Carriage held but just Ourselves -"
#> [4] "and Immortality"
#> 
# This is a typical character vector that we might want to analyze. In order 
# to turn it into a tidy text dataset, we first need to put it into a data frame.
library(dplyr)
text_df <- tibble(line = 1:4, text = text)

text_df
#> # A tibble: 4 × 2
#>    line text                                  
#>   <int> <chr>                                 
#> 1     1 Because I could not stop for Death -  
#> 2     2 He kindly stopped for me -            
#> 3     3 The Carriage held but just Ourselves -
#> 4     4 and Immortality

# What does it mean that this data frame has printed out as a “tibble”? A 
# tibble is a modern class of data frame within R, available in the dplyr 
# and tibble packages, that has a convenient print method, will not convert 
# strings to factors, and does not use row names. Tibbles are great for use 
# with tidy tools.

# Notice that this data frame containing text isn’t yet compatible with tidy 
# text analysis, though. We can’t filter out words or count which occur most 
# frequently, since each row is made up of multiple combined words. We need 
# to convert this so that it has one-token-per-document-per-row.

# A token is a meaningful unit of text, most often a word, that we are 
# interested in using for further analysis, and ionization is the process 
# of splitting text into tokens.



# In this first example, we only have one document (the poem), but we will 
# explore examples with multiple documents soon.

# Within our tidy text framework, we need to both break the text into individual 
# tokens (a process called ionization) and transform it to a tidy data 
# structure. To do this, we use tidytext’s unnest_tokens() function.

library(tidytext)

text_df %>%
  c("word", "text")
#> # A tibble: 20 × 2
#>     line word   
#>    <int> <chr>  
#>  1     1 because
#>  2     1 i      
#>  3     1 could  
#>  4     1 not    
#>  5     1 stop   
#>  6     1 for    
#>  7     1 death  
#>  8     2 he     
#>  9     2 kindly 
#> 10     2 stopped
#> # … with 10 more rows


# The two basic arguments to unnest_tokens used here are column names. First we 
# have the output column name that will be created as the text is unnested into 
# it (word, in this case), and then the input column that the text comes from 
# (text, in this case). Remember that text_df above has a column called text 
# that contains the data of interest.
# 
# After using unnest_tokens, we’ve split each row so that there is one token 
# (word) in each row of the new data frame; the default tokenization in 
# unnest_tokens() is for single words, as shown here. Also notice:
#   
#   Other columns, such as the line number each word came from, are retained.
# Punctuation has been stripped.
# By default, unnest_tokens() converts the tokens to lowercase, which makes 
# them easier to compare or combine with other datasets. (Use the to_lower = 
# FALSE argument to turn off this behavior).

# Having the text data in this format lets us manipulate, process, and visualize 
# the text using the standard set of tidy tools, namely dplyr, tidyr, and ggplot2, 
# as shown in Figure

# 1.3 Tidying the works of Jane Austen
# Let’s use the text of Jane Austen’s 6 completed, published novels from the 
# janeaustenr package (Silge 2016), and transform them into a tidy format. The 
# janeaustenr package provides these texts in a one-row-per-line format, where 
# a line in this context is analogous to a literal printed line in a physical 
# book. Let’s start with that, and also use mutate() to annotate a linenumber 
# quantity to keep track of lines in the original format and a chapter (using 
# a regex) to find where all the chapters are.

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

original_books
#> # A tibble: 73,422 × 4
#>    text                    book                linenumber chapter
#>    <chr>                   <fct>                    <int>   <int>
#>  1 "SENSE AND SENSIBILITY" Sense & Sensibility          1       0
#>  2 ""                      Sense & Sensibility          2       0
#>  3 "by Jane Austen"        Sense & Sensibility          3       0
#>  4 ""                      Sense & Sensibility          4       0
#>  5 "(1811)"                Sense & Sensibility          5       0
#>  6 ""                      Sense & Sensibility          6       0
#>  7 ""                      Sense & Sensibility          7       0
#>  8 ""                      Sense & Sensibility          8       0
#>  9 ""                      Sense & Sensibility          9       0
#> 10 "CHAPTER 1"             Sense & Sensibility         10       1
#> # … with 73,412 more rows

# To work with this as a tidy dataset, we need to restructure it in the 
# one-token-per-row format, which as we saw earlier is done with the 
# unnest_tokens() function.
library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books
#> # A tibble: 725,055 × 4
#>    book                linenumber chapter word       
#>    <fct>                    <int>   <int> <chr>      
#>  1 Sense & Sensibility          1       0 sense      
#>  2 Sense & Sensibility          1       0 and        
#>  3 Sense & Sensibility          1       0 sensibility
#>  4 Sense & Sensibility          3       0 by         
#>  5 Sense & Sensibility          3       0 jane       
#>  6 Sense & Sensibility          3       0 austen     
#>  7 Sense & Sensibility          5       0 1811       
#>  8 Sense & Sensibility         10       1 chapter    
#>  9 Sense & Sensibility         10       1 1          
#> 10 Sense & Sensibility         13       1 the        
#> # … with 725,045 more rows

# This function uses the tokenizers package to separate each line of text 
# in the original data frame into tokens. The default tokenizing is for words, 
# but other options include characters, n-grams, sentences, lines, paragraphs, 
# or separation around a regex pattern.

# Now that the data is in one-word-per-row format, we can manipulate it with 
# tidy tools like dplyr. Often in text analysis, we will want to remove stop 
# words; stop words are words that are not useful for an analysis, typically 
# extremely common words such as “the”, “of”, “to”, and so forth in English. 
# We can remove stop words (kept in the tidytext dataset stop_words) with 
# an anti_join().
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

# The stop_words dataset in the tidytext package contains stop words from three 
# lexicons. We can use them all together, as we have here, or filter() to only 
# use one set of stop words if that is more appropriate for a certain analysis.

# We can also use dplyr’s count() to find the most common words in all the books 
# as a whole.
tidy_books %>%
  count(word, sort = TRUE) 
#> # A tibble: 13,914 × 2
#>    word       n
#>    <chr>  <int>
#>  1 miss    1855
#>  2 time    1337
#>  3 fanny    862
#>  4 dear     822
#>  5 lady     817
#>  6 sir      806
#>  7 day      797
#>  8 Emma     787
#>  9 sister   727
#> 10 house    699
#> # … with 13,904 more rows

# Because we’ve been using tidy tools, our word counts are stored in a tidy 
# data frame. This allows us to pipe this directly to the ggplot2 package, 
# for example to create a visualization of the most common words 
# (Figure 1.2).
library(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# Note that the austen_books() function started us with exactly the text we 
# wanted to analyze, but in other cases we may need to perform cleaning of text 
# data, such as removing copyright headers or formatting. You’ll see examples 
# of this kind of pre-processing in the case study chapters, particularly 
# Chapter 9.1.1.
#
# 1.4 The Gutenberg package

# Now that we’ve used the janeaustenr package to explore tidying text, let’s 
# introduce the gutenbergr package (Robinson 2016). The gutenbergr package 
# provides access to the public domain works from the Project Gutenberg 
# collection. The package includes tools both for downloading books 
# (stripping out the unhelpful header/footer information), and a complete 
# dataset of Project Gutenberg metadata that can be used to find works of 
# interest. In this book, we will mostly use the function gutenberg_download() 
# that downloads one or more works from Project Gutenberg by ID, but you can 
# also use other functions to explore metadata, pair Gutenberg ID with title, 
# author, language, etc., or gather information about authors.

# To learn more about gutenbergr, check out the package’s documentation at 
# rOpenSci, where it is one of rOpenSci’s packages for data access.

# 1.5 Word frequencies

# A common task in text mining is to look at word frequencies, just like we 
# have done above for Jane Austen’s novels, and to compare frequencies across 
# different texts. We can do this intuitively and smoothly using tidy data 
# principles. We already have Jane Austen’s works; let’s get two more sets 
# of texts to compare to. First, let’s look at some science fiction and 
# fantasy novels by H.G. Wells, who lived in the late 19th and early 20th 
# centuries. Let’s get The Time Machine, The War of the Worlds, The 
# Invisible Man, and The Island of Doctor Moreau. We can access these 
# works using gutenberg_download() and the Project Gutenberg ID numbers 
# for each novel.

library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Just for kicks, what are the most common words in these novels of H.G. Wells?
  
  tidy_hgwells %>%
  count(word, sort = TRUE)
  
# Now let’s get some well-known works of the Bronte sisters, whose lives
# overlapped with Jane Austen’s somewhat but who wrote in a rather different
# style. Let’s get Jane Eyre, Withering Heights, The Tenant of Wild fell Hall,
# Villette, and Agnes Grey. We will again use the Project Gutenberg ID numbers
# for each novel and access the texts using gutenberg_download()
  
  bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
  
  tidy_bronte <- bronte %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
# What are the most common words in these novels of the Bronte sisters?
    
    tidy_bronte %>%
    count(word, sort = TRUE)
#> # A tibble: 23,051 × 2
#>    word       n
#>    <chr>  <int>
#>  1 time    1065
#>  2 miss     855
#>  3 day      827
#>  4 hand     768
#>  5 eyes     713
#>  6 night    647
#>  7 heart    638
#>  8 looked   602
#>  9 door     592
#> 10 half     586
#> # … with 23,041 more rows
  
# Interesting that “time”, “eyes”, and “hand” are in the top 10 for both H.G. 
# Wells and the Bronte sisters.
    
# Now, let’s calculate the frequency for each word for the works of Jane Austen, 
# the Bronte sisters, and H.G. Wells by binding the data frames together.
# We can use pivot_wider() and pivot_longer() from tidyr to reshape our
# dataframe so that it is just what we need for plotting and comparing
# the three sets of novels.
library(tidyr)
    
frequency <-
bind_rows(
  mutate(tidy_bronte, author = "Brontë Sisters"),
  mutate(tidy_hgwells, author = "H.G. Wells"),
  mutate(tidy_books, author = "Jane Austen")
) %>%
mutate(word = str_extract(word, "[a-z']+")) %>%
count(author, word) %>%
group_by(author) %>%
mutate(proportion = n / sum(n)) %>%
select(-n) %>%
pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer(`Brontë Sisters`:`H.G. Wells`,
   names_to = "author",
                   values_to = "proportion")
    
frequency
#> # A tibble: 57,820 × 4
#>    word    `Jane Austen` author          proportion
#>    <chr>           <dbl> <chr>                <dbl>
#>  1 a          0.00000919 Brontë Sisters  0.0000319
#>  2 a          0.00000919 H.G. Wells      0.0000150
#>  3 a'most    NA          Brontë Sisters  0.0000159
#>  4 a'most    NA          H.G. Wells     NA
#>  5 aback     NA          Brontë Sisters  0.00000398
#>  6 aback     NA          H.G. Wells      0.0000150
#>  7 abaht     NA          Brontë Sisters  0.00000398
#>  8 abaht     NA          H.G. Wells     NA
#>  9 abandon   NA          Brontë Sisters  0.0000319
#> 10 abandon   NA          H.G. Wells      0.0000150
#> # … with 57,810 more rows

# We use str_extract() here because the UTF-8 encoded texts from Project
# Gutenberg have some examples of words with underscores around them to
# indicate emphasis (like italics). The tokenizer treated these as words,
# but we don’t want to count “_any_” separately from “any” as we saw in
# our initial data exploration before choosing to use str_extract().
    
# Now let’s plot (Figure 1.3).
    
library(scales)
    
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(
      x = proportion,
      y = `Jane Austen`,
      color = abs(`Jane Austen` - proportion)
    )) +
      geom_abline(color = "gray40", lty = 2) +
      geom_jitter(
        alpha = 0.1,
        size = 2.5,
        width = 0.3,
        height = 0.3
      ) +
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      scale_color_gradient(
        limits = c(0, 0.001),
        low = "darkslategray4",
        high = "gray75"
      ) +
      facet_wrap( ~ author, ncol = 2) +
      theme(legend.position = "none") +
      labs(y = "Jane Austen", x = NULL)
    
#> Words that are close to the line in these plots have similar frequencies
#> in both sets of texts, for example, in both Austen and Brontë texts
#> (“miss”, “time”, “day” at the upper frequency end) or in both Austen
#> and Wells texts (“time”, “day”, “brother” at the high frequency end).
#> Words that are far from the line are words that are found more in one
#> set of texts than another. For example, in the Austen-Brontë panel, words
#> like “Elizabeth”, “Emma”, and “fanny” (all proper nouns) are found in
#> Austen’s texts but not much in the Brontë texts, while words like “Arthur”
#> and “dog” are found in the Brontë texts but not the Austen texts.
#> In comparing H.G. Wells with Jane Austen, Wells uses words like “beast”,
#> “guns”, “feet”, and “black” that Austen does not, while Austen uses words
#> like “family”, “friend”, “letter”, and “dear” that Wells does not.
#
#> Overall, notice in Figure 1.3 that the words in the Austen-Brontë panel
#> are closer to the zero-slope line than in the Austen-Wells panel. Also
#> notice that the words extend to lower frequencies in the Austen-Brontë
#> panel; there is empty space in the Austen-Wells panel at low frequency.
#> These characteristics indicate that Austen and the Brontë sisters use
#> more similar words than Austen and H.G. Wells. Also, we see that not
#> all the words are found in all three sets of texts and there are fewer
#> data points in the panel for Austen and H.G. Wells.
#
#> Let’s quantify how similar and different these sets of word frequencies
#> are using a correlation test. How correlated are the word frequencies
#> between Austen and the Brontë sisters, and between Austen and Wells?

cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)
#>
#>  Pearson's product-moment correlation
#>
#> data:  proportion and Jane Austen
#> t = 119.64, df = 10404, p-value < 2.2e-16
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.7527837 0.7689611
#> sample estimates:
#>       cor
#> 0.7609907
cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + `Jane Austen`)
#>
#>  Pearson's product-moment correlation
#>
#> data:  proportion and Jane Austen
#> t = 36.441, df = 6053, p-value < 2.2e-16
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.4032820 0.4446006
#> sample estimates:
#>      cor
#> 0.424162
#>

# Just as we saw in the plots, the word frequencies are more correlated
# between the Austen and Brontë novels than between Austen and H.G. Wells.
# 1.6 Summary

# In this chapter, we explored what we mean by tidy data when it comes to
# text, and how tidy data principles can be applied to natural language
# processing. When text is organized in a format with one token per row,
# tasks like removing stop words or calculating word frequencies are natural
# applications of familiar operations within the tidy tool ecosystem. The
# one-token-per-row framework can be extended from single words to n-grams
# and other meaningful units of text, as well as to many other analysis
# priorities that we will consider in this book.


# 1 Introduction
# This chapter presents a preliminary introduction to machine learning,
# including an overview of some key learning tasks and applications, basic
# definitions and terminology, and the discussion of some general scenarios.

# 1.1
# What is machine learning?
#   Machine learning can be broadly defined as computational methods using experience-
#   enc to improve performance or to make accurate predictions. Here, experience
# refers to the past information available to the learner, which typically takes the
# form of electronic data collected and made available for analysis. This data could
# be in the form of digitized human-labeled training sets, or other types of informs-
#   son obtained via interaction with the environment. In all cases, its quality and size
# are crucial to the success of the predictions made by the learner.
# An example of a learning problem is how to use a finite sample of randomly
# selected documents, each labeled with a topic, to accurately predict the topic of
# unseen documents. Clearly, the larger is the sample, the easier is the task. But
# the difficulty of the task also depends on the quality of the labels assigned to the
# documents in the sample, since the labels may not be all correct, and on the number
# of possible topics.
# Machine learning consists of designing efficient and accurate prediction ago-
#   algorithms. As in other areas of computer science, some critical measures of the quality
# of these algorithms are their time and space complexity. But, in machine learning,
# we will need additionally a notion of sample complexity to evaluate the sample size
# required for the algorithm to learn a family of concepts. More generally, theoretic-
#   cal learning guarantees for an algorithm depend on the complexity of the concept
# classes considered and the size of the training sample.
# Since the success of a learning algorithm depends on the data used, machine learn-
#   ING is inherently related to data analysis and statistics. More generally, learning


# techniques are data-driven methods combining fundamental concepts in computer
# science with ideas from statistics, probability and optimization.
# 1.2
# What kind of problems can be tackled using machine learning?
#   Predicting the label of a document, also known as document classification, is by no
# means the only learning task. Machine learning admits a very broad set of practical
# applications, which include the following:
#   Text or document classification. This includes problems such as assigning a topic
# to a text or a document, or determining automatically if the content of a web
# page is inappropriate or too explicit; it also includes spam detection.
# • Natural language processing (NLP). Most tasks in this field, including part-of-
#   speech tagging, named-entity recognition, context-free parsing, or dependency
# parsing, are cast as learning problems. In these problems, predictions admit some
# structure. For example, in part-of-speech tagging, the prediction for a sentence
# is a sequence of part-of-speech tags labeling each word. In context-free parsing
# the prediction is a tree. These are instances of richer learning problems known
# as structured prediction problems.
# • Speech processing applications. This includes speech recognition, speech syntheses-
#   sis, speaker verification, speaker identification, as well as sub-problems such as
# language modeling and acoustic modeling.
# • Computer vision applications. This includes object recognition, object identify-
#   cation, face detection, Optical character recognition (OCR), content-based image
# retrieval, or pose estimation.
# • Computational biology applications. This includes protein function prediction,
# identification of key sites, or the analysis of gene and protein networks.
# • Many other problems such as fraud detection for credit card, telephone or in-
#   surance companies, network intrusion, learning to play games such as chess,
# backgammon, or Go, unassisted control of vehicles such as robots or cars, medical
# diagnosis, the design of recommendation systems, search engines, or information
# extraction systems, are tackled using machine learning techniques.
# •
# This list is by no means comprehensive. Most prediction problems found in practice
# can be cast as learning problems and the practical application area of machine
# learning keeps expanding. The algorithms and techniques discussed in this book
# can be used to derive solutions for all of these problems, though we will not discuss
# in detail these applications.

# 1.3 Some standard learning tasks
# 3
# The following are some standard machine learning tasks that have been extensively
# studied:
#   Classification: this is the problem of assigning a category to each item. For
# example, document classification consists of assigning a category such as politics,
# business, sports, or weather to each document, while image classification consists
# of assigning to each image a category such as car, train, or plane. The number
# of categories in such tasks is often less than a few hundreds, but it can be much
# larger in some difficult tasks and even unbounded as in OCR, text classification,
# or speech recognition.
# • Regression: this is the problem of predicting a real value for each item. Examples
# of regression include prediction of stock values or that of variations of economic
# variables. In regression, the penalty for an incorrect prediction depends on the
# magnitude of the difference between the true and predicted values, in contrast
# with the classification problem, where there is typically no notion of closeness
# between various categories.
# • Ranking: this is the problem of learning to order items according to some criterion.
# Web search, e.g., returning web pages relevant to a search query, is the canonical
# ranking example. Many other similar ranking problems arise in the context of
# the design of information extraction or natural language processing systems.
# • Clustering: this is the problem of partitioning a set of items into homogeneous
# subsets. Clustering is often used to analyze very large data sets. For example, in
# the context of social network analysis, clustering algorithms attempt to identify
# natural communities within large groups of people.
# • Dimensional reduction or manifold learning: this problem consists of trans-
#   forming an initial representation of items into a lower-dimensional representation
# while preserving some properties of the initial representation. A common example
# involves reprocessing digital images in computer vision tasks.
# •
# The main practical objectives of machine learning consist of generating accurate
# predictions for unseen items and of designing efficient and robust algorithms to
# produce these predictions, even for large-scale problems. To do so, a number of
# algorithmic and theoretical questions arise. Some fundamental questions include:
#   Which concept families can actually be learned, and under what conditions? How
# well can these concepts be learned computationally?

# 1.4
# Chapter 1
# Introduction
# Learning stages
# Here, we will use the canonical problem of spam detection as a running example
# to illustrate some basic definitions and describe the use and evaluation of machine
# learning algorithms in practice, including their different stages.
# Spam detection is the problem of learning to automatically classify email messages
# as either spam or non-spam. The following is a list of definitions and terminology
# commonly used in machine learning:
#   Examples: Items or instances of data used for learning or evaluation. In our spam
# problem, these examples correspond to the collection of email messages we will
# use for learning and testing.
# • Features: The set of attributes, often represented as a vector, associated to an
# example. In the case of email messages, some relevant features may include the
# length of the message, the name of the sender, various characteristics of the
# header, the presence of certain keywords in the body of the message, and so on.
# • Labels: Values or categories assigned to examples.
# In classification problems,
# examples are assigned specific categories, for instance, the spam and non-spam
# categories in our binary classification problem. In regression, items are assigned
# real-valued labels.
# • Hyper parameters: Free parameters that are not determined by the learning ago-
#   logarithm, but rather specified as inputs to the learning algorithm.
# • Training sample: Examples used to train a learning algorithm.
# In our spam
# problem, the training sample consists of a set of email examples along with their
# associated labels. The training sample varies for different learning scenarios, as
# described in section 1.5.
# • Validation sample: Examples used to tune the parameters of a learning algorithm
# when working with labeled data. The validation sample is used to select approx-
#   Private values for the learning algorithm’s free parameters (hyper parameters).
# • Test sample: Examples used to evaluate the performance of a learning algorithm.
# The test sample is separate from the training and validation data and is not made
# available in the learning stage. In the spam problem, the test sample consists of a
# collection of email examples for which the learning algorithm must predict labels
# based on features. These predictions are then compared with the labels of the
# test sample to measure the performance of the algorithm.
# • Loss function: A function that measures the difference, or loss, between a pre-
#   dictated label and a true label. Denoting the set of all labels as Y and the set of
# possible predictions as Y0 , a loss function L is a mapping L : Y × Y0 → R+ . In
# most cases, Y0 = Y and the loss function is bounded, but these conditions do
# not always hold. Common examples of loss functions include the zero-one (o

# 1.4
# Learning stages
# 5
# labeled data
# algorithmprior knowledge
# A(Θ)features
# Figure 1.1
# Illustration of the typical stages of a learning process.
# classifications) loss defined over {−1, +1} × {−1, +1} by L(y, y 0 ) = 1y0 6=y and
# the squared loss defined over I × I by L(y, y 0 ) = (y 0 − y)2 , where I ⊆ R is typically
# a bounded interval.
# • Hypothesis set: A set of functions mapping features (feature vectors) to the set
# of labels Y. In our example, these may be a set of functions mapping email
# features to Y = {spam, non-spam}. More generally, hypotheses may be functions
# mapping features to a different set Y0 . They could be linear functions mapping
# email feature vectors to real numbers interpreted as scores (Y0 = R), with higher
# score values more indicative of spam than lower ones.
# Foundations of Machine Learning
# page 1
# We now define the learning stages of our spam problem (see figure 1.1). We start
# with a given collection of labeled examples. We first randomly partition the data
# into a training sample, a validation sample, and a test sample. The size of each of
# these samples depends on a number of different considerations. For example, the
# amount of data reserved for validation depends on the number of hyperparameters
# of the algorithm, which are represented here by the vector Θ. Also, when the
# labeled sample is relatively small, the amount of training data is often chosen to be
# larger than that of the test data since the learning performance directly depends
# on the training sample.
# Next, we associate relevant features to the examples. This is a critical step in
# the design of machine learning solutions. Useful features can effectively guide the
# learning algorithm, while poor or uninformative ones can be misleading. Although
# it is critical, to a large extent, the choice of the features is left to the user. This
# choice reflects the user’s prior knowledge about the learning task which in practice
# can have a dramatic effect on the performance results.
# Now, we use the features selected to train our learning algorithm A by tuning
# the values of its free parameters Θ (also called hyper parameters). For each value of
# these parameters, the algorithm selects a different hypothesis out of the hypothesis
# set. We choose the one resulting in the best performance on the validation sample
# (Θ0 ). Finally, using that hypothesis, we predict the labels of the examples in
# the test sample. The performance of the algorithm is evaluated by using the loss

classify <-
  c(
    idea = c("−1", "+1"),
    seems = c("−1", "+1"),
    by = c("y", "y0"),
    y = 6,
    y0 = 1
  )
classify

# function associated to the task, e.g., the zero-one loss in our spam detection task,
# to compare the predicted and true labels. Thus, the performance of an algorithm is
# of course evaluated based on its test error and not its error on the training sample.
# 1.5
# Learning scenarios
# We next briefly describe some common machine learning scenarios. These scenarios
# differ in the types of training data available to the learner, the order and method
# by which training data is received and the test data used to evaluate the learning
# algorithm.
# Supervised learning: The learner receives a set of labeled examples as training
# data and makes predictions for all unseen points. This is the most common sec-
#   n associated with classification, regression, and ranking problems. The spam
# detection problem discussed in the previous section is an instance of supervised
# learning.
# • Unsupervised learning: The learner exclusively receives unlabeled training data,
# and makes predictions for all unseen points. Since in general no labeled example
# is available in that setting, it can be difficult to quantitatively evaluate the per-
#   conformance of a learner. Clustering and dimensional reduction are example of
# unsupervised learning problems.
# • Semi-supervised learning: The learner receives a training sample consisting of
# both labeled and unlabeled data, and makes predictions for all unseen points.
# Semi-supervised learning is common in settings where unlabeled data is easily
# accessible but labels are expensive to obtain. Various types of problems arising in
# applications, including classification, regression, or ranking tasks, can be framed
# as instances of semi-supervised learning. The hope is that the distribution of
# unlabeled data accessible to the learner can help him achieve a better performance
# than in the supervised setting. The analysis of the conditions under which this can
# indeed be realized is the topic of much modern theoretical and applied machine
# learning research.
# • Transducer inference: As in the semi-supervised scenario, the learner receives
# a labeled training sample along with a set of unlabeled test points. However, the
# objective of transducer inference is to predict labels only for these particular
# test points. Transducer inference appears to be an easier task and matches
# the scenario encountered in a variety of modern applications. However, as in
# the semi-supervised setting, the assumptions under which a better performance
# can be achieved in this setting are research questions that have not been fully
# resolved.

# On-line learning: In contrast with the previous scenarios, the online scenario
# involves multiple rounds where training and testing phases are intermixed. At
# each round, the learner receives an unlabeled training point, makes a prediction,
# receives the true label, and incurs a loss. The objective in the on-line setting is to
# minimize the cumulative loss over all rounds or to minimize the regret, that is the
# difference of the cumulative loss incurred and that of the best expert in hindsight.
# Unlike the previous settings just discussed, no distributional assumption is made
# in on-line learning. In fact, instances and their labels may be chosen adversarial
# within this scenario.
# • Reinforcement learning: The training and testing phases are also intermixed in
# reinforcement learning. To collect information, the learner actively interacts with
# the environment and in some cases affects the environment, and receives an mill-
#   mediate reward for each action. The object of the learner is to maximize his
# reward over a course of actions and iterations with the environment. However,
# no long-term reward feedback is provided by the environment, and the learner
# is faced with the exploration versus exploitation dilemma, since he must choose
# between exploring unknown actions to gain more information versus exploiting
# the information already collected.
# • Active learning: The learner deceptively or interactively collects training examples,
# typically by querying an oracle to request labels for new points. The goal in
# active learning is to achieve a performance comparable to the standard supervised
# learning scenario (or passive learning scenario), but with fewer labeled examples.
# Active learning is often used in applications where labels are expensive to obtain,
# for example computational biology applications.
# •
# In practice, many other intermediate and somewhat more complex learning scenario-
#   bios may be encountered.

# 1.6
# Generalization
# Machine learning is fundamentally about generalization. As an example, the stein-
#   dart supervised learning scenario consists of using a finite sample of labeled exam-
#   oles to make accurate predictions about unseen examples. The problem is typically
# formulated as that of selecting a function out of a hypothesis set, that is a subset
# of the family of all functions. The function selected is subsequently used to label
# all instances, including unseen examples.
# How should a hypothesis set be chosen? With a rich or complex hypothesis set,
# the learner may choose a function or predictor that is consistent with the training
# sample, that is one that commits no error on the training sample. With a less com-
#   plea family, incurring some errors on the training sample may be unavoidable. But

# Figure 1.2
# The gig-rag line on the left panel is consistent over the blue and red training sample, but it is
# a complex separation surface that is not likely to generalize well to unseen data. In contrast,
# the decision surface on the right panel is simpler and might generalize better in spite of its
# classifications of a few points of the training sample.
# which will lead to a better generalization? How should we define the complexity of
# a hypothesis set?
#   Figure 1.2 illustrates these two types of solution: one is a gig-rag line that perfectly
# separates the two populations of blue and red points and that is chosen from a
# complex family; the other one is a smoother line chosen from a simpler family
# that only imperfectly discriminates between the two sets. We will see that, in
# general, the best predictor on the training sample may not be the best overall. A
# predictor chosen from a very complex family can essentially memorize the data, but
# generalization is distinct from the memorization of the training labels.
# We will see that the trade-off between the sample size and complexity plays a
# critical role in generalization. When the sample size is relatively small, choosing
# from a too complex a family may lead to poor generalization, which is also known
# as over fitting. On the other hand, with a too simple a family it may not be possible
# to achieve a sufficient accuracy, which is known as under fitting.
# In the next chapters, we will analyze more in detail the problem of generalization
# and will seek to derive theoretical guarantees for learning. This will depend on
# different notions of complexity that we will thoroughly discuss.

# 2
# The PAC Learning Framework
# Several fundamental questions arise when designing and analyzing algorithms that
# learn from examples: What can be learned efficiently? What is inherently hard
# to learn? How many examples are needed to learn successfully? Is there a gen-
#   real model of learning? In this chapter, we begin to formalize and address these
# questions by introducing the Probably Approximately Correct (PAC) learning frame-
#   work. The PAC framework helps define the class of learn able concepts in terms of
# the number of sample points needed to achieve an approximate solution, sample
# complexity, and the time and space complexity of the learning algorithm, which
# depends on the cost of the computational representation of the concepts.
# We first describe the PAC framework and illustrate it, then present some general
# learning guarantees within this framework when the hypothesis set used is finite,
# both for the consistent case where the hypothesis set used contains the concept to
# learn and for the opposite inconsistent case.

# 2.1
# The PAC learning model
# We first introduce several definitions and the notation needed to present the PAC
# model, which will also be used throughout much of this book.
# We denote by X the set of all possible examples or instances. X is also sometimes
# referred to as the input space. The set of all possible labels or target values is
# denoted by Y. For the purpose of this introductory chapter, we will limit ourselves
# to the case where Y is reduced to two labels, Y = {0, 1}, which corresponds to
# the so-called binary classification. Later chapters will extend these results to more
# general settings.
# A concept c : X → Y is a mapping from X to Y. Since Y = {0, 1}, we can identify
# c with the subset of X over which it takes the value 1. Thus, in the following,
# we equivalently refer to a concept to learn as a mapping from X to {0, 1}, or as a
# subset of X. As an example, a concept may be the set of points inside a triangle