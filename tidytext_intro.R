#' tidytext_intro.R
#'
#' @lachlandeer
#'
#'

# --- Libraries --- #

library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(textstem)
library(reshape2)
library(wordcloud)

# --- Download Data --- #

url <- "https://github.com/EmilHvitfeldt/smltar/raw/master/data/kickstarter.csv.gz"
# where to save data
out_file <- "data/kickstarter.csv.gz"
# download it!
download.file(url, destfile = out_file, mode = "wb")

# --- Load Data --- #

kickstarter <- read_csv('data/kickstarter.csv.gz')

# --- First, how many are successful? --- #

kickstarter %>%
    ggplot(aes(x = as.factor(state))) +
    geom_histogram(stat = 'count')

# --- How long are the blurbs? --- #
kickstarter <-
    kickstarter %>%
    mutate(n_char = nchar(blurb))

kickstarter %>%
    ggplot(aes(x = n_char)) +
    geom_histogram()

# OK so there is a weird discontinuity ...
# maybe a change in policy?

kickstarter %>%
    ggplot(aes(x = created_at, y = n_char)) +
    geom_bin2d()

# ok seems like a change aroun 2010?

kickstarter %>%
    filter(n_char > 135) %>%
    summarise(max(created_at))

# cool!

# --- Text Analytics --- #

# first, throw away small blurbs --> likely uninformative
kickstarter <- 
    kickstarter %>%
    filter(n_char >= 30)

# principles of text analysis in the tidytext framework ...

# convert to tokens
# first add a row id & sample the data to make faster to analyze...
kickstarter <-
    kickstarter %>%
    rownames_to_column("id") %>%
    slice_sample(prop = 0.2)

# unnest tokens... 
text_df <- 
    kickstarter %>%
    unnest_tokens(word, blurb)

# stop words ... 

text_df <-
    text_df %>%
    anti_join(stop_words)

# so whats the most common words?
common_words <- 
    text_df %>%
    group_by(state) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = 25)

common_words %>%
    ggplot(aes(x = n, y = reorder(word, n))) +
    geom_col() +
    facet_wrap(~state, scales = "free_y")

# custom stop words? 

my_stop_words <- tibble(
    word = c(
        'project'
    ),
    lexicon = 'kickstarter'
)

text_df <- 
    text_df %>%
    anti_join(my_stop_words)

# which words are used more or less 
# in success vs unsuccessful reviews?

text_df %>%
    count(word, state, sort = TRUE) %>%
    acast(word ~ state, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray60"),
                      max.words = 75)
    # commonality.cloud(colors = c("gray20"),
    #                   max.words = 75)

# tf-idf?
# The statistic tf-idf is intended to measure how 
# important a word is to a document in a collection 
# (or corpus) of documents

kck_words <- 
    text_df %>%
    group_by(state) %>%
    count(word, sort = TRUE)

kck_tf_idf <-
    kck_words %>%
    bind_tf_idf(word, state, n)

kck_tf_idf %>%
    group_by(state) %>%
    slice_max(tf_idf, n = 15) %>%
    ungroup() %>%
    ggplot(aes(tf_idf, reorder(word, tf_idf))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~state, ncol = 2, scales = "free") +
    labs(x = "tf-idf", y = NULL)
