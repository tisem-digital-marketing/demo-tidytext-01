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

# --- What is in the data ? --- #

# How many are successful? 

kickstarter %>%
    ggplot(aes(x = as.factor(state))) +
    geom_bar() +
    ylab("Number of projects") +
    xlab("Project Success? (1 = Successful)") +
    scale_y_continuous(breaks= seq(0, 150000, 25000))

kickstarter %>%
    group_by(state) %>%
    count()

# What can we say about the text?

# for kickstarter, the limit was number of characters

kickstarter <-
    kickstarter %>%
    mutate(n_char = nchar(blurb)) 

# plot the distribution of reviews?

kickstarter %>%
    ggplot(aes(x = n_char)) +
    geom_histogram()

# Ok weird discontinuity ... why?

kickstarter %>%
    ggplot(aes(x = created_at, y = n_char)) +
    geom_bin2d() +
    facet_wrap(~state)

# so there seems to be a change in the character limit in 2010..

# --- Text Analytics --- #
set.seed(1234567890)

kickstarter_sample <- 
    kickstarter %>% 
    slice_sample(prop = 0.25)

kickstarter_sample <-
    kickstarter_sample %>%
    rownames_to_column("campaign_id")

tidy_df <- 
    kickstarter_sample %>%
    unnest_tokens(word, blurb) 

# remove stopwords 
tidy_df <-
    tidy_df %>%
    anti_join(stop_words)

common_words <- 
    tidy_df %>%
    count(word, sort = TRUE)

my_stop_words <- 
    tibble(
        word = c(
            'project'
        ),
        lexicon = 'kickstarter'
    )

# remove the custom stopword

tidy_df <-
    tidy_df %>%
    anti_join(my_stop_words)

common_words <- 
    tidy_df %>%
    group_by(state) %>%
    count(word, sort = TRUE) %>%
    pivot_wider(names_from = state, values_from = n)

# word clouds 

# comparison 
tidy_df %>%
    count(word, state, sort = TRUE) %>% 
    acast(word ~ state, value.var = 'n', fill = 0) %>%
    comparison.cloud(max.words = 75, colors = c("gray20", "gray70"))

# commonalities
tidy_df %>%
    count(word, state, sort = TRUE) %>% 
    acast(word ~ state, value.var = 'n', fill = 0) %>%
    commonality.cloud(max.words = 75, colors = "gray20")

