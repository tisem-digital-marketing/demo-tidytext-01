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
