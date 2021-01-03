library(tidyverse)
library(reticulate)
library(cleanNLP)

use_virtualenv("../env", required = TRUE)
cnlp_init_spacy("en_core_web_sm")

z <- read_csv("../notebooks/data/movie_plot.csv.gz")
token <- cnlp_annotate(z)$token
write_csv(token, "../notebooks/data/movie_plot_token.csv.gz")
