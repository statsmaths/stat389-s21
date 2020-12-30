library(tidyverse)
library(reticulate)
library(cleanNLP)

use_virtualenv("../env", required = TRUE)
cnlp_init_spacy("en_core_web_sm")

z <- read_csv("../notebooks/data/food_page.csv")
token <- cnlp_annotate(z)$token
write_csv(token, "../notebooks/data/food_page_token.csv.gz")
