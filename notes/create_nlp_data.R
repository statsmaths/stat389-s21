library(tidyverse)
library(reticulate)
library(cleanNLP)

use_virtualenv("../env", required = TRUE)
cnlp_init_spacy("en_core_web_sm")

z <- read_csv("../uploads/data/stylo_us.csv")
token <- cnlp_annotate(z)$token
write_csv(token, "../uploads/data/stylo_us_token.csv.gz")
