source("wiki.R")

pages <- wiki_get_pages(c("Café"), lang = "fr")
z <- wiki_expand_pages(pages)
wiki <- wiki_get_pages_text(z)

library(tidyverse)
library(reticulate)
library(cleanNLP)

wiki <- tapply(wiki$text, wiki$page, paste, collapse = " ")
wiki <- tibble(doc_id = names(wiki), text = as.character(wiki))

use_virtualenv("/Users/admin/gh/stat389-s21/env", required = TRUE)
#cnlp_init_spacy("en_core_web_sm")
cnlp_init_spacy("fr_core_news_sm")
token <- cnlp_annotate(wiki)$token




library(topicmodels)

X <- cnlp_utils_tf(
  filter(token, upos %in% c("NOUN", "ADJ", "ADV")),
  min_df = 0.001, max_df = 0.5
)
lda_model <- LDA(x = X, k = 16, control = list(seed = 2811, verbose = 1))

terms <- lda_model@terms
beta <- lda_model@beta
gamma <- lda_model@gamma
top_k <- get_terms(lda_model, k = 5)
topics <- tibble(
  doc_id = rep(wiki$doc_id, ncol(gamma)),
  topic_num = as.numeric(col(gamma)),
  prob = as.numeric(gamma)
)
topics$topic_name <- apply(top_k, 2, paste, collapse = "; ")[topics$topic_num]
topics <- left_join(topics, wiki, by = "doc_id")

topics %>%
  filter(prob > 0.1) %>%
  group_by(topic_name) %>%
  arrange(desc(prob)) %>%
  slice_head(n = 10) %>%
  print(n = Inf)

temp <- filter(topics, prob > 0.1)
temp <- mutate(temp, topic_name = fct_inorder(topic_name))
temp <- arrange(temp, desc(prob))
split(temp$label, temp$topic_name)
