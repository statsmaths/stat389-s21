library(jsonlite)
library(tidyverse)

topics_create_json <- function(topic_docs, topic_terms, meta, truncate = -1)
{

  tnames <- topic_terms %>%
    arrange(topic, desc(beta)) %>%
    group_by(topic) %>%
    slice_head(n = 5) %>%
    group_by(topic) %>%
    summarize(name = paste(token, collapse = "; "))

  topic_words <- topic_terms %>%
    arrange(topic, desc(beta)) %>%
    group_by(topic) %>%
    slice_head(n = 100) %>%
    mutate(weight = round(100 * exp(beta) / max(exp(beta)))) %>%
    filter(weight > 0) %>%
    ungroup()

  topic_weights <- topic_docs %>%
    group_by(topic) %>%
    summarize(proportion = sum(prob)) %>%
    mutate(proportion = proportion / sum(proportion) * 100)

  top_docs <- topic_docs %>%
    arrange(topic, desc(prob)) %>%
    group_by(topic) %>%
    filter(prob > 0) %>%
    mutate(prob = round(prob * 100)) %>%
    filter(prob > 0) %>%
    ungroup()

  dset <- sort(unique(topic_docs$doc_id))
  top_docs$id <- match(top_docs$doc_id, dset) - 1L

  tset <- sort(unique(topic_terms$topic))
  topics <- vector("list", length(tset))
  for (j in seq_along(topics))
  {
    topics[[j]] <- list(
      "short" = unbox(sprintf("Cluster %d", j)),
      "long" = unbox(sprintf("Cluster %d: %s", j, tnames$name[tnames$topic == tset[j]])),
      "proportion" = unbox(round(topic_weights$proportion[topic_weights$topic == tset[j]])),
      "top_docs_ids" = top_docs$id[top_docs$topic == tset[j]],
      "doc_perc" = top_docs$prob[top_docs$topic == tset[j]],
      "top_word" = topic_words$token[topic_words$topic == tset[j]],
      "word_wgt" = topic_words$weight[topic_words$topic == tset[j]]
    )
  }

  top_topics <- topic_docs %>%
    arrange(doc_id, desc(prob)) %>%
    group_by(doc_id) %>%
    filter(prob > 0) %>%
    mutate(prob = round(prob * 100)) %>%
    filter(prob > 0) %>%
    ungroup()

  top_topics$id <- match(top_topics$topic, tset) - 1L

  docs <- vector("list", length(dset))
  if (truncate > 0) { meta$text <- stringi::stri_sub(meta$text, 1, truncate) }
  for (j in seq_along(docs))
  {
    docs[[j]] <- list(
      "top_topics_ids" = top_topics$id[top_topics$doc_id == dset[j]],
      "topic_weights" = top_topics$prob[top_topics$doc_id == dset[j]],
      "title" = unbox(dset[j]),
      "text" = meta$text[meta$doc_id == dset[j]],
      "meta" = list()
    )
  }

  output <- list(topics = topics, docs = docs)
  return(output)
}
