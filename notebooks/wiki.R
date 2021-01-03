# Title: Functions for Downloading Wikipedia Page Texts
# Author: Taylor Arnold
# Date: 01 January 2021

# This contains in-memory versions of our two datasets
.volatiles <- new.env()

# Download's a Wikipedia page for a given language of Wikipedia. This
# function follows redirects. It returns a named vector of three components:
# the display title, the page text, and a collapsed set of the page links
.download_wiki_page <- function(page, lang = "en")
{
  base_url <- sprintf("https://%s.wikipedia.org/w/api.php", lang)
  options <- "?action=parse&format=json&redirects&page="
  src_url <- paste0(base_url, options, URLencode(page))
  obj_json <- jsonlite::read_json(src_url)

  title <- obj_json$parse$title
  links <- obj_json$parse$links
  text <- obj_json$parse$text[["*"]]

  title <- stringi::stri_replace_all(title, "_", fixed = " ")
  ok_links <- ((sapply(links, getElement, "ns") == 0) &
               (sapply(lapply(links, getElement, "exists"), length) == 1))
  links <- sapply(links[ok_links], getElement, "*")
  links <- stringi::stri_replace_all(links, "_", fixed = " ")
  links <- paste(links, collapse = " ")

  message(sprintf("Downloading %s", page))

  return(list(title = title, text = text, links = links))
}

# Load the two datasets into memory
.load_wiki_data <- function()
{
  if (!file.exists(wpath <- file.path("data", "wiki_src.csv.gz")))
  {
    dt <- tibble::tibble(
      page = character(), lang = character(),
      text = character(), links = character()
    )
    readr::write_csv(dt, wpath)
  }
  if (!file.exists(lpath <- file.path("data", "wiki_lookup.csv.gz")))
  {
    dt <- tibble::tibble(
      query = character(), lang = character(), page = character()
    )
    readr::write_csv(dt, lpath)
  }

  .volatiles$wiki_text <- readr::read_csv(wpath, col_types = "cccc")
  .volatiles$wiki_look <- readr::read_csv(lpath, col_types = "ccc")
}

.save_wiki_data <- function()
{
  wpath <- file.path("data", "wiki_src.csv.gz")
  readr::write_csv(.volatiles$wiki_text, wpath)

  lpath <- file.path("data", "wiki_lookup.csv.gz")
  readr::write_csv(.volatiles$wiki_look, lpath)
}

wiki_get_pages <- function(query, lang)
{
  if (is.null(.volatiles$wiki_text)) { .load_wiki_data() }

  query <- unique(query)
  df_search <- tibble::tibble(query = query, lang = lang)
  df_need <- dplyr::anti_join(
    df_search, .volatiles$wiki_look, by = c("query", "lang")
  )

  if (n <- nrow(df_need))
  {
    df_need$page <- ""
    df_need$text <- ""
    df_need$links <- ""

    for (i in seq_len(nrow(df_need)))
    {
      new <- .download_wiki_page(df_need$query[i], lang)
      df_need$page[i] <- new$title
      df_need$text[i] <- new$text
      df_need$links[i] <- new$links
    }

    .volatiles$wiki_look <- dplyr::bind_rows(
      .volatiles$wiki_look, dplyr::select(df_need, query, lang, page)
    )

    add_these <- dplyr::anti_join(
      dplyr::select(df_need, page, lang, text, links),
      .volatiles$wiki_text,
      by = c("page", "lang")
    )
    .volatiles$wiki_text <- dplyr::bind_rows(
      .volatiles$wiki_text, add_these
    )

    .save_wiki_data()
  }

  df_search <- dplyr::inner_join(
    df_search, .volatiles$wiki_look, by = c("lang", "query")
  )
  df_search <- dplyr::inner_join(
    df_search, .volatiles$wiki_text, by = c("lang", "page")
  )
  df_search <- dplyr::select(df_search, page, lang, text, links)
  df_search <- dplyr::arrange(df_search, page)

  return(df_search)
}

wiki_expand_pages <- function(input_data)
{
  lang <- unique(input_data$lang)
  if (length(lang) > 1) { stop("Can only expand pages in one language.") }
  all_links <- unique(unlist(stringi::stri_split(
    input_data$links, fixed = " "
  )))
  output <- dplyr::bind_rows(input_data, wiki_get_pages(all_links, lang = lang))
  output <- unique(output)
  output
}

wiki_get_pages_text <- function(input_data)
{
  all_text <- vector("list", nrow(input_data))
  for (j in seq_len(nrow(input_data)))
  {
    obj_html <- xml2::read_html(input_data$text[j])
    refs <- xml2::xml_find_all(obj_html, ".//sup/a")
    xml2::xml_text(refs) <- ""
    probs <- xml2::xml_find_all(obj_html, ".//span[not(@class)]")
    xml2::xml_text(probs) <- ""

    inst <- xml2::xml_find_all(
      obj_html, ".//*[self::p or self::h2[not(@id)] or self::h3]"
    )
    tags <- xml2::xml_name(inst)
    text <- stringi::stri_replace_all(
      xml2::xml_text(inst), "", regex = "\\[[\\W\\w]+\\]"
    )

    p <- rep(NA_character_, length(tags))
    h2 <- rep(NA_character_, length(tags))
    p[tags == "p"] <- text[tags == "p"]
    sec_num <- rep(NA_integer_, length(h2))
    h2[tags == "h2"] <- text[tags == "h2"]
    sec_num[!is.na(h2)] <- seq_len(sum(!is.na(h2)))
    h2[!is.na(h2)] <- sprintf(
      "%d. %s", seq_len(sum(!is.na(h2))), h2[!is.na(h2)]
    )

    dt <- tibble::tibble(
      page = input_data$page[j], sec_num = sec_num, section = h2, text = p
    )
    dt <- tidyr::fill(dt, sec_num, section)
    dt <- dt[!is.na(dt$text),]
    dt <- dt[stringi::stri_length(dt$text) > 25,]
    dt$sec_num[is.na(dt$section)] <- 0
    dt$section[is.na(dt$section)] <- "0. Lead"
    all_text[[j]] <- dt
  }
  dplyr::bind_rows(all_text)
}
