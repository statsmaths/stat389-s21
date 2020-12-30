#' Checks available materials from GitHub and downloads those that do not
#' currently exist locally.
.update_materials <- function()
{
  base <- "https://raw.githubusercontent.com/statsmaths/stat389-s21/main/uploads/"

  # what files are available to download?
  avail <- readLines(file.path(base, "manifest.txt"))
  avail <- avail[avail != ""]

  # download files that do not exist
  downloaded <- c()
  for (f in avail)
  {
    if (!file.exists(f))
    {
      url <- file.path(base, f)
      download.file(url, f, quiet = TRUE)
      downloaded <- c(downloaded, f)
    }
  }

  if (length(downloaded))
  {
    message(sprintf("Downloaded file %s\n", downloaded))
  } else {
    message("Nothing new to download.")
  }
}

.update_materials()
