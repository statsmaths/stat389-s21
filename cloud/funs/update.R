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

  this_time <- as.character(Sys.time())
  if (length(downloaded))
  {
    message(sprintf("%s - Downloaded file '%s'.\n", this_time, downloaded))
  } else {
    message(sprintf("%s - Nothing new to download", this_time))
  }
}

.update_materials()
