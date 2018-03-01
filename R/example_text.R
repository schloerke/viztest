## Gather example files

# altered from pkgload::run_example
get_example_txt <- function (path, test = FALSE, run = FALSE) {
  if (!file.exists(path)) {
    stop("'", path, "' does not exist", call. = FALSE)
  }
  tmp <- textConnection("tmpVal", open = "w")
  on.exit({
    close(tmp)
  })
  if (getRversion() < "3.2") {
    tools::Rd2ex(path, out = tmp, commentDontrun = !run)
  }
  else {
    tools::Rd2ex(path, out = tmp, commentDontrun = !run, commentDonttest = !test)
  }
  ret <- textConnectionValue(tmp)
  if (length(ret) > 0) {
    ret <- paste(ret, collapse = "\n")
  } else {
    ret <- ""
  }
  invisible(ret)
}
# get_example_txt(devtools:::rd_files()[2]) %>% cat()
