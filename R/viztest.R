## Gather example files

# altered from pkgload::run_example
get_example_txt <- function (path, test = FALSE, run = FALSE) {
  if (!file.exists(path)) {
    stop("'", path, "' does not exist", call. = FALSE)
  }
  # tmp <- tempfile(fileext = ".R")
  # tmp <- tempfile(fileext = ".R")
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


#' Visual Test Package Examples
#'
#' Execute all package examples in independent knitr files to capture images of each visual item produced, such as plots, htmlwidgets, shiny application objects.
#'
#' @export
#' @param pkg Package to load with devtools
#' @param output_dir Save directory. Defaults to "viztest/PKG-VERSION"
#' @param ... ignored
#' @param delay amount of delay to use before capturing
#' @param fig.width,fig.height Figure width and height in inches
#' @param vwidth,vheight Screenshot size in pixels
#' @param test If \code{TRUE}, code in \code{\\donttest{}} will be commented out.
#'             If \code{FALSE}, code in \code{\\testonly{}} will be commented out.
#' @param run If \code{TRUE}, code in \code{\\dontrun{}} will be commented out.
#' @param stomp If \code{TRUE}, the prior output folder will be completely deleted before executing
#' @param save_individual If \code{TRUE}, the individual knitr files used to produce the images will be saved in the output folder
#' @examples
#' \dontrun{
#' # R session running in local R package folder
#' viztest(stomp = TRUE)
#' }
viztest <- function(
  pkg = ".",
  output_dir = file.path(
    "viztest",
    paste0(devtools::as.package(pkg)$package, "-", devtools::as.package(pkg)$version)
  ),
  ...,
  delay = 2,
  fig.width = 8,
  fig.height = 6,
  vwidth = 992,
  vheight = 744,
  test = TRUE, run = FALSE,
  stomp = FALSE,
  save_individual = FALSE
) {

  # remove and create output dir
  if (dir.exists(output_dir)) {
    if (!isTRUE(stomp)) {
      stop(
        "Can not save viztest results if the output directory still exists:'", output_dir, "'. \n", # nolint
        "Or use `stomp != TRUE`"
      )
    }
    message("Deleting folder: ", output_dir)
    # unlink(output_dir, recursive = TRUE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # make sure it's the latest docs
  pkg <- devtools::as.package(pkg)
  devtools::document(pkg)

  # get all the doc files from the local pkg
  devtools_rd_files <- utils::getFromNamespace("rd_files", "devtools")
  rd_files <- devtools_rd_files(pkg)

  if (length(rd_files) == 0) {
    message("No .Rd files found")
    return(invisible(TRUE))
  }
  message("Gathering ", length(rd_files), " examples")

  rd_txts <- lapply(rd_files, get_example_txt, test = test, run = run)
  rd_names <- gsub(".Rd", "", names(rd_txts), fixed = TRUE)

  example_txts <- paste0(
    "# ", rd_names, "\n",
    "```{r ", rd_names, " }\n",
    unlist(rd_txts),
    "\n```"
  )

  knitr_head_txt <- paste0(
    # nolint start
"---
title: \"", pkg$package, " viztest\"
author: \"", pkg$maintainer, "\"
date: \"", format(Sys.Date(), "%m/%d/%Y"), "\"
---

```{r _knitr_setup, include = FALSE }
library(knitr)
opts_chunk$set(
  fig.path = \"", output_dir, .Platform$file.sep, "images", .Platform$file.sep, "\",
  fig.width = ", fig.width, ",
  fig.height = ", fig.height, ",
  screenshot.opts = list(vwidth = ", vwidth, ", vheight = ", vheight, ", delay = ", delay, ")
)
library(", pkg$package, ")
```

"
# nolint end
  )
  knitr_txts <- paste0(knitr_head_txt, example_txts)

  # save a global Rmd file
  cat(
    paste0(knitr_head_txt, paste0(example_txts, collapse = "\n\n")),
    file = file.path(output_dir, "_examples.Rmd")
  )

  # run each rmd example independently
  pb <- progress::progress_bar$new(
    total = length(knitr_txts),
    format = ":current/:total ellapsed::elapsed eta::eta [:bar] :name_val",
    show_after = 0,
    clear = FALSE
  )
  pb$tick(0)

  lapply(seq_along(rd_names), function(i) {
    if (isTRUE(save_individual)) {
      cat(knitr_txts[[i]], file = file.path(output_dir, paste0(rd_names[[i]], ".Rmd")))
    }
    pb$tick(tokens = list(name_val = rd_names[[i]]))
    ret <- knitr::knit(text = knitr_txts[[i]], quiet = TRUE)
    ret
  })

  message("produced ", length(dir(file.path(output_dir, "images"))), " example images")
  cat("\n")
  invisible()
}
