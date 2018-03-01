

#' Visually Test Package Examples
#'
#' Execute all package examples in independent knitr files to capture images
#' of each visual item produced, such as plots, htmlwidgets, shiny application objects.
#'
#' @export
#' @inheritParams viz_compare
#' @param pkg Local R package to load with devtools
#' @param old_pkg R package name description.  This should either be a CRAN or GitHub name to
#'        work with \code{devtools::\link[devtools]{install_cran}}
#'        or \code{devtools::\link[devtools]{install_github}} respectively.
#' @param output_dir Output directory. Defaults to "viztest-PKG-VERSION"
#' @param ... parameters sent to \code{\link{viz_compare}}
#' @param delay Amount of delay to use before capturing
#' @param fig.width,fig.height Figure width and height in inches
#' @param vwidth,vheight Screenshot size in pixels
#' @param test If \code{TRUE}, code in \code{\\donttest{}} will be commented out.
#'             If \code{FALSE}, code in \code{\\testonly{}} will be commented out.
#' @param run If \code{TRUE}, code in \code{\\dontrun{}} will be commented out.
#' @param stomp If \code{TRUE}, allows \code{viztest} to reexecute in an existing output directory
#' @param cache If \code{TRUE}, the local R package examples will be cached with knitr for faster
#'        execution.  The \code{old_pkg} will always be cached as it's source code does not change.
#' @param save_individual If \code{TRUE}, individual example knitr files will be saved
#' @importFrom utils packageVersion
#' @examples
#' \dontrun{
#' # R session running in local R package folder
#' viztest(stomp = TRUE)
#' }
viztest <- function(
  pkg = ".",
  old_pkg = paste0(devtools::as.package(pkg)$package),
  output_dir = file.path(
    # viztest-leaflet-2.0.0
    paste(
      "viztest", devtools::as.package(pkg)$package, devtools::as.package(pkg)$version,
      sep = "-"
    )
  ),
  ...,
  delay = 2,
  fig.width = 8,
  fig.height = 6,
  vwidth = 992,
  vheight = 744,
  test = TRUE, run = FALSE,
  stomp = FALSE,
  cache = TRUE,
  save_individual = TRUE,
  resize = TRUE,
  browse = TRUE
) {

  # remove and create output dir
  if (dir.exists(output_dir)) {
    if (!isTRUE(stomp)) {
      stop(
        "Can not save viztest results if the output directory still exists:'", output_dir, "'. \n", # nolint
        "Or use `stomp != TRUE`"
      )
    }
    # message("Deleting folder: ", output_dir)
    # unlink(output_dir, recursive = TRUE)
  }

  # make sure it's the latest docs
  pkg <- devtools::as.package(pkg)
  devtools::document(pkg)

  # install locally
  cran_dir <- file.path(output_dir, rel_cran_dir)
  local_dir <- file.path(output_dir, rel_local_dir)
  cran_lib_dir <- file.path(cran_dir, rel_lib_dir)
  local_lib_dir <- file.path(local_dir, rel_lib_dir)
  lapply(
    c(cran_lib_dir,local_lib_dir),
    dir.create,
    recursive = TRUE, showWarnings = FALSE
  )
if(FALSE) {
  message("\n\nInstalling old version")
  withr::with_libpaths(cran_lib_dir, action = "prefix", {
    if (grepl("/", old_pkg, fixed = TRUE)) {
      devtools::install_github(old_pkg)
    } else {
      devtools::install_cran(old_pkg)
    }
    # devtools::install_cran(pkg$package)
    # devtools::install_github(pkg$package)
    # pkgman::pkg_install(old_pkg)
  })
  message("\n\nInstalling new, local version")
  withr::with_libpaths(local_lib_dir, action = "prefix", {
    devtools::install(pkg)
  })
}

  # get all the doc files from the local pkg
  devtools_rd_files <- utils::getFromNamespace("rd_files", "devtools")
  rd_files <- devtools_rd_files(pkg) %>% head()

  if (length(rd_files) == 0) {
    message("No .Rd files found")
    return(invisible(TRUE))
  }
  message("Gathering ", length(rd_files), " examples")

  rd_txts <- lapply(rd_files, get_example_txt, test = test, run = run)
  rd_names <- gsub(".Rd", "", names(rd_txts), fixed = TRUE)

  knit_examples <- function(name, example_dir, ..., cache = FALSE) {

    make_knitr_head_txt <- function(file) {
      knitr_head_txt <- paste0(
        # nolint start
"---
title: \"", pkg$package, " viztest - ", file, "\"
date: \"", format(Sys.Date(), "%m/%d/%Y"), "\"
---

```{r, eval = FALSE, include = FALSE}
# command to compile Rmd
withr::with_dir(\"", normalizePath("."), "\", {
  withr::with_libpaths(
    \"", normalizePath(rel_lib_dir), "\",
    action = \"prefix\",
    {
      knitr::knit(\"FILE.Rmd\")
    }
  )
})
```

```{r _knitr_setup, include = FALSE }
library(knitr)
opts_chunk$set(
  cache = ", cache, ",
  cache.path = \"", rel_cache_dir, .Platform$file.sep, "\",
  fig.path = \"", rel_images_dir, .Platform$file.sep, "\",
  fig.width = ", fig.width, ",
  fig.height = ", fig.height, ",
  screenshot.opts = list(vwidth = ", vwidth, ", vheight = ", vheight, ", delay = ", delay, ")
)
set.seed(", 66 + 97 + 144 + 144 + 101 + 116, ")
```
```{r}
library(", pkg$package, ")
packageVersion(\"", pkg$package, "\")
```

"
# 66 + 97 + 144 + 144 + 101 + 116 = "B a r r e t" in ascii
  # nolint end
      )
      knitr_head_txt
    }

    # detach pkg
    detach_package(pkg$package)

    # run each rmd example independently
    pb <- progress::progress_bar$new(
      total = length(rd_txts),
      format = paste0(":current/:total ellapsed::elapsed eta::eta [:bar] :rd_name"),
      show_after = 0,
      clear = FALSE
    )
    pb$tick(0)

    withr::with_dir(example_dir, {
      withr::with_libpaths(rel_lib_dir, action = "prefix", {

        purrr::map2(rd_names, rd_txts, function(rd_name, rd_txt) {
          knitr_head_txt <- make_knitr_head_txt(rd_name)
          knitr_txt <- paste0(
            knitr_head_txt,
            "# ", rd_name, "\n",
            "```{r ", rd_name, " }\n",
            "### Package: ", name, "\n",
            rd_txt, "\n",
            "```"
          )

          pb$tick(tokens = list(rd_name = rd_name))
          if (isTRUE(save_individual)) {
            save_file <- paste0(rd_name, ".Rmd")
            cat(knitr_txt, file = save_file)
            knitr::knit(save_file, quiet = TRUE)
          } else {
            knitr::knit(text = knitr_txt, quiet = TRUE)
          }
        })
      })
    })
  }

  message("\nRunning CRAN library on local examples")
  knit_examples(old_pkg, cran_dir, cache = TRUE)

  message("\nRunning local library on local examples")
  knit_examples(paste0(pkg$package, "-", pkg$version), local_dir, cache = cache)

  # message(length(dir(file.path(output_dir, "images"))), " files in ", output_dir)
  # cat("\n")
  viz_compare(output_dir, resize = resize, browse = browse)
}






detach_package <- function(pkg_name) {
  pkg <- paste0("package:", pkg_name)
  while(pkg %in% search()) {
    message("Detaching package: ", pkg)
    detach(pkg, unload = TRUE, character.only = TRUE)
  }
  TRUE
}
