

as_pkg <- function(...) {
  devtools::as.package(...)
}
pkg_name <- function(pkg) {
  as_pkg(pkg)$package
}
pkg_version <- function(pkg) {
  as_pkg(pkg)$version
}




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
#' @param stomp If \code{TRUE}, allows \code{viztest} to reexecute in an existing output
#'        directory
#' @param skip_old If \code{TRUE}, the old folder will not be deleted and
#'        the old package will not be executed
#' @param cache If \code{TRUE}, the local R package examples will be cached with knitr
#'        for faster execution.
#' @param document If \code{TRUE}, the local package will be documented before the
#'        examples are extracted
#' @param save_individual If \code{TRUE}, individual example knitr files will be saved
#' @importFrom utils packageVersion
#' @examples
#' \dontrun{
#' # R session running in local R package folder
#' viztest(stomp = TRUE)
#' }
viztest <- function(
  pkg = ".",
  old_pkg = pkg_name(pkg),
  output_dir = paste(
    "viztest", pkg_name(pkg), pkg_version(pkg),
    sep = "-"
  ),
  ...,
  delay = 2,
  fig.width = 8,
  fig.height = 6,
  vwidth = 992,
  vheight = 744,
  test = TRUE, run = FALSE,
  stomp = FALSE,
  skip_old = FALSE,
  cache = FALSE,
  document = TRUE,
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
  }

  # make sure it's the latest docs
  pkg <- as_pkg(pkg)

  if (isTRUE(document)) {
    callr_document(pkg)
  }

  # install locally
  cran_dir <- file.path(output_dir, rel_cran_dir)
  local_dir <- file.path(output_dir, rel_local_dir)
  remotes_lib_dir <- file.path(output_dir, rel_remotes_dir)
  cran_lib_dir <- file.path(cran_dir, rel_lib_dir)
  local_lib_dir <- file.path(local_dir, rel_lib_dir)

  # get all the doc files from the local pkg
  devtools_rd_files <- utils::getFromNamespace("rd_files", "devtools")
  rd_files <- devtools_rd_files(pkg) # %>% head()

  if (length(rd_files) == 0) {
    message("No .Rd files found")
    return(invisible(TRUE))
  }
  message("Gathering ", length(rd_files), " examples")

  rd_txts <- lapply(rd_files, get_example_txt, test = test, run = run)
  rd_names <- gsub(".Rd", "", names(rd_txts), fixed = TRUE)

  knit_examples <- function(name, example_dir, ..., cache = FALSE) {

    make_knitr_head_txt <- function(file) {
      glue::glue(
'---
title: "<< pkg$package >> viztest - << file >>"
date: "<< format(Sys.Date(), "%m/%d/%Y") >>"
output:
  md_document:
    variant: markdown_strict
  html_document: default
---

```{r, eval = FALSE, include = FALSE}
# command to compile Rmd
withr::with_dir("<< normalizePath(".") >>", {
  withr::with_libpaths(
    "<< normalizePath(rel_lib_dir) >>",
    action = "prefix",
    {
      rmarkdown::render("<< file >>.Rmd", output_format = "all")
    }
  )
})
```

```{r _knitr_setup, include = FALSE, cache = FALSE }
library(knitr)
cache_folder <- "<< rel_cache_dir >>"
images_folder <- "<< rel_images_dir >>"
pandoc_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
if (!is.null(pandoc_to)) {
  cache_folder <- paste0(cache_folder, "_", pandoc_to)
  images_folder <- paste0(images_folder, "_", pandoc_to)
}
cache_folder <- paste0(cache_folder, .Platform$file.sep)
images_folder <- paste0(images_folder, .Platform$file.sep)
opts_chunk$set(
  cache = << cache >>,
  cache.path = cache_folder,
  fig.path = images_folder,
  fig.width = << fig.width >>,
  fig.height = << fig.height >>,
  screenshot.opts = list(
    vwidth = << vwidth >>,
    vheight = << vheight >>,
    delay = << delay >>
  )
)
set.seed(<< 66 + 97 + 144 + 144 + 101 + 116 >>)
```
```{r}
library(<< pkg_name(pkg) >>)
packageVersion("<< pkg_name(pkg) >>")
```

',
# 66 + 97 + 144 + 144 + 101 + 116 = "B a r r e t" in ascii
  # nolint end
        .open = "<<",
        .close = ">>"
      )
    }

    # run each rmd example independently
    withr::with_dir(example_dir, {
      withr::with_libpaths(rel_lib_dir, action = "prefix", {

        render_infos <- purrr::map2(rd_names, rd_txts, function(rd_name, rd_txt) {
          knitr_head_txt <- make_knitr_head_txt(rd_name)
          knitr_txt <- paste0(
            knitr_head_txt,
            "# ", rd_name, "\n",
            "```{r ", rd_name, " }\n",
            "### Package: ", name, "\n",
            rd_txt, "\n",
            "```"
          )
          if (isTRUE(save_individual)) {
            save_file <- paste0(rd_name, ".Rmd")
            cat(knitr_txt, file = save_file)
            save_file
          } else {
            knitr_txt
          }
        })
        render_infos <- unlist(render_infos)

        callr_render_infos(rd_names, render_infos, isTRUE(save_individual))
        invisible()
      })
    })
  }

  dir.create(remotes_lib_dir, recursive = TRUE, showWarnings = FALSE)
  withr::with_libpaths(remotes_lib_dir, {
    devtools::install_cran("remotes", reload = FALSE)
    devtools::install_cran("callr", reload = FALSE)
    devtools::install_github("r-lib/crancache", reload = FALSE)
  })

  if (isTRUE(skip_old)) {
    message("\nSkipping old version")
  } else {
    message("Deleting 'old' folder")
    unlink(cran_dir, recursive = TRUE)

    dir.create(cran_lib_dir, recursive = TRUE, showWarnings = FALSE)
    message("\n\nInstalling old version")
    withr::with_libpaths(c(cran_lib_dir, remotes_lib_dir), action = "replace", {
      if (grepl("/", old_pkg, fixed = TRUE)) {
        callr_install(old_pkg, "github")
      } else {
        callr_install(old_pkg, "cran")
      }
      # pkgman::pkg_install(old_pkg)
    })
    message("\nRunning old version on new examples")
    knit_examples(old_pkg, cran_dir, cache = cache)
  }



  message("\nDeleting 'new' folder")
  unlink(local_dir, recursive = TRUE)
  message("\nInstalling new, local version")
  dir.create(local_lib_dir, recursive = TRUE, showWarnings = FALSE)
  withr::with_libpaths(c(local_lib_dir, remotes_lib_dir), action = "replace", {
    callr_install(pkg$path, location = "local")
  })
  message("\nRunning new version on new examples")
  knit_examples(paste0(pkg_name(pkg), "-", pkg_version(pkg)), local_dir, cache = cache)

  viz_compare(output_dir, resize = resize, browse = browse)
}






# detach_package <- function(pkg_name) {
#   pkg <- paste0("package:", pkg_name)
#   while(pkg %in% search()) {
#     message("Detaching package: ", pkg)
#     detach(pkg, unload = TRUE, character.only = TRUE)
#   }
#   TRUE
# }
