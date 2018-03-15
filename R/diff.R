
#' Compare two output folders
#'
#' @param output_dir Output folder created by \code{viztest}
#' @param resize \code{resize} parameter supplied to \code{webshot::\link[webshot]{resize}}.
#'        Defaults (\code{TRUE}) to shrink each dimension to 50%. Shrinking will not alter
#'        the original files.
#' @param browse If \code{TRUE}, diff.html in \code{output_dir} will be opened
#' @export
#' @importFrom utils browseURL
#' @importFrom stats runif
viz_compare <- function(output_dir, resize = TRUE, browse = TRUE) {
  withr::with_dir(output_dir, {
    find_img_dir <- function(dir) {
      for (img_dir in c(
        paste0(rel_images_dir, "_", "markdown_strict"),
        paste0(rel_images_dir)
      )) {
        img_path <- file.path(dir, img_dir)
        if (dir.exists(img_path)) {
          return(img_path)
        }
      }
      stop("Could not find output image dir")
    }
    cran_images <- find_img_dir(rel_cran_dir)
    local_images <- find_img_dir(rel_local_dir)

    if (!(is.null(resize) || identical(resize, FALSE))) {
      if (isTRUE(resize)) {
        resize <- "50%"
      }
      tmpdir <- file.path(tempdir(), paste0("viztest-", gsub("0.", "", runif(1))))
      on.exit({
        unlink(tmpdir, recursive = TRUE)
      })
      cran_images <- resize_images(cran_images, rel_cran_dir, tmpdir, resize)
      local_images <- resize_images(local_images, rel_local_dir, tmpdir, resize)
    }

    ret <- shinytest::diffviewer_widget(cran_images, local_images)
    # message("Saving diff.html output")
    # if selfcontained is TRUE, it takes fooorrreeeeevvvveeerrr
    htmlwidgets::saveWidget(ret, "diff.html", selfcontained = FALSE)

    if(isTRUE(browse)) {
      browseURL("diff.html")
    }

    invisible(ret)
  })
}
