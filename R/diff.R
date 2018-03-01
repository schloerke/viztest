
#' Compare two output folders
#'
#' @param output_dir Output folder created by \code{viztest}
#' @param resize \code{resize} parameter supplied to \code{webshot::\link[webshot]{resize}}.
#'        Defaults (\code{TRUE}) to shrink each dimension to 50%. Shrinking will not alter
#'        the original files.
#' @param browse If \code{TRUE}, diff.html in \code{output_dir} will be opened
#' @export
#' @importFrom utils browseURL
viz_compare <- function(output_dir, resize = TRUE, browse = TRUE) {
  withr::with_dir(output_dir, {
    cran_images <- file.path(rel_cran_dir, rel_images_dir)
    local_images <- file.path(rel_local_dir, rel_images_dir)

    if (!(is.null(resize) || identical(resize, FALSE))) {
      if (isTRUE(resize)) {
        resize <- "50%"
      }
      tmpdir <- file.path(tempdir(), paste0("viztest-", gsub("0.", "", runif(1))))
      on.exit({
        unlink(tmpdir, recursive = TRUE)
      })
      resize_images <- function(from_folder, rel_dir) {
        to_folder <- file.path(tmpdir, paste0(rel_dir, "_images"))
        dir.create(to_folder, recursive = TRUE)
        from_files <- dir(from_folder, full.names = TRUE)
        file.copy(
          from_files,
          to_folder,
          recursive = TRUE
        )
        message("Shrinking ", from_folder)
        pb <- progress::progress_bar$new(
          total = length(from_files),
          format = paste0(":current/:total ellapsed::elapsed eta::eta [:bar] :to_file"),
          show_after = 0,
          clear = FALSE
        )
        pb$tick(0)

        lapply(
          dir(to_folder, full.names = TRUE),
          function(to_file) {
            pb$tick(tokens = list(to_file = basename(to_file)))
            webshot::resize(to_file, resize)
          }
        )

        to_folder
      }
      cran_images <- resize_images(cran_images, rel_cran_dir)
      local_images <- resize_images(local_images, rel_local_dir)
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
