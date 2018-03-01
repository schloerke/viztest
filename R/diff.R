
#' Compare two output folders
#'
#' @param output_dir Output folder created by \code{viztest}
#' @param browse If \code{TRUE}, diff.html in \code{output_dir} will be opened
#' @export
#' @importFrom utils browseURL
viz_compare <- function(output_dir, browse = TRUE) {
  withr::with_dir(output_dir, {
    cran_images <- file.path(rel_cran_dir, rel_images_dir)
    local_images <- file.path(rel_local_dir, rel_images_dir)

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
