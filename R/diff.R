
#' Compare two output folders
#'
#' @param output_a,output_b folders to compare that contain output images
#' @export
viz_compare <- function(output_a, output_b) {
  shinytest::diffviewer_widget(output_a, output_b)
}
