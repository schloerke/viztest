
callr_document <- function(pkg) {
  message("Documenting local ", pkg_name(pkg))
  callr::r(
    function(pkg) {
      devtools::document(pkg)
    },
    list(
      pkg = pkg
    )
  )
}



callr_knit_file <- function(file) {
  callr::r(
    function(file) {
      rmarkdown::render(file, quiet = TRUE, output_format = 'all')
    },
    list(
      file = file
    )
  )
}
callr_knit_text <- function(text) {
  callr::r(
    function(text) {
      knitr::knit(text = text, quiet = TRUE)
    },
    list(
      text = text
    )
  )
}
