
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


callr_render_infos <- function(rd_names, render_infos, save_individual) {
  p <- callr::r_bg(
  # p <- callr::r(
    function(rd_names, render_infos, save_individual) {
      pb <- progress::progress_bar$new(
        total = length(render_infos),
        format = paste0(":current/:total ellapsed::elapsed eta::eta [:bar] :rd_name\n"),
        show_after = 0,
        clear = FALSE,
        stream = stdout(),
        force = TRUE
      )

      purrr::map2(rd_names, render_infos, function(rd_name, render_info) {
        pb$tick(tokens = list(rd_name = rd_name))
        if (save_individual) {
          rmarkdown::render(render_info, quiet = TRUE, output_format = 'all')
        } else {
          knitr::knit(text = render_info, quiet = TRUE)
        }
      })
      invisible()
    },
    list(
      rd_names = rd_names,
      render_infos = render_infos,
      save_individual = save_individual
    ),
    stdout = "|", stderr = "|"
  )
  on.exit({
    p$kill()
  })
  cat_n <- function(txt) {
    if (length(txt) > 0) {
      cat(txt, sep = "\n")
    }
  }
  while(p$is_alive()) {
    p$wait(500) # wait until min(c(time_ms, process ends))
    cat_n(p$read_error_lines())
    cat_n(p$read_output_lines())
  }
  invisible()
}


print_process <- function(p) {
}
