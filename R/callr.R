
callr_document <- function(pkg) {
  message("Documenting local ", pkg_name(pkg))
  callr::r(
    function(pkg) {
      devtools::document(pkg)
    },
    list(
      pkg = pkg
    ),
    show = TRUE
  )
}
callr_install <- function(pkg, location) {
  callr::r(
    function(pkg, location) {
      switch(location,
        "local" = remotes::install_local(pkg, dependencies = TRUE),
        "github" = remotes::install_github(pkg, dependencies = TRUE),
        "cran" = remotes::install_version(pkg, dependencies = TRUE)
      )
    },
    list(
      pkg = pkg,
      location = location
    ),
    show = TRUE
  )
}


callr_render_infos <- function(rd_names, render_infos, save_individual) {
  p <- callr::r(
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
        tryCatch(
          {
            if (save_individual) {
              rmarkdown::render(render_info, quiet = TRUE, output_format = 'all')
            } else {
              knitr::knit(text = render_info, quiet = TRUE)
            }
          },
          error = function(e) {
            message("", e) # must keep "" to avoid from throwing in parent process
            NULL
          }
        )
        invisible()
      })
      invisible()
    },
    list(
      rd_names = rd_names,
      render_infos = render_infos,
      save_individual = save_individual
    ),
    show = TRUE
  )
  invisible()
}
