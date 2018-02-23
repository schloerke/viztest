##################
save_example_widgets <- function(
  pkg = ".",
  output_dir = file.path("viztest", devtools::as.package(pkg)$version),
  ...,
  stomp = FALSE
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
    unlink(output_dir, recursive = TRUE)
  }
  dir.create(output_dir, recursive = TRUE)

  # make sure it's the latest docs
  devtools::document(pkg)

  # get all the doc files from the local pkg
  rd_files <- devtools:::rd_files(pkg) #%>% head()
  pb <- progress::progress_bar$new(
    total = length(rd_files)
  )

  if (length(rd_files) == 0) {
    message("No .Rd files found")
    return(invisible(TRUE))
  }
  message("Running ", length(rd_files), " examples")

  print_widget <- function(name) {
    count <- 1
    widget_print <- function(x, ..., view = TRUE) {
      widget <- x
      file_name <- paste0(name, "-", count)
      html_file <- paste0(file_name, ".html")
      png_file <- paste0(file_name, ".png")
      message("Saving: ", file.path(output_dir, png_file))

      withr::with_dir(output_dir, {
        htmlwidgets::saveWidget(widget, file = html_file)
        webshot::webshot(html_file, png_file, delay = 1)
        unlink(html_file)
      })
      count <<- count + 1

      # do not actually print it
      NULL
    }
    widget_print
  }

  pb <- progress::progress_bar$new(
    total = length(rd_files),
    format = "[:bar] :current/:total :elapsedfull eta::eta :name_val\n",
    show_after = 0
  )
  lapply(names(rd_files), function(rd_name) {
    message("example: ", rd_name)
    pb$tick(
      tokens = list(
        name_val = rd_name
      )
    )

    assign(
      "print.htmlwidget",
      print_widget(gsub(".Rd", "", rd_name, fixed = TRUE)),
      envir = globalenv()
    )

    pkgload::run_example(rd_files[[rd_name]], test = FALSE)
    TRUE
  })

  invisible(TRUE)
}

# # works. only widgets
# save_example_widgets(stomp = TRUE) # nolint
