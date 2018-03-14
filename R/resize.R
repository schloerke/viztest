

resize_images <- function(from_folder, rel_dir, tmpdir, resize) {
  to_folder <- file.path(tmpdir, paste0(rel_dir, "_images"))
  dir.create(to_folder, recursive = TRUE)
  from_files <- dir(from_folder, full.names = TRUE)
  file.copy(
    from_files,
    to_folder,
    recursive = TRUE
  )
  message("\nShrinking ", from_folder)
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
