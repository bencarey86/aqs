.create_temp_subdirectory <- function(name) {
    temp_dir <- file.path(tempdir(), paste0(name, Sys.time(), sample(1:1000000, 1)))
    dir.create(temp_dir)
    temp_dir
}
