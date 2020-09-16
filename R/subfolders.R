#' Make subfolders for Moodle upload.
#'
#' Use a Moodle "Assignment Submission Report" to create subfolders
#' named using Moodle format to facilitate uploading of feedback
#' files.
#'
#' @param path Path to the assignment submission report.
#' @param outdir Subdirectory in which to create the folders. If it
#'   does not exist, it will be created.
#' @param overwrite Whether to overwrite an existing subdirectory.
#' @return Although this function is intended to be used for its side
#'   effect of creating subfolders, for convenience it returns a data
#'   frame with Username, Email address, ID number, and path to the
#'   subfolders.
#' @export
feedback_subfolders <- function(path, outdir = "feedback_folders",
                                overwrite = FALSE) {
  if (dir.exists(outdir) && !overwrite) {
    stop("subdirectory '", outdir, "' exists and overwrite = FALSE")
  }

  if (dir.exists(outdir)) unlink(outdir, TRUE, TRUE)

  dir.create(outdir, FALSE)

  users <- readxl::read_excel(path, skip = 3)

  users[["subfolder"]] <- paste0(sub(" ", "_", users[["Username"]]),
                                 "_assignsubmission_file_")

  lapply(users[["subfolder"]], function(.x) {
    dir.create(file.path(outdir, .x), FALSE)
  })
  
  users[, c("Username", "Email address", "ID number", "subfolder")]
}
