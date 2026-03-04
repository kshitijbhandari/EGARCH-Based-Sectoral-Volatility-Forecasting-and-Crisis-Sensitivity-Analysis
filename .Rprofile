project_lib <- normalizePath(
  file.path(getwd(), "TimeSeries", "library"),
  winslash = "/",
  mustWork = FALSE
)

if (dir.exists(project_lib)) {
  .libPaths(c(project_lib, .libPaths()))
}
