.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "www",
    system.file(
      "www",
      package = "healthinequalities"
    )
  )
}

.onUnload <- function(libname, pkgname) {
   shiny::removeResourcePath("www")
}