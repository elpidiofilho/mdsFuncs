.onAttach <- function(libname, pkgname) {
  packageStartupMessage("mdsFuncs ",
    utils::packageDescription("mdsFuncs", field = "Version"),
    " ",
    appendLF = TRUE
  )
}
