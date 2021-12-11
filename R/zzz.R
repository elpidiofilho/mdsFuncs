.onAttach <- function(libname, pkgname)  {
  packageStartupMessage("mdsFuncs", utils::packageDescription("mdsFuncs ",
                                                              fields = "Version"),
                        , appendLF = TRUE)
}
