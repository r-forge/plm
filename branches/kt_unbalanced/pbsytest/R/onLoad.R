## onload message

.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Behavior of 'plm::pbsystest(..., test=\"re\")' changed, statistic is now chisquared.\n To get old behavior of v1.4-0, use 'plm::pbsystest(..., test=\"re\", normal=TRUE)'")
}