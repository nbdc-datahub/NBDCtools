.onLoad <- function(libname, pkgname) {
  # create empty environment for caching metadata objects
  options("NBDCtoolsData.env" = new.env())
}

.onAttach <- function(libname, pkgname) {
  if (!getOption("nbdctools_start_msg_displayed", FALSE)) {
    packageStartupMessage(glue::glue(
      "Welcome to the `NBDCtools` package! For more information, ",
      "visit: https://software.nbdc-datahub.org/NBDCtools/"
    ))
    packageStartupMessage(glue::glue(
      "This package is developed by the ABCD Data Analysis, Informatics & ",
      "Resource Center (DAIRC) at the J. Craig Venter Institute (JCVI)"
    ))
    # TODO uncomment once we have a paper
    # packageStartupMessage(glue::glue(
    #   "If `NBDCtools` is helpful to your research, ",
    #   "please kindly cite it as:\n",
    #   "L Zhang, xxx & J Linkersdörfer. NBDCtools: xxx. 2025. xxx",
    # ))
    options(nbdctools_start_msg_displayed = TRUE)
  }
}
