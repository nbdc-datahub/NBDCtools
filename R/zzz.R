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
      "Resource Center (DAIRC) at the J. Craig Venter Institute (JCVI)."
    ))
    packageStartupMessage(glue::glue(
      "If `NBDCtools` is helpful to your research, please cite:\n",
      "Zhang, L., Das, B., Celhay, O., Smith, C. J., Ziemer, L. R., ",
      "Dale, A. M., & Linkersd\u00f6rfer, J. (2026). ",
      "NBDC Data Tools: Building Analysis-ready Datasets From ABCD and HBCD ",
      "Releases in R and Python. bioRxiv. ",
      "https://doi.org/10.64898/2026.11.02.763757"
    ))
    options(nbdctools_start_msg_displayed = TRUE)
  }
}
