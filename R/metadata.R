# all metadata -----------------------------------------------------------------

#' Get release version number(s)
#' @description These functions retrieve all release version number(s)
#' for a given study, or the latest release version number.
#'
#' @param study character. The study name. One of "abcd" or "hbcd".
#'
#' @returns character. The latest release version number(s)
#' of the specified study.
#' @export
#' @examplesIf requireNamespace("NBDCtoolsData", quietly = TRUE)
#' get_releases("abcd")
#' get_releases("hbcd")
#' get_latest_release("abcd")
#' get_latest_release("hbcd")
get_releases <- function(study) {
  chk::chk_string(study)
  chk::chk_subset(study, names(get_data_pkg("dds")))
  names(get_data_pkg("dds")[[study]])
}

#' @rdname get_releases
#' @export
get_releases_abcd <- function() {
  get_releases("abcd")
}

#' @rdname get_releases
#' @export
get_releases_hbcd <- function() {
  get_releases("hbcd")
}

#' @rdname get_releases
#' @export
get_latest_release <- function(study) {
  get_releases(study) |>
    tail(1)
}

#' @rdname get_releases
#' @export
get_latest_release_abcd <- function() {
  get_latest_release("abcd")
}

#' @rdname get_releases
#' @export
get_latest_release_hbcd <- function() {
  get_latest_release("hbcd")
}

#' Internal function to resolve release version to make sure
#' "latest" is converted to the actual latest release version number,
#' and other versions are validated.
#' @return character. The resolved release version number.
#' @noRd
resolve_release <- function(study, release) {
  if (release == "custom") {
    return("custom")
  }
  if (release == "latest") {
    return(get_latest_release(study))
  }
  releases <- get_releases(study)
  if (!release %in% releases) {
    chk::abort_chk(
      glue::glue(
        "Invalid release '{release}'. Valid releases are: ",
        "{paste(releases, collapse = ', ')}\n",
        "If you believe this version should exist, your metadata might be outdated.\n",
        "Please update the `NBDCtoolsData` package to get the latest metadata."
      )
    )
  }
  release
}

#' Get metadata
#'
#' @description
#' Retrieves metadata (data dictionary, levels table, event map)
#' for a given study and release version. Allows for filtering by
#' variables and tables.
#'
#' @param study character. The study name. One of "abcd" or "hbcd".
#' @param release character. Release version (Default: `"latest"`).
#' @param vars character (vector). Vector with the names of variables to be
#'   included.
#' @param tables character (vector). Vector with the names of tables to be
#'   included.
#' @param type character. Type of metadata to retrieve. One of `"dd"`,
#' `"levels"`, `"sessions"` (Default: `"dd"`).
#'
#' @return Data frame with the metadata.
#' @export
#' @examplesIf requireNamespace("NBDCtoolsData", quietly = TRUE)
#' get_metadata("abcd", type = "levels")
#'
#' get_metadata("hbcd", release = "1.0")
#'
#' get_metadata("abcd", vars = c("ab_g_dyn__visit_dtt", "ab_g_dyn__visit_age"))
#'
#' get_metadata("abcd", tables = "ab_g_dyn")
#'
#' get_metadata("abcd", tables = "ab_g_dyn")
#'
#' get_metadata("abcd", type = "sessions")
get_metadata <- function(
  study,
  release = "latest",
  vars = NULL,
  tables = NULL,
  type = "dd"
) {
  # check_data_pkg_installed()
  chk::chk_string(release)
  chk::chk_string(study)
  if (release != "custom") {
    chk::chk_subset(study, names(get_data_pkg("dds")))
  }

  release <- resolve_release(study, release)
  if (release == "custom") {
    study <- "custom_study"
  }
  if (!is.null(vars)) {
    chk::chk_character(vars)
    vars_valid <- get_data_pkg(
      x = "dds",
      is_custom = release == "custom"
    )[[study]][[release]] |>
      pull(name)
    if (!chk::vld_subset(vars, vars_valid)) {
      chk::abort_chk(
        glue::glue(
          "The following variable(s) do not exist in the metadata: ",
          "{paste(setdiff(vars, vars_valid), collapse = ', ')}"
        )
      )
    }
  }
  if (!is.null(tables)) {
    chk::chk_character(tables)
    tables_valid <- get_data_pkg(
      x = "dds",
      is_custom = release == "custom"
    )[[study]][[release]] |>
      pull(table_name)
    if (!chk::vld_subset(tables, tables_valid)) {
      chk::abort_chk(
        glue::glue(
          "The following table(s) do not exist in the metadata: ",
          "{paste(setdiff(tables, tables_valid), collapse = ', ')}"
        )
      )
    }
  }
  chk::chk_string(type)
  chk::chk_subset(type, c("dd", "levels", "sessions"))

  meta <- switch(
    type,
    dd = get_data_pkg("dds", is_custom = release == "custom")[[study]][[release]],
    levels = get_data_pkg("levels", is_custom = release == "custom")[[study]][[release]],
    sessions = get_data_pkg("sessions", is_custom = release == "custom")[[study]][[release]]
  )

  if (is.null(meta)) {
    chk::abort_chk(
      glue::glue(
        "No metadata found for study '{study}' and release '{release}'."
      )
    )
  }

  if (type == "sessions") {
    return(meta)
  }

  vars_combined <- c(
    vars,
    get_data_pkg("dds", is_custom = release == "custom")[[study]][[release]] |>
      filter(
        table_name %in% tables
      ) |>
      arrange(
        match(table_name, tables)
      ) |>
      pull(
        name
      )
  )

  if (length(vars_combined) > 0) {
    meta <- meta |>
      filter(
        name %in% vars_combined
      ) |>
      arrange(
        match(name, vars_combined)
      )
  }

  meta
}


# data dictionary --------------------------------------------------------------

#' Get data dictionary
#'
#' @description
#' Retrieves data dictionary for a given study and release version. Allows for
#' filtering by variables and tables. Wrapper around
#' [NBDCtools::get_metadata()].
#'
#' In addition to the main `get_dd()` function, there are two
#' study-specific variations:
#'
#' - `get_dd_abcd()`: for the ABCD study.
#' - `get_dd_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `get_dd()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @inheritParams get_metadata
#' @param ... Additional arguments passed to the underlying
#' [get_dd()] function.
#' @return Data frame with the data dictionary.
#' @export
#' @examplesIf requireNamespace("NBDCtoolsData", quietly = TRUE)
#'
#' get_dd("abcd")
#'
#' get_dd("hbcd", release = "1.0")
#'
#' get_dd("abcd", vars = c("ab_g_dyn__visit_dtt", "ab_g_dyn__visit_age"))
#'
#' get_dd("abcd", tables = "ab_g_dyn")
#'
#' get_dd_abcd()
#'
#' get_dd_hbcd(release = "1.0")
get_dd <- function(
  study,
  release = "latest",
  vars = NULL,
  tables = NULL
) {
  get_metadata(study, release, vars, tables, type = "dd")
}

#' @rdname get_dd
#' @export
get_dd_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_dd_abcd}. It is set to {.val abcd} by default.")
  }
  get_dd(study = "abcd", ...)
}

#' @rdname get_dd
#' @export
get_dd_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_dd_hbcd}. It is set to {.val hbcd} by default.")
  }
  get_dd(study = "hbcd", ...)
}


# levels table -----------------------------------------------------------------

#' Get levels table
#'
#' @description
#' Retrieves levels table for a given study and release version. Allows for
#' filtering by variables and tables. Wrapper around
#' [NBDCtools::get_metadata()].
#'
#' In addition to the main `get_levels()` function, there are two
#' study-specific variations:
#'
#' - `get_levels_abcd()`: for the ABCD study.
#' - `get_levels_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `get_levels()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @inheritParams get_metadata
#' @param ... Additional arguments passed to the underlying
#' [get_levels()] function.
#' @return Data frame with the levels table.
#' @export
#' @examplesIf requireNamespace("NBDCtoolsData", quietly = TRUE)
#' get_levels("abcd")
#'
#' get_levels("hbcd", release = "1.0")
#'
#' get_levels("abcd", vars = c("ab_g_dyn__visit_type"))
#'
#' get_levels("abcd", tables = "ab_g_dyn")
#'
#' get_levels_abcd(release = "6.0")
#'
#' get_levels_hbcd()
get_levels <- function(study, release = "latest", vars = NULL, tables = NULL) {
  get_metadata(study, release, vars, tables, type = "levels")
}

#' @rdname get_levels
#' @export
get_levels_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_levels_abcd}. It is set to {.val abcd} by default.")
  }
  get_levels(study = "abcd", ...)
}

#' @rdname get_levels
#' @export
get_levels_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_levels_hbcd}. It is set to {.val hbcd} by default.")
  }
  get_levels(study = "hbcd", ...)
}


# sessions table ---------------------------------------------------------------

#' Get sessions table
#'
#' @description
#' Retrieves the sessions table for a given study and release version. Wrapper
#' around [NBDCtools::get_metadata()].
#'
#' In addition to the main `get_sessions()` function, there are two
#' study-specific variations:
#'
#' - `get_sessions_abcd()`: for the ABCD study.
#' - `get_sessions_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `get_sessions()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @inheritParams get_metadata
#' @param ... Additional arguments passed to the underlying
#' [get_sessions()] function.
#' @return Data frame with the sessions table.
#' @export
#' @examplesIf requireNamespace("NBDCtoolsData", quietly = TRUE)
#' get_sessions("abcd")
#'
#' get_sessions("hbcd")
#'
#' get_sessions_abcd(release = "6.0")
#'
#' get_sessions_hbcd(release = "1.0")
get_sessions <- function(
  study,
  release = "latest"
) {
  get_metadata(study, release, type = "sessions")
}

#' @rdname get_sessions
#' @export
get_sessions_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_sessions_abcd}. It is set to {.val abcd} by default.")
  }
  get_sessions(study = "abcd", ...)
}

#' @rdname get_sessions
#' @export
get_sessions_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_sessions_hbcd}. It is set to {.val hbcd} by default.")
  }
  get_sessions(study = "hbcd", ...)
}

# get_session_latest <- function(study, release) {
#   chk::chk_string(study)
#   chk::chk_subset(study, names(get_data_pkg("sessions")))
#   chk::chk_string(release)
#   session_latest <- get_data_pkg("session_latest")[[study]]
#   chk::chk_subset(release, names(session_latest))
#   session_latest[[length(session_latest)]]
# }

# identifier columns -----------------------------------------------------------

#' Get identifier columns
#'
#' @description
#' Retrieves the identifier columns for a given study and release version.
#'
#' In addition to the main `get_id_cols()` function, there are two
#' study-specific variations:
#'
#' - `get_id_cols_abcd()`: for the ABCD study.
#' - `get_id_cols_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `get_id_cols()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @inheritParams get_metadata
#' @param ... Additional arguments passed to the underlying
#' [get_id_cols()] function.
#' @return character vector with the identifier columns.
#' @export
#' @examplesIf requireNamespace("NBDCtoolsData", quietly = TRUE)
#' get_id_cols("abcd")
#'
#' get_id_cols("hbcd")
#'
#' get_id_cols_abcd(release = "6.0")
#'
#' get_id_cols_hbcd(release = "1.0")
get_id_cols <- function(
  study,
  release = "latest"
) {
  get_dd(study, release) |>
    pull(identifier_columns) |>
    stringr::str_split(" \\| ") |>
    unlist() |>
    unique()
}

#' @rdname get_id_cols
#' @export
get_id_cols_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_id_cols_abcd}. It is set to {.val abcd} by default.")
  }
  get_id_cols(study = "abcd", ...)
}

#' @rdname get_id_cols
#' @export
get_id_cols_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_id_cols_hbcd}. It is set to {.val hbcd} by default.")
  }
  get_id_cols(study = "hbcd", ...)
}


# custom metadata ---------------------------------------------------------

#' Add custom metadata
#' @description This function allows users to add custom metadata
#' (data dictionary, levels table, sessions table) to the package environment.
#' This can be useful for users who want to use their own metadata instead of
#' the ones provided by the package, or for testing and development purposes.
#' @param dd data frame. Custom data dictionary.
#' Should have the same structure as the data dictionary provided by
#' the package.
#' @param levels data frame. Custom levels table.
#' @param sessions data frame. Custom sessions table.
#' @details
#' The custom metadata will be stored in the package environment can be
#' accessed with any function that contains the "release" argument with
#' `release = "custom"`. For example,
#' `get_dd(study = "abcd", release = "custom")`.
#'
#' The default value for `dd`, `levels`, and `sessions` is `NULL`.
#' If any of them is not `NULL`, it will be added to the package environment.
#' If the argument is `NULL`, the corresponding metadata will not be added.
#' @return invisible `TRUE`.
#' @export
#'
#' @examples
#' add_custom_metadata(
#'   dd = tibble::tibble(
#'     name = "var1",
#'     table_name = "table1",
#'     identifier_columns = "participant_id"
#'   )
#' )
#' get_dd(study = "abcd", release = "custom")
add_custom_metadata <- function(
  dd = NULL,
  levels = NULL,
  sessions = NULL
) {
  data_env <- getOption("NBDCtoolsData.env")
  if(!is.environment(data_env)) {
    chk::abort_chk(
      "The NBDCtoolsData package environment is not set. Restart your R session. ",
      "If it happens again. Please report this issue to the package developers."
    )
  }
  if (!is.null(dd)) {
    if (!is.data.frame(dd)) {
      chk::abort_chk("The 'dd' argument must be a data frame.")
    }
    cli::cli_inform(c(i = "Added custom data dictionary to NBDCtools"))
    assign("custom_dds", dd, envir = data_env)
  }
  if (!is.null(levels)) {
    if (!is.data.frame(levels)) {
      chk::abort_chk("The 'levels' argument must be a data frame.")
    }
     cli::cli_inform(c(i = "Added custom levels table to NBDCtools"))
    assign("custom_levels", levels, envir = data_env)
  }
  if (!is.null(sessions)) {
    if (!is.data.frame(sessions)) {
      chk::abort_chk("The 'sessions' argument must be a data frame.")
    }
    cli::cli_inform(c(i = "Added custom sessions table to NBDCtools"))
    assign("custom_sessions", sessions, envir = data_env)
  }
  invisible(TRUE)
}
