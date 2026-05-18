#' Null-coalescing helper
#'
#' Returns `x` unless it is `NULL`, in which case returns `default`.
#'
#' @param x Any object, possibly `NULL`.
#' @param default Value to return when `x` is `NULL`.
#'
#' @return `x` or `default`.
#' @keywords internal
#' @noRd
null_default <- function(x, default) if (is.null(x)) default else x


#' Determine if a file name is an .xlsx workbook
#'
#' Lightweight check based on file extension only. The server still needs to
#' attempt reading to fully validate the file.
#'
#' @param name Character scalar; file name or path.
#'
#' @return `TRUE` if extension is `.xlsx` (case-insensitive), otherwise `FALSE`.
#' @keywords internal
#' @noRd
#' @importFrom tools file_ext
is_xlsx <- function(name) {
  tolower(tools::file_ext(name)) %in% "xlsx"
}


#' De-duplicate a set of proposed names against an existing set
#'
#' Uses `make.unique()` so earlier names remain unchanged and collisions receive
#' suffixes like `"_v2"`, `"_v3"`, etc.
#'
#' @param names_vec Character vector of proposed names.
#' @param existing Character vector of names that already exist.
#' @param sep Character separator used for uniqueness suffixes. Default `"_v"`.
#'
#' @return Character vector of the same length as `names_vec`, with duplicates resolved.
#' @keywords internal
#' @noRd
dedupe_names <- function(names_vec, existing = character(0), sep = "_v") {
  n_new <- length(names_vec)
  all   <- make.unique(c(existing, names_vec), sep = sep)
  tail(all, n_new)
}



#' Show multiple waiter overlays for given element ids
#'
#' Iterates `waiter::waiter_show()` for a set of DOM ids. Pair this with
#' `hide_waiters(ids)` in an `on.exit()` call to ensure cleanup.
#'
#' @param ids Character vector of waiter target ids.
#' @param html HTML content for the waiter overlay. Typically a global `waiting_screen`.
#' @param color Background color of the overlay.
#'
#' @return Invisibly returns `ids`.
#' @keywords internal
#' @noRd
#' @importFrom waiter waiter_show
show_waiters <- function(ids, html, color = "#001a2b") {
  for (wid in ids) waiter::waiter_show(html = html, color = color, id = wid)
  invisible(ids)
}


#' Hide multiple waiter overlays
#'
#' @param ids Character vector of waiter target ids previously shown.
#'
#' @return Invisibly returns `ids`.
#' @keywords internal
#' @noRd
#' @importFrom waiter waiter_hide
hide_waiters <- function(ids) {
  for (wid in rev(ids)) waiter::waiter_hide(id = wid)
  invisible(ids)
}


#' Batch reader with progress and error isolation
#'
#' Reads each uploaded file via a user-supplied `read_one()` function, shows a
#' transient progress notification per file, and captures errors per-file
#' without aborting the batch.
#'
#' @param upl_df Data frame from Shiny's `fileInput()` (e.g., `input$…`).
#' @param base_names Character vector of base file names (same length as `nrow(upl_df)`).
#' @param read_one Function of the form `function(path) { … }` that returns a result
#'   for a single file path, or throws an error.
#' @param progress_prefix Character scalar used in the progress notification prefix.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{ok}{Logical vector; `TRUE` where reading succeeded.}
#'     \item{results}{List of results (length = `nrow(upl_df)`).}
#'     \item{paths}{Character vector of the file paths read.}
#'   }
#' @keywords internal
#' @noRd
#' @importFrom shiny showNotification
read_with_progress <- function(upl_df, base_names, read_one, progress_prefix = "Reading") {
  n   <- nrow(upl_df)
  ok  <- logical(n)
  res <- vector("list", n)
  pth <- character(n)

  for (i in seq_len(n)) {

    shiny::showNotification(
      sprintf("%s %d/%d: %s", progress_prefix, i, n, base_names[i]),
      type = "message", duration = 5, closeButton = FALSE
    )
    path <- upl_df$datapath[i]

    tryCatch({

      res[[i]] <- read_one(path)
      pth[i]   <- path
      ok[i]    <- TRUE

    }, error = function(e) {

      showNotification(sprintf("Failed to read '%s'. Did you upload the correct file?",
                               base_names[i]),
                       duration = 10,
                       type = 'error'
      )

    })
  }

  list(ok = ok, results = res, paths = pth)
}


#' Safe reader for model outputs workbook
#'
#' Wrapper that reads emissions and energy sheets from a COMIT output workbook.
#'
#' @param path Character scalar; path to an .xlsx file.
#'
#' @return A list with elements `Emissions` and `Energy`.
#' @keywords internal
#' @noRd
read_outputs_safe <- function(path) {
  list(
    Emissions = read_outputs(path, "Emissions"),
    Energy    = read_outputs(path, "Energy")
  )
}



#' Generate Notification of Errorsome Upload to comit App
error_notification <- function(err){

  print('Upload failed. Did you use the correct file?')
  #print(err) # Unhash if helpful for debugging to get specific error.

  id_upload_error <- showNotification('Upload failed. Did you use the correct file?',
                                      type = 'error')

}



