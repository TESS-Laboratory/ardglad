#' Create the lookup table for date interval codes
#' @return a data frame with 23 rows and 31 columns
#' @noRd
#' @keywords internal
yr_int_lookup <- function() {
  t_interval_id <- t(matrix(392:1104, nrow = 23, ncol = length(1997:2027))) |>
    as.data.frame(row.names = 1997:2027)
  colnames(t_interval_id) <- paste0("int_", 1:23)
  return(t_interval_id)
}

#' Create the lookup table for day of year intervals
#' @return a tibble with 23 rows and 2 columns
#' @noRd
#' @keywords internal
doy_lookup <- function() {
  start_doy <- seq(1, 366, by = 16)
  end_doy <- c(seq(16, 366, by = 16), 366)

  mapply(seq, start_doy, end_doy)
}

#' get the sequence of doy intervals for a given date or date range
#' @param d a character string representing a date or a vector of one or two
#'  dates
#' @return a vector of day of year intervals (integer values)
#' @noRd
#' @keywords internal
which_doy <- function(d) {
  if (length(d) > 2) {
    cli::cli_abort(c(
      "x" = "`d must be a character string representing a date or a vector of
      one or two dates",
      "i" = "In this case `d` is length {length(d)}"
    ))
  }
  doyl <- doy_lookup()
  d_form <- lubridate::ymd(d) |>
    lubridate::yday()

  sapply(
    d_form,
    function(x) {
      which(sapply(doyl, function(y) x %in% y))
    }
  )
}

#' get the time interval ids for a given date or date range
#' @param start_date a character string representing the start date
#' @param end_date a character string representing the end date
#' @return a vector of time interval ids (integer values)
#' @noRd
#' @keywords internal
time_int_range <- function(start_date, end_date = NULL) {
  is_valid_date(start_date)
  if (!is.null(end_date)) {
    ed_ob <- deparse(substitute(end_date))
    is_valid_date(end_date)
    assert_date_after(start_date, end_date)
  }

  yrs <- lubridate::year(lubridate::ymd(c(start_date, end_date)))
  doy_id <- which_doy(c(start_date, end_date))

  start_end_ids <- mapply(
    function(x, y) {
      yr_int_lookup()[as.character(y), paste0("int_", x)]
    },
    doy_id,
    yrs
  )

  if (length(start_end_ids) > 1) {
    time_int_seq <- seq(start_end_ids[1], start_end_ids[2])
  } else {
    time_int_seq <- start_end_ids
  }


  yr_int_tab <- yr_int_lookup()

  seq_names <- lapply(
    time_int_seq,
    function(x) {
      i <- which(yr_int_tab == x, arr.ind = TRUE)
      int_period <- doy_lookup()[[i[2]]]
      period_st <- int_period[1]
      period_end <- int_period[length(int_period)]

      form_date <- function(x) {
        lubridate::make_date(as.numeric(rownames(i))) +
          lubridate::days(x - 1)
      }

      paste0(
        form_date(period_st),
        ":",
        form_date(period_end)
      )
    }
  ) |>
    unlist()

  names(time_int_seq) <- seq_names

  return(time_int_seq)
}



#' Check if the input is a valid date
#' @param date_string a character string representing a date
#' @param err a logical value indicating whether to throw an error if the
#' date is invalid
#' @return a logical value indicating whether the date is valid
#' @noRd
#' @keywords internal
is_valid_date <- function(date_string, err = TRUE) {
  ds_obj <- deparse(substitute(date_string))
  # Attempt to parse the date
  parsed_date <- lubridate::ymd(date_string, quiet = TRUE)
  if (is.na(parsed_date)) {
    if (err) invalid_date_msg(date_string, ds_obj)
    return(FALSE)
  }
  return(TRUE)
}

#' Create an error message for invalid date values
#' @param d a character string representing the invalid date
#' @param obj_name a character string representing the object name
#' @noRd
#' @keywords internal
invalid_date_msg <- function(d, obj_name) {
  cli::cli_abort(c(
    "x" = "The date value `{d}` for `{obj_name}` is invalid. Please provide a
    valid date.",
    "i" = "The format of the date should be `YYYY-MM-DD`"
  ))
}

#' Check if the end date is after the start date
#' @param start_date a character string representing the start date
#' @param end_date a character string representing the end date
#' @noRd
assert_date_after <- function(start_date, end_date) {
  if (!lubridate::ymd(end_date) > lubridate::ymd(start_date)) {
    cli::cli_abort(c(
      "x" = "`end_date` must be after the `after_start",
      "i" = "Please check that `end_date` occurs after `start_date`"
    ))
  }
}
