#' check the class of an object
#' @param x the object to check the class of
#' @param classes the classes to check against
#' @noRd
#' @keywords internal
class_assert <- function(x, classes) {
  if (!any(class(x) %in% classes)) {
    obj_name <- deparse(substitute(x))
    classes_str <- glue::glue(
      glue::glue_collapse(classes[-length(classes)], sep = c(", ")),
      " or ", classes[length(classes)]
    )
    cli::cli_abort(
      c("x" = "{.code {obj_name}} must be an object of class
          {.emph {.field {classes_str}}} class{?es}, not {.emph {.type {x}}}")
    )
  }
}
