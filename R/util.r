
#' Cancel all open orders
#' @param dry_run should this be run on the exchange? (default FALSE)
#' @importFrom bittrex bt_cancel bt_getopenorders
#' @importFrom magrittr %>%
#' @export 
cancel_open_orders <- function(dry_run=FALSE) { 
  if (!dry_run) {
    open_orders <- bt_getopenorders() %>% get_result
    for (i in seq_len(nrow(open_orders))) {
      bt_cancel(open_orders$order_uuid[i])
    }
  }
  invisible(TRUE)
}

#' If a bittrex call was successful, return the data
#'
#' @param x the return of a call to the Bittrex exchange 
#' @export
get_result <- function(x) {
  if (x$success != TRUE)
    stop(x$message)
  x$result
}

#' Turn an integer into a posixct type
#' 
#' @param x the integer to turn into a posixct
#' @export
make_posixct <- function(x) {
  as.POSIXct(x, origin="1970-01-01 00:00.00 UTC", tz="UTC")
}

