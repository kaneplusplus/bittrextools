
#' Cancel all open orders
#' @param dry_run should this be run on the exchange? (default FALSE)
#' @importFrom bittrex bt_cancel bt_getopenorders
#' @importFrom magrittr %>%
#' @export 
cancel_open_orders <- function(dry_run=FALSE) {
  open_orders <- bt_getopenorders() %>% get_result
  for (i in seq_len(nrow(open_orders))) {
    if (!dry_run) {
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

#' Buy a cryptocurrency
#'
#' @param market which currency to buy
#' @param quantity how many of the quote currency should be bought?
#' @param price how much (in the base currency) would you like to pay?
#' @param dry_run should this be run on the exchange? (default FALSE)
#' @importFrom bittrex bt_buy
#' @export
bt_buy <- function(market, quantity, price, dry_run=TRUE) {
  if (dry_run) {
    cat("Dry run BUY:", market, ",", quantity, ",", price, "\n")
    "1234"
  } else {
    bt_buy(market, quantity, price) %>% get_result
  }
}

#' Sell a cryptocurrency
#'
#' @param market which currency to buy
#' @param quantity how many of the quote currency should be sold?
#' @param price how much you'd like someone else to pay (in the base currency) 
#' for each unit the quote currency?
#' @param dry_run should this be run on the exchange? (default FALSE)
#' @importFrom bittrex bt_sell
#' @export
bt_sell <- function(market, quantity, price, dry_run=TRUE) {
  if (dry_run) {
    cat("Dry run: SELL,", market, ",", quantity, ",", price, "\n")
    "1234"
  } else {
    bt_sell(market, quantity, price) %>% get_result
  }
}

