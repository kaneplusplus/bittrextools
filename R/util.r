
#' Get the open orders
#' @importFrom magrittr %>%
#' @export
open_orders <- function() {
  bt_getopenorders() %>% get_result()
}

#' Cancel the open buy orders
#' @param dry_run is this a dry run (not actually run)? (default: TRUE)
#' @param log_cb a callback is given market, time_stamp, order_type, quantity
#' price, and order_uuid.
#' @importFrom bittrex bt_getopenorders bt_cancel
#' @importFrom dplyr filter
cancel_open_buys <- function(dry_run=TRUE, log_cb) {
  if (!dry_run) {
    open_buys <- bt_getopenorders() %>% get_result() %>%  
      filter(order_type == "LIMIT_BUY")
    for (i in 1:nrow(open_buys)) {
      log_tbl <- tibble(market_name=as.character(open_buys$exchange[i]),
        time_stamp=as.integer(as.POSIXct(Sys.time())),
        order_type=as.character(open_buys$order_type[i]),
        quantity=as.double(open_buys$quantity_remaining[i]),
        price=as.double(open_buys$limit),
        order_uuid=as.character(order_uuid))
      result <- bt_cancel(open_buys$order_uuid[i]) %>% get_result()
      if (!missing(log_cb)) {
        log_cb(log_tbl)
      }
    }
  }
  invisible(TRUE)
}

#' Cancel the open sell orders
#' @param dry_run is this a dry run (not actually run)? (default: TRUE)
#' @param log_cb a callback is given market, time_stamp, order_type, quantity
#' price, and order_uuid.
#' @importFrom bittrex bt_getopenorders bt_cancel
#' @importFrom dplyr filter
cancel_open_sells <- function(dry_run=TRUE) {
  if (!dry_run) {
    open_sells <- bt_getopenorders() %>% get_result() %>%
      filter(order_type == "LIMIT_SELL")
    for (i in 1:nrow(open_buys)) {
      log_tbl <- tibble(market_name=as.character(open_buys$exchange[i]),
        time_stamp=as.integer(as.POSIXct(Sys.time())),
        order_type=as.character(open_buys$order_type[i]),
        quantity=as.double(open_buys$quantity_remaining[i]),
        price=as.double(open_buys$limit),
        order_uuid=as.character(order_uuid))
      result <- bt_cancel(open_sells$order_uuid[i]) %>% get_result()
      if (!missing(log_cb)) {
        log_cb(log_tbl)
      }
    }
  }
}

#' Cancel all open orders
#' @param dry_run is this a dry run (not actually run)? (default: TRUE)
#' @param log_cb a callback is given market, time_stamp, order_type, quantity
#' price, and order_uuid.
#' @importFrom bittrex bt_cancel bt_getopenorders
#' @importFrom magrittr %>%
#' @export 
cancel_open_orders <- function(dry_run=TRUE, log_cb) { 
  
  if (!dry_run) {
    cancel_open_buys(dry_run, log_cb)
    cancel_open_sells(dry_run, log_cb)
  }
  invisible(TRUE)
}

#' Place a buy order
#' @param market the market to place the buy limit order on.
#' @param quantity the quantity of the transaction currency to buy.
#' @param rate the price you are willing to pay per unit of the transaction
#' currency.
#' @param dry_run is this a dry run (not actually run)? (default: TRUE)
#' @param log_cb a callback is given market, time_stamp, order_type, quantity
#' price, and order_uuid.
#' @importFrom magrittr %>%
#' @importFrom bittrex bt_buy
#' @export
execute_buy <- function(market, quantity, rate, dry_run=TRUE, log_cb) {
  if (!dry_run) {
    log_tbl <- tibble(market_name=as.character(market),
      time_stamp=as.integer(as.POSIXct(Sys.time())),
      order_type=as.character("LIMIT_BUY"),
      quantity=as.double(quantity),
      price=as.double(rate),
      order_uuid=as.character("NA"))
    result <- bt_buy(market, quantity, rate) %>% get_result()
    log_tbl$order_uuid[1] <- result$uuid
    if (!missing(log_cb)) {
      log_cb(log_tbl)
    }
  }
  invisible(TRUE)
}

#' Place a sell order
#' @param market the market to place the buy limit order on.
#' @param quantity the quantity of the base currency to sell. 
#' @param rate the price you are willing to pay per unit of the quote
#' currency.
#' @param dry_run is this a dry run (not actually run)? (default: TRUE)
#' @param log_cb a callback is given market, time_stamp, order_type, quantity
#' price, and order_uuid.
#' @importFrom magrittr %>%
#' @importFrom bittrex bt_buy
#' @export
execute_sell <- function(market, quantity, rate, dry_run=TRUE, log_cb) {
  if (!dry_run) {
    log_tbl <- tibble(market_name=as.character(market),
      time_stamp=as.integer(as.POSIXct(Sys.time())),
      order_type=as.character("LIMIT_BUY"),
      quantity=as.double(quantity),
      price=as.double(rate),
      order_uuid=as.character("NA"))
    result <- bt_sell(market, quantity, rate) %>% get_result()
    log_tbl$order_uuid[1] <- result$uuid
    if (!missing(log_cb)) {
      log_cb(log_tbl)
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
as_posixct <- function(x) {
  as.POSIXct(x, origin="1970-01-01 00:00.00 UTC", tz="UTC")
}

