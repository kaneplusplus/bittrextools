
#' Get the open orders
#' @importFrom magrittr %>%
#' @export
open_orders <- function() {
  bt_getopenorders() %>% get_result()
}

#' Cancel the open buy orders
#' @param dry_run is this a dry run (not actually run)? (default: TRUE)
#' @param log_cb a callback is given market, time_stamp, order_type, quantity
#' price, and order_uuid. (default: NULL)
#' @importFrom bittrex bt_getopenorders bt_cancel
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @export
cancel_open_buys <- function(dry_run=TRUE, log_cb=NULL) {
  order_type <- order_uuid <- NULL
  open_buys <- bt_getopenorders() %>% get_result() %>%  
    filter(order_type == "LIMIT_BUY")
  for (i in 1:nrow(open_buys)) {
    log_tbl <- tibble(market_name=as.character(open_buys$exchange[i]),
      time_stamp=as.integer(as.POSIXct(Sys.time())),
      order_type=as.character(open_buys$order_type[i]),
      quantity=as.double(open_buys$quantity_remaining[i]),
      price=as.double(open_buys$limit),
      order_uuid=as.character(order_uuid),
      dry_run=as.integer(dry_run))
    if (!dry_run) {
      result <- bt_cancel(open_buys$order_uuid[i]) %>% get_result()
    }
    if (!is.null(log_cb)) {
      log_cb(log_tbl)
    }
  }
  invisible(TRUE)
}

#' Cancel the open sell orders
#' @param dry_run is this a dry run (not actually run)? (default: TRUE)
#' @param log_cb a callback is given market, time_stamp, order_type, quantity
#' price, and order_uuid. (default: NULL)
#' @importFrom bittrex bt_getopenorders bt_cancel
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @export
cancel_open_sells <- function(dry_run=TRUE, log_cb=NULL) {
  order_type <- open_buys <- order_uuid <- NULL
  open_sells <- bt_getopenorders() %>% get_result() %>%
    filter(order_type == "LIMIT_SELL")
  for (i in 1:nrow(open_buys)) {
    log_tbl <- tibble(market_name=as.character(open_buys$exchange[i]),
      time_stamp=as.integer(as.POSIXct(Sys.time())),
      order_type=as.character(open_buys$order_type[i]),
      quantity=as.double(open_buys$quantity_remaining[i]),
      price=as.double(open_buys$limit),
      order_uuid=as.character(order_uuid),
      dry_run=as.integer(dry_run))
    if (!dry_run) {
      result <- bt_cancel(open_sells$order_uuid[i]) %>% get_result()
    }
    if (!is.null(log_cb)) {
      log_cb(log_tbl)
    }
  }
}

#' Cancel all open orders
#' @param dry_run is this a dry run (not actually run)? (default: TRUE)
#' @param log_cb a callback is given market, time_stamp, order_type, quantity
#' price, and order_uuid. (default: NULL)
#' @importFrom bittrex bt_cancel bt_getopenorders
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @export 
cancel_open_orders <- function(dry_run=TRUE, log_cb=NULL) { 
  cancel_open_buys(dry_run, log_cb)
  cancel_open_sells(dry_run, log_cb)
  invisible(TRUE)
}

#' Place a buy order
#' @param market the market to place the buy limit order on.
#' @param quantity the quantity of the transaction currency to buy.
#' @param rate the price you are willing to pay per unit of the transaction
#' currency.
#' @param dry_run is this a dry run (not actually run)? (default: TRUE)
#' @param log_cb a callback is given market, time_stamp, order_type, quantity
#' price, and order_uuid. (default: NULL)
#' @importFrom magrittr %>%
#' @importFrom bittrex bt_buy
#' @importFrom tibble tibble
#' @export
execute_buy <- function(market, quantity, rate, dry_run=TRUE, log_cb=NULL) {
  log_tbl <- tibble(market_name=as.character(market),
    time_stamp=as.integer(as.POSIXct(Sys.time())),
    order_type=as.character("LIMIT_BUY"),
    quantity=as.double(quantity),
    price=as.double(rate),
    order_uuid=as.character(""),
    dry_run=as.integer(dry_run))
  if (!dry_run) {
    result <- bt_buy(market, quantity, rate) %>% get_result()
    log_tbl$order_uuid[1] <- result$uuid
  }
  if (!missing(log_cb)) {
    log_cb(log_tbl)
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
#' price, and order_uuid. (default: NULL)
#' @importFrom magrittr %>%
#' @importFrom bittrex bt_sell
#' @importFrom tibble tibble
#' @export
execute_sell <- function(market, quantity, rate, dry_run=TRUE, log_cb=NULL) {
  log_tbl <- tibble(market_name=as.character(market),
    time_stamp=as.integer(as.POSIXct(Sys.time())),
    order_type=as.character("LIMIT_BUY"),
    quantity=as.double(quantity),
    price=as.double(rate),
    order_uuid=as.character(""),
    dry_run=as.integer(dry_run))
  if (!dry_run) {
    result <- bt_sell(market, quantity, rate) %>% get_result()
    log_tbl$order_uuid[1] <- result$uuid
  }
  if (!missing(log_cb)) {
    log_cb(log_tbl)
  }
  invisible(TRUE)
}

#' Flip the base and quote currency for a market
#'
#' @param x the vector of markets to flip.
#' @export
flip_market_name <- function(x) {
  if (length(x)) {
    mkts <- matrix(unlist(strsplit(x, "-")), ncol=2, byrow=TRUE)
    apply(mkts, 1, function(s) paste0(s[2], "-", s[1]))
  } else {
    character()
  }
}


#' Flip a market 
#'
#' @param market_name the market names.
#' @param high the vector of highs.
#' @param low the vector of lows. 
#' @param volume the vector of volumes. 
#' @param last the vector of last prices.
#' @param base_volume the vector of base_volumes - this gets set to NA.
#' @param time_stamp the vector of time stamps.
#' @param bid the vecor of bids.
#' @param ask the vecor of asks. 
#' @param open_buy_orders the vector of open buy orders.
#' @param open_sell_orders the vector of open sell orders.
#' @param prev_day the previous day's close.
#' @param created the vector of dates the market was created.
#' @export
flip_markets <- function(market_name, high, low, volume, last, base_volume,
  time_stamp, bid, ask, open_buy_orders, open_sell_orders, prev_day, created) {
  ret <- tibble(market_name=flip_market_name(market_name))
  if (!missing(high)) {
    ret$low <- 1/high
  }
  if (!missing(low)) {
    ret$high <- 1/low
  }
  if (!missing(volume)) {
    ret$volume <- volume
  }
  if (!missing(last)) {
    ret$last <- 1/last
  }
  if (!missing(base_volume)) {
    ret$base_volume <- NA
  }
  if (!missing(time_stamp)) {
    ret$time_stamp <- time_stamp
  }
  if (!missing(ask)) {
    ret$bid <- 1/ask
  }
  if (!missing(bid)) {
    ret$ask <- 1/bid
  }
  if (!missing(open_sell_orders)) {
    ret$open_buy_orders <- open_sell_orders
  }
  if (!missing(open_buy_orders)) {
    ret$open_sell_orders <- open_buy_orders
  }
  if (!missing(prev_day)) {
    ret$prev_day <- 1/prev_day
  }
  if (!missing(created)) {
    ret$created <- created
  }
  ret
}

#' Flip Bittrex market summaries
#'
#' @param x the market summary
#' @export
flip_market_summary <- function(x) {
  if (nrow(x) == 0) {
    tibble(market_name=character(), high=double(), low=double(),
      volume=double(), last=double(), base_volume=double(),
      time_stamp=integer(), bid=double(), ask=double(),
      open_buy_orders=integer(), open_sell_orders=integer(),
      prev_day=double(), created=character())
  } else {
    flip_markets(market_name=x$market_name, high=x$high, low=x$low, 
      volume=x$volume, last=x$last, base_volume=x$base_volume,
      time_stamp=x$time_stamp, bid=x$bid, ask=x$ask, 
      open_buy_orders=x$open_buy_orders, open_sell_orders=x$open_sell_orders,
      prev_day=x$prev_day, created=x$created)
  }
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

