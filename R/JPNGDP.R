#' Japan Real GDP
#'
#' Real Gross Domestic Product for Japan, seasonally adjusted.
#'
#' @format A quarterly \code{data.frame} with 2 columns:
#' \describe{
#'   \item{date}{Date of observation (first day of quarter)}
#'   \item{RGDP}{Real GDP in billions of chained 2015 yen}
#' }
#'
#' @source Board of Governors of the Federal Reserve System (US) via FRED
#' \url{https://fred.stlouisfed.org/series/JPNRGDPEXP}
#'
#' @examples
#' data(JPNGDP)
#' head(JPNGDP)
#' plot(JPNGDP$date, JPNGDP$RGDP, type = "l")

"JPNGDP"






#' Japan Real GDP

# require("xts")
# url_JPNGDP <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=JPNRGDPEXP"
# JPNGDP <- read.csv(url) ; names(JPNGDP) <- c("date", "RGDP") ; JPNGDP[, 1] <- as.Date(JPNGDP[, 1]); head(JPNGDP) ; tail(JPNGDP)
# plot.ts(temp$JPNRGDPEXP)
#
# JPNGDP <- xts(JPNGDP$RGDP, order.by = JPNGDP$date)
# colnames(JPNGDP) <- "RGDP"

# JPNGDP <- ts(tsatoolkit::JPNGDP, start = c(1994, 1), frequency = 4)
# plot.ts(JPNGDP)

# head(JPNGDP) ; tail(JPNGDP) ; plot.ts(JPNGDP)
# usethis::use_data(JPNGDP, overwrite = TRUE)




