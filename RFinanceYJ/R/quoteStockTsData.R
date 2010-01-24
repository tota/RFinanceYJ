quoteStockTsData <-
function( x, since=NULL, start_num=0){
  r <- NULL
  quote.table <- NULL
  stock.data <- data.frame(NULL)
  start <- (gsub("([0-9]{4,4})-([0-9]{2,2})-([0-9]{2,2})","&c=\\1&a=\\2&b=\\3",since))
  quote.url <- paste('http://table.yahoo.co.jp/t?s=',x,start,'&y=',start_num)

  try( r <- xmlRoot(htmlTreeParse(quote.url, error=xmlErrorCumulator(immediate=F))) , TRUE)
  if( is.null(r) ) stop(paste("Can not access :", x))

  try( quote.table <- r[[2]][[1]][[1]][[16]][[1]][[1]][[1]][[4]][[1]][[1]][[1]], TRUE )
  if( is.null(quote.table) ) stop(paste("Can not quote :", x))

  end <- xmlSize(quote.table)

  for(i in 2:end){
    tmp <- quote.table[[i]]
    d <- gsub("^([0-9]{4})([^0-9]+)([0-9]{1,2})([^0-9]+)([0-9]{1,2})([^0-9]+)",
              "\\1-\\3-\\5",
              iconv(xmlValue(tmp[[1]]), "EUC-JP", "UTF-8", ""))
    o <- gsub("[^0-9]", "", xmlValue(tmp[[2]]))
    h <- gsub("[^0-9]", "", xmlValue(tmp[[3]]))
    l <- gsub("[^0-9]", "", xmlValue(tmp[[4]]))
    c <- gsub("[^0-9]", "", xmlValue(tmp[[5]]))
    v <-ifelse(xmlSize(tmp) >= 6, gsub("[^0-9]", "", xmlValue(tmp[[6]])),0)
    tmp.data <-
      data.frame(date=d,open=o,height=h,low=l,close=c,volume=v)
    stock.data <- rbind(stock.data, tmp.data)
  }
  stock.data$date <- as.POSIXct(stock.data$date)
  stock.data$open <- as.character(stock.data$open)
  stock.data$height <- as.character(stock.data$height)
  stock.data$low <- as.character(stock.data$low)
  stock.data$close <- as.character(stock.data$close)
  stock.data$volume <- as.character(stock.data$volume)
  
  stock.data[order(stock.data$date),]
}

