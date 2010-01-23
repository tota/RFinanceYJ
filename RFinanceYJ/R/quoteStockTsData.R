quoteStockTsData <-
function( x, since=NULL){
  stock.data <- data.frame(NULL)
  quote.url <- paste('http://table.yahoo.co.jp/t?s=', x)
  try( r <- xmlRoot(htmlTreeParse(quote.url, error=xmlErrorCumulator(immediate=F))) , TRUE)
  quote.table <- r[[2]][[1]][[1]][[16]][[1]][[1]][[1]][[4]][[1]][[1]][[1]]
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

