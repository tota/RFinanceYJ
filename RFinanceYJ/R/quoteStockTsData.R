quoteStockTsData <-
function( x, since=NULL){
  stock.data <- data.frame(NULL)
  quote.url <- paste('http://table.yahoo.co.jp/t?s=', x)
  try( r <- xmlRoot(htmlTreeParse(quote.url, error=NULL)) , TRUE)
  quote.table <- r[[2]][[1]][[1]][[16]][[1]][[1]][[1]][[4]][[1]][[1]][[1]]
  end <- xmlSize(quote.table)

  for(i in 2:end){
    tmp <- quote.table[[i]]
    d <- gsub("^([0-9]{4})([^0-9]+)([0-9]{1,2})([^0-9]+)([0-9]{1,2})([^0-9]+)",
              "\\1-\\3-\\5",
              iconv(xmlValue(tmp[[1]]), "EUC-JP", "UTF-8", ""))
    tmp.data <-
      data.frame(date=as.POSIXct(d),
                 open=gsub("[^0-9]", "", xmlValue(tmp[[2]])),
                 height=gsub("[^0-9]", "", xmlValue(tmp[[3]])),
                 low=gsub("[^0-9]", "", xmlValue(tmp[[4]])),
                 close=gsub("[^0-9]", "", xmlValue(tmp[[5]])),
                 volume=gsub("[^0-9]", "", xmlValue(tmp[[6]]))
                 )
    stock.data <- rbind(stock.data, tmp.data)
  }
  stock.data
}

