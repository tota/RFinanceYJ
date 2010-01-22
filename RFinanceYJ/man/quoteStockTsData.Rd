\name{quoteStockTsData}
\alias{quoteStockTsData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 株価時系列データの取得
}
\description{
   株価時系列データの取得
}
\usage{
quoteStockTsData(x, since = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    <stock_code>.<market> ex: 4689.t
}
  \item{since}{
    Unmounting
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( x, since=NULL){
  stock.data <- data.frame(NULL)
  quote.url <- paste('http://table.yahoo.co.jp/t?s=', x)
  try( r <- xmlRoot(htmlTreeParse(quote.url)) )
  quote.table <- r[[2]][[1]][[1]][[16]][[1]][[1]][[1]][[4]][[1]][[1]][[1]]
  end <- xmlSize(quote.table)

  for(i in 2:end){
    tmp <- quote.table[[i]]
    d <- gsub("^([0-9]{4})([^0-9]+)([0-9]{1,2})([^0-9]+)([0-9]{1,2})([^0-9]+)",
              "\\1-\\3-\\5",
              xmlValue(tmp[[1]]))
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
