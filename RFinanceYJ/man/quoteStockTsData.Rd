\name{quoteStockTsData}
\alias{quoteStockTsData}

\title{
    quote Japanese stock market from Yahoo!-finance-Japan
}
\description{
    quote Japanese stock market from Yahoo!-finance-Japan
}
\usage{
quoteStockTsData(x, since = NULL, start.num = 0 )
}

\arguments{
  \item{x}{
    <stock_code>.<market> ex: 4689.t
}
  \item{since}{
    'YYYY-MM-DD' ex:'2009-01-01'
  }
  \item{start.num}{
    offset.  ex:50
  }
}
\details{

}
\value{
  A Data frame. ( $date, $open, $height, $low, $close, $volume )
  
}
\references{

}
\author{
  Yohei Sato <yokkuns@tkul.jp>
  Nobuaki Oshiro <akiaki5516@gmail.com>
}
\note{

}

\seealso{

}
\examples{
stock <- quoteStockTsData('4689.t')
head(stock)
stock <- quoteStockTsData('4689.t', since='2009-01-01')
head(stock)
plot(stock$date, stock$close, type="l", col="blue")
}
