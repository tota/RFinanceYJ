\name{quoteStockXtsData}
\alias{quoteStockXtsData}

\title{
    quote Japanese stock market from Yahoo!-finance-Japan
}
\description{
    quote Japanese stock market from Yahoo!-finance-Japan
}

\usage{
quoteStockXtsData(x, ...)
}

\arguments{
  \item{x}{
    <stock_code>.<market> ex: 4689.t
  }
  \item{...}{
     arguments of quoteStockTsData
  }
}
\details{

}
\value{
    xts
}
\references{

}
\author{
  Yohei Sato <yokkuns@tkul.jp>
  Nobuaki Oshiro <akiaki5516@gmail.com>
  Shinichi Takayanagi <teramonagi@gmail.com>
}
\note{

}

\seealso{
quoteStockTsData
}

\examples{
    \dontrun{
        stock.xts <- quoteStockXtsData('4689.t')
    }
}