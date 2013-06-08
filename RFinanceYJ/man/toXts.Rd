\name{toXts}
\alias{toXts}

\title{
    convert to xts from DataFrame
}
\description{
    convert to xts from DataFrame
}
\usage{
toXts(stock.df)
}

\arguments{
  \item{stock.df}{
      DataFrame
  }
}

\value{
    xts
}

\author{
  Yohei Sato <yokkuns@tkul.jp>
  Nobuaki Oshiro <akiaki5516@gmail.com>
  Shinichi Takayanagi <teramonagi@gmail.com>
}

\examples{
    \dontrun{
        stock.df <- quoteStockTsData('4689.t')
        stock.xts <- toXts(stock.df)
    }
}