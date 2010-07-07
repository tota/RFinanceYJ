\name{quoteFXTsData}
\alias{quoteFXTsData}

\title{
    quote foreign exchange rate from Yahoo!-finance-Japan
}
\description{
    quote foreign exchange rate from Yahoo!-finance-Japan
}
\usage{
quoteFXTsData(x, since = NULL, start.num = 0, date.end = NULL, time.interval = 'daily')
}

\arguments{
  \item{x}{
    <currencypair_code> ex: USDJPY=X
  }
  \item{since}{
    'YYYY-MM-DD' ex:'2009-01-01'
  }
  \item{start.num}{
    offset.  ex:50
  }
  \item{date.end}{
     'YYYY-MM-DD' ex:'2009-01-01'
  }
  \item{time.interval}{
    Data frequency.Possible types are 'daily','weekly', or 'monthly'.
  }
}
\details{

}
\value{
  A Data frame. ( $date, $open, $height, $low, $close)
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

}
\examples{
fx <- quoteFXTsData("USDJPY=X")
head(fx)
fx <- quoteFXTsData("USDJPY=X",since="2009-01-01",date.end="2010-05-31",time.interval="monthly")
head(fx)
}
