\name{quoteFundTsData}
\alias{quoteFundTsData}

\title{
    quote Japanese investment trust fund from Yahoo!-finance-Japan
}
\description{
    quote Japanese investment trust fund from Yahoo!-finance-Japan
}
\usage{
quoteFundTsData(x, since = NULL, start.num = 0, date.end = NULL, time.interval = 'daily')
}

\arguments{
  \item{x}{
    <investmenttrust_code> ex: 11311023
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

\value{
  A Data frame. ( $date, $constant.value, $NAV )
}

\author{
  Yohei Sato <yokkuns@tkul.jp>
  Nobuaki Oshiro <akiaki5516@gmail.com>
  Shinichi Takayanagi <teramonagi@gmail.com>
}

\examples{
fund <- quoteFundTsData("11311023")
head(fund)
fund <- quoteFundTsData("11311023",since="2009-01-01",date.end="2010-05-31",time.interval="monthly")
head(fund)
}
