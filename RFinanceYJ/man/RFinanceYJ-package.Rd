\name{RFinanceYJ-package}
\alias{RFinanceYJ-package}
\alias{RFinanceYJ}
\docType{package}
\title{
Japanese finance market from Yahoo!-finance-Japan
}
\description{
Japanese finance market from Yahoo!-finance-Japan
}
\details{
\tabular{ll}{
Package: \tab RFinanceYJ\cr
Type: \tab Package\cr
Version: \tab 0.2.0\cr
Date: \tab 2013-06-08\cr
License: \tab BSD\cr
LazyLoad: \tab yes\cr
}
}
\author{
Yohei Sato <rfinance-dev@googlegroups.com>
}

\keyword{ package }
\seealso{
XML
}
\examples{
stock <- quoteStockTsData('4689.t')
head(stock)
stock <- quoteStockTsData('4689.t', since='2013-01-01')
head(stock)
stock <- quoteStockTsData('4689.t', since='2013-01-01',time.interval='monthly')
head(stock)
plot(stock$date, stock$close, type="l", col="blue")
}
