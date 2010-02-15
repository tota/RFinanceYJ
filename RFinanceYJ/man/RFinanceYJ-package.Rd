\name{RFinanceYJ-package}
\alias{RFinanceYJ-package}
\alias{RFinanceYJ}
\docType{package}
\title{
Japanese stock market from Yahoo!-finance-Japan
}
\description{
Japanese stock market from Yahoo!-finance-Japan
}
\details{
\tabular{ll}{
Package: \tab RFinanceYJ\cr
Type: \tab Package\cr
Version: \tab 0.0.9\cr
Date: \tab 2010-01-23\cr
License: \tab BSD\cr
LazyLoad: \tab yes\cr
}
}
\author{
Yohei Sato <rfinance-dev@googlegroups.com>
}
\references{
}

\keyword{ package }
\seealso{
XML
}
\examples{
stock <- quoteStockTsData('4689.t')
head(stock)
stock <- quoteStockTsData('4689.t', since='2009-01-01')
head(stock)
plot(stock$date, stock$close, type="l", col="blue")
}
