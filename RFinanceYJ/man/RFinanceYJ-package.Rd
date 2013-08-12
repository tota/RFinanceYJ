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
Version: \tab 0.3.1\cr
Date: \tab 2013-08-12\cr
License: \tab BSD 3-clause License\cr
LazyLoad: \tab yes\cr
}
}
\author{
Yohei Sato <yohei0511@gmail.com>
}

\keyword{ package }
\seealso{
XML
}
\examples{
\dontrun{
stock <- quoteStockTsData('4689.t')
head(stock)
stock <- quoteStockTsData('4689.t', since='2013-01-01')
head(stock)
stock <- quoteStockTsData('4689.t', since='2013-01-01',time.interval='monthly')
head(stock)
plot(stock$date, stock$close, type="l", col="blue")
}
}
