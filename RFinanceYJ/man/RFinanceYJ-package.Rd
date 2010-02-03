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
Version: \tab 0.0.4\cr
Date: \tab 2010-01-23\cr
License: \tab New BSD License\cr
LazyLoad: \tab yes\cr
}
}
\author{
Yohei Sato <yokkuns@tkul.jp>
}
\references{

}

\keyword{ package }
\seealso{

}
\examples{
stock <- quoteStockTsData('4689.t')
stock <- quoteStockTsData('4689.t', since='2009-01-01')
}
