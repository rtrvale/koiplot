% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/koiplot.R
\name{koiplot}
\alias{koiplot}
\title{Plot a Dataframe or pair of vectors}
\usage{
koiplot(x, y, nmax = 10000, add = F, ...)
}
\arguments{
\item{x}{either a vector or a data frame}

\item{y}{either a vector of the same length as x, or ignored if x is a data
frame}

\item{nmax}{maximum number of rows/entries of x to be plotted}

\item{add}{if TRUE, add this plot over current plot (not recommended because
the new plot will automatically be on top, but sometimes useful)}

\item{...}{extra optional arguments}
}
\description{
This function takes either a data frame or a pair of vectors as inputs,
plus other optional inputs. It organises the data for plotting and then calls
koiplotxy().
}
\details{
This function exists to make it possible to plot a data frame quickly without
additional arguments if desired.

If x is a data frame, then columns to plot are chosen using the choose_plot_columns()
function. Otherwise, colours, angles and alpha values are taken from ... if available,
and otherwise are set to default values. Width, height and curvature are taken from
... if specified and otherwise set to default values. Any other arguments from ...
are passed to the plot() function via koiplotxy().
}
\examples{
# basic use
koiplot(iris)

## comparison of scatterplot and koiplot

# simulate data
set.seed(060317)
n <- 9
znoise <- 0.1
sigma <- 0.3
x_c <- runif(n)
y_c <- runif(n)
x <- x_c + rnorm(100*n, 0, sigma)
y <- y_c + rnorm(100*n, 0, sigma)
z <- x_c^2 + rnorm(100*n, 0, znoise)
w <- rep(factor(LETTERS[1:n]), 100)
col <- rainbow(n)[w]

# comparison plots
par(mfrow=c(2,2))

plot(x,y,col=col,pch=19,xlim=range(x),ylim=range(y))
koiplot(data.frame(x=x,y=y,z=z,w=w),xlim=range(x),ylim=range(y))
plot(x,y,col=col,pch=19,xlim=range(x),ylim=range(y))
koiplot(data.frame(x=x,y=y,z=z,w=w),xlim=range(x),ylim=range(y))
# note that the plots in the right column are not identical because the
# brushstrokes are drawn with random xnoise and ynoise. To get identical plots, it
# is necessary to set the seed to the same value before each call to koiplot()

par(mfrow=c(1,1))

# single plot
koiplot(x,y,angle=z,cols=w,alpha=0.2,width=2, height=1, curvature=0.1,
        bty="n",xlab="", ylab="",xaxt="n",yaxt="n")
}

