#' Plot a Dataframe or pair of vectors
#' @description This function takes either a data frame or a pair of vectors as inputs,
#' plus other optional inputs. It organises the data for plotting and then calls
#' koiplotxy().
#' @param x either a vector or a data frame
#' @param y either a vector of the same length as x, or ignored if x is a data
#' frame
#' @param nmax maximum number of rows/entries of x to be plotted
#' @param ... extra optional arguments
#' @details This function exists to make it possible to plot a data frame quickly without
#' additional arguments if desired.
#'
#' If x is a data frame, then columns to plot are chosen using the choose_plot_columns()
#' function. Otherwise, colours, angles and alpha values are taken from ... if available,
#' and otherwise are set to default values. Width, height and curvature are taken from
#' ... if specified and otherwise set to default values. Any other arguments from ...
#' are passed to the plot() function via koiplotxy().
#' @examples
#' # basic use
#' koiplot(iris)
#'
#' ## comparison of scatterplot and koiplot
#'
#' # simulate data
#' set.seed(060317)
#' n <- 9
#' znoise <- 0.1
#' sigma <- 0.3
#' x_c <- runif(n)
#' y_c <- runif(n)
#' x <- x_c + rnorm(100*n, 0, sigma)
#' y <- y_c + rnorm(100*n, 0, sigma)
#' z <- x_c^2 + rnorm(100*n, 0, znoise)
#' w <- rep(factor(LETTERS[1:n]), 100)
#' col <- rainbow(n)[w]
#'
#' # comparison plots
#' par(mfrow=c(2,2))
#'
#' plot(x,y,col=col,pch=19,xlim=range(x),ylim=range(y))
#' koiplot(data.frame(x=x,y=y,z=z,w=w),xlim=range(x),ylim=range(y))
#' plot(x,y,col=col,pch=19,xlim=range(x),ylim=range(y))
#' koiplot(data.frame(x=x,y=y,z=z,w=w),xlim=range(x),ylim=range(y))
#' # note that the plots in the right column are not identical because the
#' # brushstrokes are drawn with random xnoise and ynoise. To get identical plots, it
#' # is necessary to set the seed to the same value before each call to koiplot()
#'
#' par(mfrow=c(1,1))
#'
#' # single plot
#' koiplot(x,y,angle=z,cols=w,alpha=0.2,width=2, height=1, curvature=0.1,
#'         bty="n",xlab="", ylab="",xaxt="n",yaxt="n")
#' @export
koiplot <- function(x, y, nmax=10000, ...){

  # to do : make nmax also work with the case when x and y are
  # not data frames

  extra_args <- list(...)

  if (is.data.frame(x)){
    plot_columns <- choose_plot_columns(x)
    x <- plot_columns$x
    y <- plot_columns$y
    angle <- plot_columns$angle
    alpha <- plot_columns$alpha
    cols <- plot_columns$cols
    if (is.null(extra_args$xlab)) extra_args$xlab <- plot_columns$names_x
    if (is.null(extra_args$ylab)) extra_args$ylab <- plot_columns$names_y

  }else{
    angle <- if(!is.null(extra_args$angle)){extra_args$angle}else{0}
    extra_args$angle <- NULL

    alpha <- if(!is.null(extra_args$alpha)){extra_args$alpha}else{0.1}
    extra_args$alpha <- NULL

    cols <- if(!is.null(extra_args$cols)){extra_args$cols}else{"blue"}
    extra_args$cols <- NULL
  }
  if (is.null(extra_args$width)){
    width <- (max(x, na.rm=T) - min(x, na.rm=T))*0.3
  }else{
    width <- extra_args$width
    extra_args$width <- NULL
  }
  height <- if(is.null(extra_args$height)){
    (max(y, na.rm=T)- min(y, na.rm=T))*0.1
  }else{
    extra_args$height
  }
  extra_args$height <- NULL
  curvature <- if(is.null(extra_args$curvature)){
    0}else{extra_args$curvature}
  extra_args$curvature <- NULL

  rescale_angles <- if(is.null(extra_args$rescale_angles)){T}else{
    extra_args$rescale_angles}
  extra_args$rescale_angles <- NULL

  args <- list(x=x,
               y=y,
               angle=angle,
               alpha=alpha,
               cols=cols,
               width=width,
               height=height,
               curvature=curvature,
               rescale_angles=rescale_angles)

  # before adding the extra arguments to plot(), now is the time to
  # make sure we are only plotting nmax points

  #if (length(x) > nmax){
  #  s <- sample(1:length(x), nmax)
  #  args <- lapply(args, function(u) u[s])
  #}

  if (length(extra_args) > 0){
    for (i in 1:length(extra_args)){
      args[[names(extra_args)[i]]] <- extra_args[[names(extra_args)[i]]]
    }
  }
  #print(args)
  if (is.null(args$xlim)){
    args$xlim <- range(x) + mean(args$width)*c(-1,1)
  }
  if (is.null(args$ylim)){
    args$ylim <- range(y) + mean(args$height)*c(-1,1)
  }

  args$nmax <- nmax

  # warning message about col
  if (!is.null(args[["col"]])){
    cat("Warning in koiplot(): the col argument does nothing. To specify colours, use cols\n")
  }

  do.call(koiplotxy, args)

}
