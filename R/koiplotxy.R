#' Scatterplot using brushstrokes
#' @description Makes a scatterplot of two specified vectors using brushstrokes from the
#' brushstroke() function instead of points
#' @param x vector of x-coordinates
#' @param y vector of y-coordinates
#' @param angle vector of angles for the brushstrokes (or a number)
#' @param alpha vector of alpha values for the brushstroke colours (or a number)
#' @param cols vector of colours for the brushstrokes (or a single colour)
#' @param width vector of widths for the brushstrokes (or a number)
#' @param height vector of heights for the brushstrokes (or a number)
#' @param curavture ditto curvatures
#' @param min_angle minimum angle
#' @param max_angle maximum angle
#' @param brushstroke_xnoise vector of x-noise to be used for the brushstrokes
#' @param brushstroke_ynoise ditto y-noise
#' @param brushstroke_border_col ditto border colours
#' @param nmax maximum number of brushstrokes to be drawn (defaults to 10000 as more
#' becomes slow)
#' @param ... extra arguments to plot() such as xlim, ylim, xaxt, title etc.
#' @details The arguments are passed to prepare_plot_data(). Then nmax random values from x #' and y are chosen
#' for plotting. Then a plot is made and the brushstrokes are added using brushstroke().
#' @examples
#' koiplotxy(1:10, 1:10, cols=rep(c("red", "green"), 5),
#' angle=1:10, min_angle=0, max_angle=90,
#' brushstroke_border_col="blue",alpha=seq(0.1,1,0.1),
#' brushstroke_xnoise=0:9/10, brushstroke_ynoise=9:0/100,
#' curvature=0:9/10, xaxt="n", bty="n", col="orange",
#' main="Swatches")
#' # note that the "col" argument does nothing because plot() is not plotting any points
#' @export
koiplotxy <- function(x,
                      y,
                      angle=NULL,
                      alpha=0.1,
                      cols="blue",
                      width=NULL,
                      height=NULL,
                      curvature = NULL,
                      rescale_angles=T,
                      min_angle = -45,
                      max_angle = 45,
                      brushstroke_xnoise=NULL,
                      brushstroke_ynoise=NULL,
                      brushstroke_border_col=NA,
                      nmax=10000,
                      ...){

  # function for plotting raw data
  # x = numerical vector of x-coords, same for y
  # angle = numeric angles
  # alpha = transparency (scaled)
  # col = colours (vector of strings)
  # width, height : for individual brushstrokes
  # ... = extra arguments to plot()

  plot_data <- prepare_plot_data(x,
                                 y,
                                 angle,
                                 alpha,
                                 cols,
                                 width,
                                 height,
                                 curvature,
                                 rescale_angles,
                                 min_angle,
                                 max_angle,
                                 brushstroke_xnoise,
                                 brushstroke_ynoise,
                                 brushstroke_border_col
  )

  if (length(x) > nmax){
    cat("Choosing", nmax, "random rows. Change nmax to plot more rows\n")
  }
  if (nmax > length(x)){
    nmax <- length(x)
  }
  s <- sample(1:length(x), nmax)
  plot_data <- lapply(plot_data, function(u) u[s])


  for (name in names(formals(prepare_plot_data))){
    assign(name, plot_data[name][[1]])
  }

  plot(x,y, type="n", ...)
  usr <- par("usr")
  xstretch = usr[2] - usr[1]
  ystretch = usr[4] - usr[3]

  for (i in 1:length(x)){
    brushstroke(x=x[i],
                y=y[i],
                width=width[i],
                height=height[i],
                curvature = curvature[i],
                angle=angle[i],
                col=cols[i],
                border_col=brushstroke_border_col[i],
                xnoise=brushstroke_xnoise[i],
                ynoise=brushstroke_ynoise[i],
                xstretch=xstretch,
                ystretch=ystretch)
  }
}

#Example:
#koiplotxy(1:10, 1:10, cols=rep(c("red", "green"), 5),
#angle=1:10, min_angle=0, max_angle=90,
#brushstroke_border_col="blue",alpha=seq(0.1,1,0.1),
#brushstroke_xnoise=0:9/10, brushstroke_ynoise=9:0/100,
#curvature=0:9/10, xaxt="n", bty="n", col="blue",
#main="Swatches")
#
# note that the "col" argument does nothing because plot() is not plotting anything
