#' Prepare data for plotting
#' @description Puts arguments into a form in which koiplotxy() can be applied
#' @param x vector of x-coordinates
#' @param y vector of y-coordinates
#' @param angle vector of angles '
#' @param alpha vector of alpha values
#' @param cols vector of colours (character)
#' @param width vector of widths
#' @param height vector of heights
#' @param curvature vector of curvatures
#' @param rescale_angles boolean: if true, angles are to be rescaled to a specified range
#' @param max_angle max angle in range for angle rescaling (specified in degrees)
#' @param min_angle min angle in range for angle rescaling (specified in degrees)
#' @param brushstroke_xnoise amount of x-noise for brushstrokes (also a vector)
#' @param brushstroke_ynoise amount of y-noise for brushstrokes
#' @param brushstroke_border_col border colours for brushstrokes (NA = no borders plotted)
#' @details All arguments are recycled to the maximum length of x or y. If cols is numeric, a vector
#' of colours is created from it using rainbow().
#' @export
prepare_plot_data <- function(x,
                              y,
                              angle=NULL,
                              alpha=NULL,
                              cols="blue",
                              width=NULL,
                              height=NULL,
                              curvature=NULL,
                              rescale_angles = T,
                              min_angle = -45,
                              max_angle = 45,
                              brushstroke_xnoise=NULL,
                              brushstroke_ynoise=NULL,
                              brushstroke_border_col=NA){

  # this function takes plotting data and coerces each argument into
  # a vector of the correct length. It also rescales the alpha and
  # angle arguments into a specified range

  if (length(x) != length(y)) cat("Warning in prepare_plot_data(): x and y lengths differ\n")
  if (all(is.na(x))) stop("in prepare_plot_data(): x consists of all NAs\n")
  if (all(is.na(y))) stop("in prepare_plot_data(): y consists of all NAs\n")
  if (max(x, na.rm=T)-min(x, na.rm=T)==0) cat("Warning in prepare_plot_data(): width of brushstrokes is zero\n")
  if (max(y, na.rm=T)-min(y, na.rm=T)==0) cat("Warning in prepare_plot_data(): height of brushstrokes is zero\n")

  n <- max(length(x), length(y))

  # default values of missing arguments
  if (is.null(angle)) angle <- 0
  if (is.null(alpha)) alpha <- 0.1
  if (is.null(cols)) cols <- "blue"
  if (is.null(width)) width  <- 0.3*(max(x, na.rm=T)-min(x, na.rm=T))
  if (is.null(height)) height <- 0.1*(max(y, na.rm=T)-min(y, na.rm=T))
  if (is.null(curvature)) curvature <- 0

  # recycle arguments to length of x
  args <- c("y", "angle", "alpha", "cols", "width", "height", "curvature",
            "min_angle", "max_angle", "brushstroke_border_col")

  for (i in 1:length(args)){
    name <- args[i]
    assign(name, rep(get(name), length.out = n))
  }

  #rescale angles
  if (rescale_angles){
    if (max(angle, na.rm=T) != min(angle, na.rm=T)){
      angle <- min_angle +
      (max_angle-min_angle)*(angle-min(angle, na.rm=T))/(max(angle, na.rm=T)-min(angle, na.rm=T))
    }
    angle <- pi*angle/180
  }
  # if cols is a factor or character vector, convert it to colours
  # colour_column might be a factor or character variable


  if (!is.character(cols)){
    colour_column <- cols
    colour_column <- as.factor(colour_column)
    num_colours <- length(levels(colour_column))
    cols <- rainbow(num_colours)[as.numeric(colour_column)]
    cols <- rgb(t((1/2)*(col2rgb(cols)/255 + 1)))
    # should pastelize these as well
  }

  #print(cols)

  # add transparency to colours
  cols <- col2rgb(cols)/255
  alpha[alpha > 1] <- 1
  alpha[alpha < 0] <- 0

  cols <- rgb(t(cols), alpha=alpha)

  #print(cols)
  #print(alpha)

  list(x=x,
       y=y,
       angle=angle,
       alpha=alpha,
       cols=cols,
       width=width,
       height=height,
       curvature=curvature,
       brushstroke_xnoise=brushstroke_xnoise,
       brushstroke_ynoise=brushstroke_ynoise,
       brushstroke_border_col=brushstroke_border_col
  )

}
