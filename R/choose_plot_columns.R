#' Choose which columns of a data frame to plot
#' @description This function chooses the columns to plot when a data frame
#' is passed to the koiplot() function.
#' @details This function takes a data frame as input. It chooses
#' \code{x} and \code{y} to be the first two numeric columns
#' encountered, starting from the left. It chooses the third numeric
#' column (if there is one) to be the angle variable. It chooses the
#' first non-numeric column to be the colour (after converting to a factor
#' and mapping with the rainbow() function) but if there is more than one
#' non-numeric column then it chooses the one with the smallest number
#' of distinct values among those which have at least 2 distinct values.
#'
#' The function imputes NA values in \code{x} and \code{y} by the value
#' halfway between the max and min. It imputes NA values in the angle column
#' by the mean of the other angles. It does not do imputation in the colours.
#'
#' If one or more of these columns consists entirely of NAs then the
#' function will fail.
#' @param X a data frame
#' @param default_angle the angle at which to plot the brushstrokes
#' @param default_alpha the alpha value (transparency) of the brushstroke colours
#' @param trace debugging flag
#' @return a list with components x, y, angle, alpha, cols (colours),
#' names_x (name of x column),
#' names_y (name of y column)
#' @examples X <- data.frame(a=1:2, b=c(NA,4), c=factor(5:6), d=7:8)
#' choose_plot_columns(X)
#' X$b[2] <- NA
#' choose_plot_columns(X) # fails with warning message
#' @export
choose_plot_columns <- function(X,
                                default_angle=0,
                                default_alpha=0.15,
                                trace=T){

  if (typeof(X) != "list") stop("Input should be a data frame\n")
  if (ncol(X) < 2) stop("Data frame inputs needs to have at least 2 columns\n")

  #X <- X[sample(1:nrow(X)),] # scrambles plotting order

  numeric_cols <- unlist(lapply(X, is.numeric))
  if (sum(numeric_cols) < 2) stop("Need at least two numeric columns for plotting\n")

  x <- X[,which(numeric_cols)[1]]
  y <- X[,which(numeric_cols)[2]]

  names_x <- names(X)[which(numeric_cols)[1]]
  names_y <- names(X)[which(numeric_cols)[2]]

  if (trace){
    cat("x: ", names(X)[which(numeric_cols)[1]], "\n", sep="")
    cat("y: ", names(X)[which(numeric_cols)[2]], "\n", sep="")
  }
  # no need to save original variables as we are not rescaling here
  #x_original <- x
  #y_original <- y

  x[is.na(x)] <- 0.5*(max(x, na.rm=T)-min(x, na.rm=T)) + min(x, na.rm=T)
  y[is.na(y)] <- 0.5*(max(y, na.rm=T)-min(y, na.rm=T)) + min(y, na.rm=T)

  angle <- default_angle
  alpha <- default_alpha

  if (sum(numeric_cols) >= 3){
    angle <- X[,which(numeric_cols)[3]]
    angle[is.na(angle)] <- mean(angle, na.rm=T)
  }
  if (trace) cat("angle: ", names(X)[which(numeric_cols)[3]], "\n")

  #if (sum(numeric_cols) >= 4){
  #  alpha <- X[,which(numeric_cols)[4]]
  #  alpha[is.na(alpha)] <- mean(alpha, na.rm=T)
  #}
  #if (trace) cat("transparency: ", names(X)[which(numeric_cols)[4]], "\n", sep="")

  if (sum(!numeric_cols) >= 1){
    colour_column <- X[,which(!numeric_cols)[1]]

    # search for a numeric column with few values (so no ID cols)
    nnc <- which(!numeric_cols)
    unique_count <- apply(X[,nnc, drop=F], 2, function(x) length(unique(x)))
    if (any(unique_count > 1)){
      colour_column_index <- nnc[which(unique_count ==
                                         min(unique_count[unique_count >1]))[1]]
      colour_column <- X[,colour_column_index]
    }

    if (trace) cat("colour: ", names(X)[colour_column_index],"\n", sep="")
    # colour_column might be a factor or character variable
    colour_column <- as.factor(colour_column)
    num_colours <- length(levels(colour_column))
    cols <- rainbow(num_colours)[as.numeric(colour_column)]
    cols <- rgb(t((1/2)*(col2rgb(cols)/255 + 1)))

  }else{
    cols <- rep("blue", length(x))
  }

  list(x=x,y=y,angle=angle,alpha=alpha,cols=cols,names_x=names_x,
       names_y=names_y)
}
