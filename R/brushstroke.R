#' Draw a single brushstroke
#'
#' @param x x-coordinate of centre
#' @param y y-coordinate of centre
#' @param width width of rectangle
#' @param height height of rectangle
#' @param angle angle with positive x-axis
#' @param xnoise amount of noise to be added to vertical edges
#' @param ynoise amount of noise to be added to horizontal edges
#' @param nside number of points which can be adjusted by noise
#' on each of the four sides
#' @param col fill colour
#' @param curvature if positive, bends the middle of the rectangle
#' downwards. If negative, bends it upwards
#' @param border_col rectangle border colour (NA is transparent)
#' @param xstretch amount stretched in x direction
#' @param ystretch amount stretched in y direction; these are useful
#' when plotting with axes on different scales
#' @return void
#' @examples plot(1:10, 1:10, type="n", bty="n",xlab="",ylab="",yaxt="n", xaxt="n")
#' for (i in 1:10){
#'   brushstroke(i, 10, width=i/10, height=0.25, xnoise=0, ynoise=0, curvature=0)
#'   brushstroke(i, 9, width=0.5, height=i/10, xnoise=0, ynoise=0, curvature=0)
#'   brushstroke(i, 7, width=0.5, height=0.25, curvature=0, xnoise=(i-1)*0.05, ynoise=0)
#'   brushstroke(i, 6, width=0.5, height=0.25, curvature=0, xnoise=0, ynoise=0.01*(i-1))
#'   brushstroke(i, 5, width=0.5, height=0.25, curvature=0.1*(i-5), xnoise=0, ynoise=0)
#'   brushstroke(i, 3, width=0.5, height=0.25, curvature=0, xnoise=0, ynoise=0, angle=(i-5)/5 * pi/4)
#'   brushstroke(i, 2, width=0.5, height=0.25, curvature=0, xnoise=0, ynoise=0, col=rgb(0,0,1,i/10))
#'   brushstroke(i, 1, width=0.5, height=0.25)
#' }

#' @export
brushstroke <- function(x,
                        y,
                        width=1,
                        height=0.5,
                        angle=0,
                        xnoise=NULL,
                        ynoise=NULL,
                        nside=10,
                        col="blue",
                        curvature=NULL,
                        border_col= NA,
                        xstretch=1,
                        ystretch=1
){

  # x, y = location of rectangle centre
  # width, height = total width and height
  # angle = angle in degrees from horizontal, positive is counter-clockwise rotation
  # xnoise = noise added to vertical edges (so along x-direction)
  # ynoise = noise added to horizontal edges (along y-direction)
  # nside = number of control points per side used to draw
  # col = fill colour of polygon
  # curvature = amount of stretch of polygon long sides from horizontal

  w <- width
  h <- height

  if (is.null(xnoise)) xnoise <- w/5
  if (is.null(ynoise)) ynoise <- h/10
  if (is.null(curvature)) curvature <- 0.16*height

  # x and y coords for a rectangle centered at (0,0)
  xx1 <- seq(-w/2, w/2, length=nside)
  xx2 <- rep(w/2, nside)
  xx3 <- rev(xx1)
  xx4 <- rep(-w/2, nside)

  yy1 <- rep(-h/2, nside) + curvature/(-w/2)^2 * (xx1^2 - (-w/2)^2)
  yy2 <- seq(-h/2, h/2, length=nside)
  yy3 <- rep(h/2, nside) + curvature/(-w/2)^2 * (xx1^2 - (-w/2)^2)
  yy4 <- rev(yy2)

  yy1 <- yy1 + rnorm(nside, 0, ynoise)
  yy3 <- yy3 + rnorm(nside, 0, ynoise)

  xx2 <- xx2 + (-1)*abs(runif(nside)*xnoise) #*rep(c(1,-1), nside)[1:nside]
  xx4 <- xx4 + abs(runif(nside)*xnoise) #*rep(c(1,-1), nside)[1:nside]

  xx <- c(xx1,xx2,xx3,xx4)
  yy <- c(yy1,yy2,yy3,yy4)

  vertices <- rbind(xx, yy)
  vertices <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)),
                     nrow=2) %*% (vertices/c(xstretch, ystretch))
  vertices <- c(xstretch, ystretch) * vertices

  vertices <- vertices + c(x,y)

  polygon(vertices[1,], vertices[2,], col=col, border=border_col)

}
