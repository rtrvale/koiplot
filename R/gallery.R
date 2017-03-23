#' Gallery of extra examples
#' @description A gallery of some examples of the use of koiplot(). May be
#' extended in future versions of the package.
#' @param what a string naming the plot to be shown, or else all plots in the
#' gallery will be shown.
#' @examples
#' gallery()
#' @export
gallery <- function(what=""){

opar <- par()$ask
if (what == "") par(ask=T)

if (what == "spiral" | what == ""){
  # spiral
  t <- seq(1,100,0.1)
  koiplot(t^4*sin(t),t^4*cos(t), cols=terrain.colors(1000)[as.factor(-t)], angle=t, width=t^3.8, height=t^3.8, xlim=5e7*c(-1,1),
          ylim=5e7*c(-1,1), main="spiral")
  }

if (what == "the_unblinking_eye" | what == ""){
# the unblinking eye
  x <- rnorm(1000)
  y <- rnorm(1000)

  koiplot(x,y, cols=grey((1000:1)/1000)[as.factor(x^2+y^2)],
        angle=rnorm(1000), rescale_angles=F,
        alpha=0.02, curvature=0.3, xlim=c(-3,3), ylim=c(-3,3),
        main = "the_unblinking_eye")

  koiplot(x/1.6,y/1.6, cols=rainbow(1000,0.9)[as.factor(x^2+y^2)],
        angle=rnorm(1000),
        rescale_angles=F, alpha=0.05, add=T, curvature=-0)
  }

if (what == "rainbow" | what == ""){
  # rainbow
  x <- runif(1000)*1.5 - 0.75
  y <- sqrt(1-x^2) + (runif(1000)-0.5)*0.2
  koiplot(x,y, cols=-x^2-y^2, angle=-x, xlim=0.6*c(-1,1),
          main="rainbow")
  }

suppressWarnings(par(ask = opar))
}
