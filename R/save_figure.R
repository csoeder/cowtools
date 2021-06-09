#' Wrapper to save a .PNG 
#' 
#' @param img_obj image to be saved
#' @param fn file to write
#' @return nothing
#' @examples
#' plot.gg <- ggplot(pressure) + geom_point(aes(x=pressure, y=temperature))
#' save_figure(img_obj=plot.gg, fn="pressurePlot.png")
save_figure <- function(img_obj, fn) {
	print("potato")
	print(getwd())
	png(height =  500, width = 800, filename = fn)
	img_obj
	dev.off()
}
