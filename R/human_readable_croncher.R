#' Reformat very large/small numbers as human-readable
#' 
#' @param num_in A number.
#' @return the number as string, human friendly
#' @examples
#' human_readable_croncher(100000000)
#' human_readable_croncher(1/100000000.)
human_readable_croncher <- function(num_in) {
	dig <- 3
	num_out <- formatC(num_in, digits=dig, format='g') %>% as.numeric() %>% sitools::f2si()
	return(num_out)
}
