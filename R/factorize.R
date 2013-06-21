#'@export
factorize <- function(a, levels=NULL) {
	retval <- .Call(sprintf("factorize_%s", class(a)), a, levels)
	if (is.null(levels)) {
		attributes(retval) <- list(
			levels = paste(attr(retval, "levels")),
			class = "factor"
		)
	} else {
		attributes(retval) <- list(
			levels = paste(levels),
			class = "factor"
		)
	}
	retval
}

#'@export
interact <- function(factor1, factor2, levels=NULL) {
	stopifnot(inherits(factor1, "factor"))
	stopifnot(inherits(factor2, "factor"))
	if (is.null(levels)) {
		retval <- .Call("interact", factor1, factor2, NULL)
		attributes(retval) <- list(
			levels = paste(
				levels(factor1)[attr(retval, "level1", TRUE)], 
				levels(factor2)[attr(retval, "level2", TRUE)], 
				sep="."
			),
			class = "factor"
		)
	} else {
		stopifnot(length(levels) == 2)
		stopifnot(class(levels) == "list")
		stopifnot(class(levels[[1]]) == "integer")
		stopifnot(class(levels[[2]]) == "integer")
		retval <- .Call("interact", factor1, factor2, levels)
		attributes(retval) <- list(
			levels = paste(
				levels(factor1)[levels[[1]]], 
				levels(factor2)[levels[[2]]], 
				sep="."
			),
			class = "factor"
		)
	}
	retval
}