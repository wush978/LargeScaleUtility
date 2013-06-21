#! /usr/bin/Rscript

library(LargeScaleUtility)
n <- 10^7
a <- sample(letters, n, TRUE)
system.time({
	a2 <- factorize(a, letters)
})
system.time({
	a3 <- factorize(a, letters, 2)
})
stopifnot(all.equal(a2, a3))

system.time({
	a2 <- factorize(a)
})
system.time({
	a3 <- factorize(a, thread_number = 2) 
})
stopifnot(all.equal(as.character(a2), as.character(a3)))
