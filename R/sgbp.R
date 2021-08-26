sgbp = function(x, predicate, region.id, ncol, sparse = TRUE) {
	ret = structure(x,
		predicate = predicate,
		region.id = region.id,
		ncol = ncol,
		class = c("sgbp", "list"))
	if (! sparse)
		as.matrix(ret)
	else
		ret
}

#' Methods for dealing with sparse geometry binary predicate lists
#' 
#' Methods for dealing with sparse geometry binary predicate lists
#' @name sgbp
#' @export
#' @param x object of class \code{sgbp}
#' @param ... ignored
#' @param n integer; maximum number of items to print
#' @param max_nb integer; maximum number of neighbours to print for each item
#' @details \code{sgbp} are sparse matrices, stored as a list with integer vectors holding the ordered \code{TRUE} indices of each row. This means that for a dense, \eqn{m \times n}{m x n} matrix \code{Q} and a list \code{L}, if \code{Q[i,j]} is \code{TRUE} then \eqn{j} is an element of \code{L[[i]]}. Reversed: when \eqn{k} is the value of \code{L[[i]][j]}, then \code{Q[i,k]} is \code{TRUE}.
print.sgbp = function(x, ..., n = 10, max_nb = 10) {
	n = min(length(x), n)
	hd = paste0("Sparse geometry binary predicate list of length ", length(x), ", ",
	 	"where the predicate was `", attr(x, "predicate"), "'")
	cat(strwrap(hd), sep = "\n")
	if (n < length(x))
		cat("first ", n, " elements:\n", sep = "")
	nbh = function(i, m) {
		X = x[[i]]
		end = if (length(X) > m) ", ..." else ""
		cat(" ", i, ": ", sep = "")
		if (length(X))
			cat(paste(head(X, m), collapse = ", "), end, "\n", sep = "")
		else
			cat("(empty)\n")
	}
	lapply(1:n, nbh, m = max_nb)
	invisible(x)
}

#' @name sgbp
#' @export
t.sgbp = function(x) {
	m = attr(x, "ncol")
	structure(sgbp(CPL_transpose_sparse_incidence(x, m),
		predicate = attr(x, "predicate"),
		region.id = as.character(1:m),
		ncol = length(x)),
		dim = NULL)
}

#' @name sgbp
#' @export
as.matrix.sgbp = function(x, ...) {
	nc = attr(x, "ncol")
	get_vec = function(x, n) { v = rep(FALSE, n); v[x] = TRUE; v }
	do.call(rbind, lapply(x, get_vec, n = nc))
}

#' @name sgbp
#' @export
dim.sgbp = function(x) {
	c(length(x), attr(x, "ncol"))
}

#' @export
Ops.sgbp = function(e1, e2) {
	if (.Generic != "!")
		stop("only ! operator is supported for sgbp objects")
	nc = 1:attr(e1, "ncol")
	sgbp(lapply(e1, function(x) setdiff(nc, x)),
		predicate = paste0("!", attr(e1, "predicate")),
		region.id = attr(e1, "region.id"),
		ncol = attr(e1, "ncol"))
}

#' @export
as.data.frame.sgbp = function(x, ...) {
	data.frame(row.id = rep(seq_along(x), lengths(x)), col.id = unlist(x))
}

setOldClass("sgbp")

setAs("sgbp", "sparseMatrix", function(from) {
	if (! requireNamespace("Matrix", quietly = TRUE))
		stop("package Matrix required, please install it first")
	idx = as.data.frame(from)
	Matrix::sparseMatrix(i = idx$row.id, j = idx$col.id, x = 1)
})
