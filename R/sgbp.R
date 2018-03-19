sgbp = function(x, predicate, region.id, ncol) {
	structure(x,
		predicate = predicate,
		region.id = region.id,
		ncol = ncol,
		class = "sgbp")
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
  cat("Sparse geometry binary predicate list of length ", length(x), ", ", sep = "")
  cat("where the predicate was `", attr(x, "predicate"), "'\n", sep = "")
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
