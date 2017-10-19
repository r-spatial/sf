#' Methods for dealing with sparse geometry binary predicate lists
#' 
#' Methods for dealing with sparse geometry binary predicate lists
#' name sgbp
#' @export
#' @param x object of class \code{sgbp}
#' @param ... ignored
#' @param n integer; maximum number of items to print
#' @param max_nb integer; maximum number of neighbours to print for each item
#' @details \code{sgbp} are sparse matrices, stored as a list with integer vectors holding the \code{TRUE} indices. This means that for a dense, m x n matrix $Q$ and a list $L$, if $Q[i,j]$ is \code{TRUE} then $j$ is an element of \code{L[[i]]}. Vice versa: when $k$ is the value of \code{L[[i]][j]}, then $Q[i,k]$ is \code{TRUE}.
print.sgbp = function(x, ..., n = 10, max_nb = 20) {
  n = min(length(x), n)
  cat("Sparse geometry binary predicate list of length ", length(x), ", ", sep = "")
  cat("where the predicate was `", attr(x, "predicate"), "'\n", sep = "")
  if (n < length(x))
    cat(", first ", n, " elements:\n", sep = "")
  else
    cat("\n")
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
