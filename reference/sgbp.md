# Methods for dealing with sparse geometry binary predicate lists

Methods for dealing with sparse geometry binary predicate lists

## Usage

``` r
# S3 method for class 'sgbp'
print(x, ..., n = 10, max_nb = 10)

# S3 method for class 'sgbp'
t(x)

# S3 method for class 'sgbp'
as.matrix(x, ...)

# S3 method for class 'sgbp'
dim(x)

# S3 method for class 'sgbp'
Ops(e1, e2)

# S3 method for class 'sgbp'
as.data.frame(x, ...)
```

## Arguments

- x:

  object of class `sgbp`

- ...:

  ignored

- n:

  integer; maximum number of items to print

- max_nb:

  integer; maximum number of neighbours to print for each item

- e1:

  object of class `sgbp`

- e2:

  object of class `sgbp`

## Details

`sgbp` are sparse matrices, stored as a list with integer vectors
holding the ordered `TRUE` indices of each row. This means that for a
dense, \\m \times n\\ matrix `Q` and a list `L`, if `Q[i,j]` is `TRUE`
then \\j\\ is an element of `L[[i]]`. Reversed: when \\k\\ is the value
of `L[[i]][j]`, then `Q[i,k]` is `TRUE`.

`==` compares only the dimension and index values, not the attributes of
two `sgbp` object; use `identical` to check for equality of everything.
