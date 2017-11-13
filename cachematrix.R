## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function stores the matrix in a 3 dimensional array withInverse
# where withInverse[,,1] stores the matrix
# and withInverse[,,2] can store the inverse of the matrix
# I did not compute and store the inverse of the matrix in this function because
#  the instructions says it can store the inverse but not that it computes it.
# I use rows and cols to create the 3 dimensional array of the right size
# and then I set withInverse[,,1] equal to the input matrix 
makeCacheMatrix <- function(x = matrix()) {
    rows <- nrow(x)
    cols <- ncol(x)
    withInverse <<- array(rep(NA,2*rows*cols),c(rows,cols,2))
    withInverse[,,1] <<- x
}


## Write a short comment describing this function

# I first check to see if the matrix matches the cached matrix
# If not I create a new cached matrix and store the value of the input matrix and 
# then compute and store the inverse.
# If the input matrix matches the cached matrix, I check to see if the inverse has
# been computed. If not, I compute and store the inverse.
#
# Finally, I return the cached inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if (!(length(x) == length(withInverse[,,1]) && all(x == withInverse[,,1]))) {
        makeCacheMatrix(x)
        withInverse[,,2] <<- solve(withInverse[,,1])
    } else {
        if (is.na(withInverse[1,1,2])) {
            withInverse[,,2] <<- solve(withInverse[,,1])
        }
    }
    withInverse[,,2]
}
