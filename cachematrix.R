## Given a matrix, these two functions can create a matrix object 
## that can store its inverse for quick retrieval. Istead of solving the
## matrix everytime, one can use the getinverse function.

## makeCacheMatrix returns a list consisting of 4 functions to: 
##   1. set a matrix
##   2. get the matrix
##   3. set the inverse
##   4. get the inverse
##
## In both functions it is assumed the matrix is square and invertible
## such that solve(x) will not return an error

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y){
        x <<- y
        I <<- NULL
        ## if a new matrix is set, reset the inverse to NULL
    }
    get <- function() x
    
    setinverse <- function(inv_mat) I <<- inv_mat
    getinverse <- function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a 'matrix' object as described above
## in makeCacheMatrix
## First it will check if the inverse of the matrix has been cached.
## If not, it will find the inverse with solve() and store the inverse.
## It is assumed that the matrix is square and invertible. Errors with 
## solve() will arise is this is not the case.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    ## check if I has already been cached
    if (!is.null(I)) {
        message("getting cached data")
        return(I) 
        ## if I was cached, this exits cacheSolve
    }
    x_mat <- x$get()
    I = solve(x_mat, ...)
    x$setinverse(I)
    I
}
