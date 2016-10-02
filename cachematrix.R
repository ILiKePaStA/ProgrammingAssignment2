## Solve for the inverse of a matrix and store the solution in cache. When
## trying to find the inverse of that matrix in the future the solution is
## retrieved from cache rather than recalculated. 
## Example of use: sm <- makeCacheMatrix(m) ;  cacheSolve(sm)



## Creates a special "matrix" object that can cache its inverse. Returns
## functions to set and get the matrix and its inverse to the parent environment.
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolution <- function(solution) s <<- solution
    getsolution <- function() s
    list(set=set, get=get, setsolution=setsolution, getsolution=getsolution)
}


## Computes the inverse of a special "matrix" returned by makeCacheMatrix. 
## If the inverse has been already calculated it is retrieved from cache using
## getsolution. Once the solution is computed it stores it in cache using 
## setsolution.
cacheSolve <- function(x, ...) {
    s <- x$getsolution()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolution(s)
    s
}
