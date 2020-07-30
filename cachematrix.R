## There are 2 functions
## makeCacheMatrix is a special object that stores a matrix and its inverse
## cacheSolve will retrieve the inverse of the matrix if it exists, or
##         calculate it otherwise.

## makeCacheMatrix holds a special matrix object.
## It creates a list with functions to:
##         set the value
##         get the value
##         set the inverse
##         get the inverse
## Its input is an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will only calculate the inverse of the matrix one time.
##         it will retrieve the existing one if it already exists.
## Its input is a makeCacheMatrix special object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Test
## x<-matrix(data=c(2,2,3,2),nrow=2,ncol=2) # This matrix is invertible.
## y<-makeCacheMatrix(x) # Creating the special matrix.
## cacheSolve(y) # Solving for the first time: inverse is calculated.
## cacheSolve(y) # Making sure it retrieves the value already stored.