## Put comments here that give an overall description of what your
## functions do

## This function is to create a "matrix" object that 
## can cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() (x)
        setInverse <- function(inverse) (inv<<- inverse)
        getInverse <- function() (inv)
        list(set = set, get = get, setInverse = setInverse, getInverse= getInverse)
}


## This function is to inverse the matrix returned by makeCacheMatrix(function above).
## This fuunction should also retrieve the inverse from the cache,
## if the inverse has already been calculated and the matrix has not changed.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                ## Return a matrix that is the inverse of 'x'
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}