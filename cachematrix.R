@@ -1,15 +1,31 @@
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix consists of set,get,setinv,getinv

makeCacheMatrix <- function(x= matrix()){
        inv <- NULL               ## initializing inverse as NULL
        set <- function(y){
                x <<- y
                inv <<- NULL 
        }
        get <- function() {x}     ##finction to get matrix x
        setInverse <- function(inverse){inv <<- inverse}
        getInverse <- function() {inv} ##function to obtain inverse of the matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve is used to get cache data

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x,...){        ##gets cache data
        inv <- x$getInverse()
        if(!is.null(inv)){            ##checking whether inverse is NULL
                message("getting cached data")
                return(inv)          ##returns inverse value
        }
        mat <- x$get()
        inv <- solve(mat, ...)       ##calculates inverse value
        x$setInverse(inv)
        inv  ## Return a matrix that is the inverse of 'x'
}

