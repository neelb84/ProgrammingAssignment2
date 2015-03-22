## Put comments here that give an overall description of what your
## functions do
##The couple of functions makeCacheMatrix and cacheSolve creates a matrix and cache its inverse, this is to avoid calculating the inverse of the same matrix multiple times
## instead cache the inverse matrix and call when required without re-calculating it
##The first function makeCacheMatrix creates a special matrix that can cache its inverse
##The second function cacheSolve calculates the inverse of the matrix created above, if the inverse is already calculated then it returns the value from the cache

## Write a short comment describing this function
##Firstly the makeCacheMatrix is defined with a Matrix as an argument and the function first initializes the 'Inverse' matrix (yet to calculate it)
## Set function assigns the matrix x from makeCacheMatrix to the cached x and then initializes I to NULL in the makeCacheMatrix environment
##Next 3 set of functions, first returns the cached Matrix defined above, secondly sets cached inverse matrix to 'I' and lastly returns inverse 'I' cached in makeVector environment
##makeCacheMatrix  finally returns the list of functions defined in makeCacheMatrix

makeCacheMatrix  <- function(x = matrix()) {
I <- NULL
        set <- function(x) {
                x <<- x
                I <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) I <<- inverse
        getinv <- function() I
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
##Firstly assigns the inverse defined in makeCacheMatrix to I
## If I (inverse matrix) is already defined for the 'x' above, then the function returns the cached "I" and prints "getting cached data"
##Otherwise it assigns the'x' locally to "data" and use 'solve' to calculate the inverse of matrix 'x'  and sets it to the envitonment of 'x' 
## Finally returns matrix inverse 'I'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I
}
