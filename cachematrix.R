## Put comments here that give an overall description of what your
## functions do

## Function to create a list containing a function to set the value of the vector
## get the value of the vector, set the value of the inverse and get the value of
## the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # create an object to store the inverse of the matrix
        m <- NULL
        
        # Defining a function to create the inverse of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Defining a function to return the value of the matrix
        get <- function() x
        
        # Defining set and get functions for inverse
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        
        # Return list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        
        # Return the inverse of the matrix 
        m <- x$getinv()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # If inverse not cached, computing and displaying the inverse of the matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        
        # Return the inverse of the matrix
        m
}
