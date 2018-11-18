## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function builds a set of functions and return them within a list

makeCacheMatrix <- function(x = matrix()) 
{ 
    inv <- NULL                             
    set <- function(y) 
    {                    
        x <<- y                             
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function populate and/or retrieve the inverse from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse()
    if(!is.null(inv)) 
    {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
