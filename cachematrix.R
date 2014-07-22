##Finds the Inverse of a matrix
##makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.



##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse


makeCacheMatrix <- function(x = matrix()) 
{
	inverse_x <- NULL
    set <- function(y) 
    {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inverse_x <<-inverse
    getinverse <- function() inverse_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) 
    {
        message("getting cached inverse matrix")
        return(inverse_x)
    } 
    else 
    {
        inverse_x <- solve(x$get()) # solve function gives invers of the square matrix
        x$setinverse(inverse_x)
        return(inverse_x)
    }

}
