## Assignment 2: 
## write functions to get the inverse of a matrix 

## Create a list with the setters and getters required for the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) 						
			{
                x <<- y
                i <<- NULL
			}
        get <- function() x						
        setinv <- function(solve) i <<- solve	
        getinv <- function() i					
        list(set = set, get = get,				
             setinv = setinv,
             getinv = getinv)
}


## Retrieve the inverse matrix from the cache or solve it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

# sample call:
# myMatrix <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
# cacheSolve(myMatrix)
