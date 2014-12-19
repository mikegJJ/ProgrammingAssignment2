## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
# These two functions do this

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  
    invm <- NULL  
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() { x }   
    setinvm <- function(solve)  { invm <<- solve }
    getinvm <- function() { invm } 
    list(get = get,          
         setinvm = setinvm,  
         getinvm = getinvm) 
}


## This function returns a matrix that is the inverse of matrix 'x' 

cacheSolve <- function(x, ...) {
    invm <- x$getinvm()            
    if(!is.null(invm)) { 
         message("getting cached data")
        return(invm)
    }
    data <- x$get()   
    invm <- solve(data, ...)
    x$setinvm(invm)    
    invm
    
}
