## These two functions working together cache the inverse of a matrix. 
##Once the inverse is calculated then it can be looked up in the cache rather than recomputed.


## The makeCacheMatrix is a list containing a function to
##set the value of the vector, get the value of the vector, set the value of the inverse, get the value of the inverse
##This function works for square matrices. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x 
        setinv <- function(mean) inv <<- mean
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function calculates the inverse of the matrix created with the makeCacheMatrix function.
##If the inverse of the matrix was calculated before then the cached value  is used.
##If the inverse of the matrix is not stored in the cache then it is calculated and cached for future use. 

cacheSolve <- function(x) {  
        inv <- x$getinv()               
        if(!is.null(inv)) {              
                
                message("getting cached data") 
                return(inv)                       
                
        }
        data <- x$get()
        inv <- solve(data)   
        x$setinv(inv)           
        inv               
}
