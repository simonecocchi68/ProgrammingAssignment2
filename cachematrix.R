##
## cachematrix.R
##
## Author: Simone Cocchi simone.cocchi68@gmail.com
## last update: 22.01.2015
## Sent to Coursera - R Programming Course Programming assignment 2 

## The 'makeCacheMatrix' is a function which 
##  - declare one formal parameter 'x' of type matrix and initialize it as an empty matrix
##
##  - define one local (member) variable 'x_inverse' used to cache the inverse of 'x'
##    and initialize it as an empty matrix
##
##  - define four member function through which the caller can set and get the actual value 
##    of the member variable 'x' and 'x_inverse'
## 
##  - return a list through which the member function described above are exposed to the caller
##
## note: the object returned incapsulate member variable and member function of the cached matrix
## note also: the set member function reset the inverse (x_inverse <<- NULL), so when the cached matrix
##            change, the inverse matrix is newly computed.
## Usage example: 
##        > a <- matrix(c(2,3,5,6), nrow=2, ncol=2)
##        > a_cached_object <- makeCacheMatrix(a)
##        > a_inverse <- cacheSolve(a_cached_object)
##        > a_inverse
##        > a <- matrix(c(2,3,5,6,6,7,5,4,3), nrow=3, ncol=3)
##        > a_cached_object$set(a)
##        > a_inverse <- cacheSolve(a_cached_object)
##
makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) x_inverse <<- inverse
    getinverse <- function() x_inverse
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The 'chacheSolve' function
##  - declare one formal parameter 'x' which must be actualized whith a list R object 
##
##  - store the cached matrix contained in the object passed as parameter in a local variable
##    called 'x_inverse' 
##    IF the cached matrix is not null (already computed)  return it
##      
##    ELSE
##
##  - store the  original matrix to be inverted in a local variable called 'matrix_data
##  - compute the inverse matrix calling solve passing as parameter the local copy of the
##    original matrix cached
##  - cache the inverse of the matrix for future use
##  - return the inverse of the matrix
##    
cacheSolve <- function(x, ...) {
   x_inverse <- x$getinverse()
   if (!is.null(x_inverse)) {
        return (x_inverse)
   } 
   matrix_data <- x$get()
   x_inverse <- solve(matrix_data)
   x$setinverse(x_inverse)
  
  ## Return a matrix that is the inverse of the cached matrix 
  return(x_inverse)
}
