## This program is used to return inverse of a matrix. 
## But the functions first check if the inverse already exists, and if so 
## it returns the inverse from the cache, thus saving computaion time.
## If the cache does not have the inverse, 
## then the inverse is computed using the solve function

## makeCacheMatrix function is a function that creates a list of four functions.
## These functions set and get, sets the matrix and returns the matrix, 
## while the functions setinverse and getinverse sets the inverse of the matrix
## and returns it respectively. 

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL   ## sets the inverse to null
  
  set <- function(y) {
    x <<- y    ##  sets the matrix in the parent function
    i <<- NULL ## sets the inverse to null in the parent function
  }

  get <- function() x   ## returns the matrix when the function is called
  
  setinverse <- function(inv) i <<- inv  ## sets the inverse of the matrix, i.e. caches it
  
  getinverse <- function() i  ## returns the inverse of the matrix
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  ## creates a list of the four functions
  
}




## cacheSolve looks for the inverse of the matrix 
## in the object created using the above function
## if it finds an inverse, it returns the same, but
## if no inverse is found it calculates the inverse
## stores it in the above object and return the inverse matrix.

cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()  ## checks if the inverse exists in the cache
  if(!is.null(i)) {
    message("getting cached data") 
    return(i)         ## returns the inverse if found in the cache
  }
  
  ## if inverse is not found in the cache, 
  ## solve function is used to calculate the inverse
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)     ## set the inverse into the object
  i
}
