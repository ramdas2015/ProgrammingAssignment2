## makeCacheMatrix : This function creates a special matrix object that
## can cache its inverse

## CacheSolve: This function computes the inverse of special matrix returned by
## makeCacheMatrix above

makeCacheMatrix <- function(x = numeric()) { 
  
  # holds the cached value or NULL if nothing is cached 
  # initially nothing is cached so set it to NULL 
  cache <- NULL 
  
  # store a matrix 
  setMatrix <- function(newValue) { 
    x <<- newValue 
    # since the matrix is assigned a new value, flush the cache 
    cache <<- NULL 
  } 
  
  
  # returns the stored matrix 
  getMatrix <- function() { 
    x 
  } 
  
  
  # cache the given argument  
  cacheInverse <- function(solve) { 
    cache <<- solve 
  } 
  
  
  # get the cached value 
  getInverse <- function() { 
    cache 
  } 
  
  # return a list. Each named element of the list is a function 
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse) 
} 



# The following function calculates the inverse of a "special" matrix created with  
# makeCacheMatrix 
cacheSolve <- function(y, ...) { 
  # get the cached value 
  inv <- y$getInverse() 
  
  if(!is.null(inv)) { 
    message("getting cached data") 
    return(inv) 
  } 
  # otherwise get the matrix, caclulate the inverse and store it in 
  # the cache 
  data <- y$getMatrix() 
  inv <- solve(data) 
  y$cacheInverse(inv) 
  
  # return the inverse 
  inv
} 
#Test Runs...
#a <- makeCacheMatrix( matrix(c(1,2,12,13), nrow = 2, ncol = 2) );
#summary(a);
#a$getMatrix();
#cacheSolve(a);
#cacheSolve(a);


