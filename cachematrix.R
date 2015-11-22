## The makeCacheMatrix stores the information of an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## 'm' variable is set to NULL
  m <- NULL
  
  ## function of 'y', determined by 'x' and 'm' is assigned to 'set' variable
  set <- function(y) {
    
    ## 'y' variable is assigned to 'x' in a different environment
    x <<- y
    
    ## 'm' variable is set to NULL in a different environment
    m <<- NULL
  }
  ## 'get' variable holds a function that returns the value of 'x' when called
  get <- function() x
  
  ## The 'setinverse' variable holds a function of the 'inverse' variable
  ## 'm' is set to the 'inverse' variable in a different environment
  setinverse <- function(inverse) m <<- inverse
  
  ## 'getinverse' holds the function that rerurns the value of 'm'
  getinverse <- function() m
  
  ##a list has combined all the functions created with their allocated names
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## cacheSolve function checks to see if an inverse has already been calculated
##in the above function (MakeCacheMatrix)
## if it has it returns the invesrse from the cache
## if not it calculates it

cacheSolve <- function(x, ...) {
  
  ## assign to 'm' to return the 'getinverse' function of the value of x 
  m <- x$getinverse()
  
  ##check if 'm' is NULL, if not the inverse has already been calculated
  if(!is.null(m)) {
    
    ##inform the use that the data has been stored in the cache
    message("getting cached data")
    
    ##print 'm' to get the cached inversed matrix
    return(m)
  }
  
  ##the last part of the function is used to when 'm' is NULL
  ##hence the system does not hold the inverse of this matix
  
  ## 'data' variable holds 'get' function of 'x'
  data <- x$get()
  
  ## 'm' variable stores the inverse calculated by the 'solve' funcion 
  m <- solve(data, ...)
  
  ## the new inverse is stored in cache in 'm' by using the 'setinverse' function
  x$setinverse(m)
  
  ## print 'm' variable
  m
}