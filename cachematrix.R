## Calculate the inverse of a matrix and cache the value so that it can be 
## reused next time without cacluating it again


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {   
      
      inv <- NULL             # Empty the value of inverse
      
      set = function(y){      # Set the value of the matrix
            x <<- y
            inv <<- NULL
      }
      
      get = function() x   # Get the value of a matrix
      
      setinv = function(sol) inv <<- sol  # Cache the inverse of matrix
      
      getinv = function () inv  # Get the value of inverse matrix
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)  # Return a list of 4 functions
      
}


## Return the value of inverse matrix. If the inverse has already been calcualted, it 
## return the cached value. Otherwise, it calculate the inverse and cached the value

cacheSolve <- function(x, ...) {
      
      inv = x$getinv()     # Get the cached inverse matrix
      
      if (!is.null(inv)){   # If the cached is not empty, return the value directly
            message("getting cached data")
            return(inv)
      }
      
      data = x$get()   # assign the matrix to data
      inv = solve(data) # inverse the matrix
      x$setinv(inv) # Cache the inverse matrix      
      inv  # Return a matrix that is the inverse of 'x'
      
}
