## The first function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix())  # creats a function taking a matrix as its only argument
  s <- NULL 
  set <- function(y) { # sets the value of the matrix
    x <<- y 
    s <<- NULL 
  }
  get <- function() x # gets the value of the matrix
  setinverse <- function(solve) s <<- solve # sets the value of the matrix inverse
  getinverse <- function() s # gets the value of the matrix inverse
  list(set = set, get = get, # creates a list which stores the four functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function computes the inverse of the special "matrix" returned by the function above

cacheSolve <- function(x, ...) { # creates a function taking a matrix as its only argument
  s <- x$getinverse() # if a matrix inverse has already been calculated this gets it 
  if(!is.null(s)) { # checks if cacheSolve has run before
    message("getting cached data") 
    return(s) 
  } # otherwise
  data <- x$get() # gets the value of the input matrix
  s <- solve(data, ...) # calculates the input matrix inverse
  x$setinverse(s) # caches the input matrix inverse
  s # returns the inverse
}
