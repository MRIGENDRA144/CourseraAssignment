makeCacheMatrix <- function(x = matrix()) 
  {k <- NULL
  set <- function(y)
    { x <<- y
    k <<- NULL  }
  get <- function() x
  setInverse <- function(MatrixSOLUTION) k <<- MatrixSOLUTION
  getInverse <- function() k
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }



cacheSolve <- function(x, ...) { 
  k <- x$getInverse()
  if(!is.null(k)) {
    message("Get Cache data back") 
    return(k) 
    }
  m <- x$get()
  k <- solve(m, ...)
  x$setInverse(k)
  k
}
