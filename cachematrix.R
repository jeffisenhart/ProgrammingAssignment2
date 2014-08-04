# makeCacheMatrix encapsulates a matix and offers an inverse function on that matrix
# The inverse function will cache the current inverse to save computation time

# Trivial usage:
# o <- makeCacheMatrix(matrix(1:1,1))
# o$get() # verify we got somthing
# inv <- cacheSolve(o)
# inv  #NOTE: when inv prints, you should NOT see the cache message appear
# inv2 <- cacheSolve(o)
# inv2  #NOTE: when inv2 prints, you should see the cache message "getting cached inverse..." appear

makeCacheMatrix <- function( x = matrix() ){
      i <- NULL
      set <- function( y ){
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse){
            i <<- inverse
      }
      getInverse <- function() i
      list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function ( x, ... ){
      i <- x$getInverse()
      if( !is.null(i) ){
            message("getting cached inverse...")
            return(i)
      }
      data <- x$get()
      i <- solve(data)
      x$setInverse(i)
      i
}