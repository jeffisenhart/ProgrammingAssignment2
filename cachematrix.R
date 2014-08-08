# makeCacheMatrix
# Author: Jeff Isenhart
# makeCacheMatrix encapsulates a matix and offers inverse set/get functions
# As per assignment instructions, we can assume the matrix passed is one that
# has an inverse


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

# cacheSolve
# Author: Jeff Isenhart
# cacheSolve will return the inverse of the matrix encapsulated within x
# If the inverse of the matrix was not previously computed, cacheSolve computes it and
# stores it via x$setInverse method for fetching later via x$getInverse

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

# Trivial usage:
# o <- makeCacheMatrix(matrix(1:1,1))
# o$get() # verify we got somthing
# inv <- cacheSolve(o)
# inv  #NOTE: when inv prints, you should NOT see the cache message appear
# inv2 <- cacheSolve(o)
# inv2  #NOTE: when inv2 prints, you should see the cache message "getting cached inverse..." appear