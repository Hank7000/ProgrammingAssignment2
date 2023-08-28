##the 2 functions use R environments to store the data we're working on
##as functions without hthe need for using it as arguments 


  makeCacheMatrix <- function(x = matrix()) {
    # creates environment to store the matrix and its inverse
    env <- new.env()
    
    # initializes the environment with the matrix and a NULL inverse
    env$matrix <- x
    env$inverse <- NULL
    
    # sets anew matrix and invalidates the inverse
    set <- function(y) {
      env$matrix <- y
      env$inverse <- NULL
    }
    
    # obtains the matrix
    get <- function() env$matrix
    
    # setss the inverse
    setInverse <- function(inverse) env$inverse <- inverse
    
    # obtains the cached inverse
    getInverse <- function() env$inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }
  
}


## cacheSolve checks out if the inverse is stored, then if it is it just returns
##it, if it's not it computes it and then caches it 

cacheSolve <- function(x, ...) {
  cacheSolve <- function(x) {
    # is inverse acached already?
    inv <- x$getInverse()
    
    # already cached? give it
    if (!is.null(inv)) {
      return(inv)
    }
    
    # not cached?compute the inverse, caches it, and then gives it
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
  }
}
