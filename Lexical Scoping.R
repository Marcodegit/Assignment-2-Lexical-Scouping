> setwd('Users/marco/Desktop/Lexical Scouping')
## 
## x as a matrix
## assuming the matrix supplied is invertible.
## set the value "z" as a null
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) z <<- inverse
  getInverse <- function() z 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
##
##changed "mean" to "solve" and "m" to "z" 
cacheSolve <- function(x, ...) {
  ## Return the inverse of 'x'
  z <- x$getInverse()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  mat <- x$get()
  z <- solve(mat,...)
  x$setInverse(z)
  z
}