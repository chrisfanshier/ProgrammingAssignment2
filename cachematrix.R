## MakeCacheMatrix  takes as its input an invertable matrix
## and generates an object that can be taken as an argument in the
## following function cacheSolve, as well as defining functions that can
## be used to retrieve the input matrix ($get), retreive the inverse of 
## the input matrix ($getinverse) once it has been solved and 
## cached by cacheSolve, or define a input matrix ($set). 

## Creates object to be used as argument for cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  solve_matrix <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       solve_matrix = solve_matrix,
       getinverse = getinverse)

}


## Solves for inverse of input matrix, caches solution in memory for retreival

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
    I <- x$getinverse()
    if(!is.null(I)) {
      message("getting cached data")
      return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$solve_matrix(I)
    I
  
}
