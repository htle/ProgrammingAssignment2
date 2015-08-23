## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Similar to the makeVector function, this function
## creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix,
## using solve(matrix) to return matrix inverse 
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## This function calculates the matrix inverse. 
## If the inverse has been calculated, it gets the inverse 
## from the cache. Otherwise, it calculates the inverse
## and assign the value to the cache via the setInverseMatrix
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverseMatrix(m)
  m
}
