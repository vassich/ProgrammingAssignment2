## 1. The makeCacheMatrix function creates a special "matrix" object that can cache its inverse, 
## that is really a list containing a function to:
## 1) set the value of the matrix;
## 2) get the value of the matrix;
## 3) set the inverse of a matrix;
## 4) get the inverse of a matrix.
## 2. The cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has not 
## changed), then the cacheSolve retrieves the inverse from the cache.


## Creating a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(data) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## Computing the inverse for special "matrix" or reterning it from cache. 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##the following lines are meant for testing
#set.seed(1) 
#my_matr <- matrix(rnorm(1000000), nrow = 1000, ncol = 1000)
#qq <- makeCacheMatrix(my_matr)
#slv <- solve(my_matr)
#cslv <- cacheSolve(qq)
#print(c(max(slv - cslv), min(slv - cslv)))
#cslv1 <- cacheSolve(qq)
#print(c(max(slv - cslv1), min(slv - cslv1)))
