## Programing Assignment #2
## The two functions below compute the inverse of a matrix suing the solve funtion and cache it for future use. 
## The first time the functions are run the inverse is computed using solve. Howver since solve is a costly funtion
## The second time it is run the value is retrived from cache.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL                                            # inv initilized to null
  
  set <- function(y) {                                   # seting the matrix
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x                                    # getting the matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # Below returns a list with functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  if(!is.null(inv)) {
    message("Retriving cached inverse.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}

## Testing the above functions
## > x = matrix (1:4, 2, 2)
## > z = makeCacheMatrix(x)
## > z$get()
##       [,1]  [,2]
## [1,]  1     3
## [2,]  2     4

## The first time we run cacheSolve the values will be set in cache.
## > cacheSolve(z)
##      [,1]   [,2]
## [1,] -2     1.5
## [2,]  1     -0.5

## All future runs of cacheSolve will retrive the inverse data from cache.
## > cacheSolve(m)
## Retriving cached inverse.
##      [,1]   [,2]
## [1,] -2     1.5
## [2,]  1     -0.5
## > 
