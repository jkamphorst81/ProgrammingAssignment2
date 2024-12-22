## 12/22/2024
## Assignment module 3 of JHU R programming course
## Following the assignment, this code contains the following functions:
## 1: makeCacheMatrix: allows you to get and set a matrix, as well as to cache
## the inverse (setinverse), and to get the inverse. 
## 2: cachSolve: calculates the inverse of a matrix set by makeCacheMatrix and 
## uses the setinverse function of makeCacheMatrix to cache it. However, it 
## first checks if an inverse matrix has already been cached using the 
## getinverse function of makeCacheMatrix. If that is the case, it will retrieve
## the cached inverse matrix.

## matrix x is passed as object
makeCacheMatrix <- function(x = matrix()) { 
  ## i is the inverse matrix, it is set to NULL for now
  i <- NULL 
  
  ## set allows you to set a new matrix, the inverse I will be set to NULL
  ## using <<- operator to make objects available in parent environment
  set <- function(y) { 
    x <<- y
    i <<- NULL
  }
  ## get will print the current matrix x
  get <- function() x
  
  ## setinverse is used by cacheSolve to cache the inverse matrix
  ## using <<- to make it available in parent environnment
  setinverse <- function(inverse) i <<- inverse
  
  ## getinverse is used by cacheSlove to see if the inverse matrix has 
  ## already been cached, if so it will retrieve from cache.
  getinverse <- function() i
  
  ## returning a list of functions, so they can accessed by ..$set etc.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function cacheSolve will return the cached inverse matrix of a 
## makeCacheMatrix object if it exists (it checks using he getinverse function. 
## If it doesn't, it will generate the inverse matrix and cache it using the 
## makeCacheMtrix setinverse function. 

cacheSolve <- function(x, ...) {
  ## checking if the inverse matrix has already been cached
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## if an inverse matrix hasn't already been cached, it will generate one using
  ## the solve function. 
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
