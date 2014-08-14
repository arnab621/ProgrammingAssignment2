## (August, 2014): 
## This is an example of caching the computationally expensive matrix inverse operation, so that   
## the inverse matrix can be retrieved from cache if already computed. 
## This is implemented using two functions: makeCacheMatrix and cacheSolve
## Note: This function assumes that the matrix is always invertible.

## The first function, makeCacheMatrix creates a list of functions for:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse using solve function
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The following function cacheSolve, calculates the inverse of the matrix created by the above function. However, 
## if the inverse has already been computed, it fetches the inverse from the cache and skips the computation. 
## Else, it calculates the inverse of the matrix and sets the value of the inverse via the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ## if inverse is already computed, it is fetched from the cached results  
  if(!is.null(inv)) {
    message("fetching cached data")
    return(inv)
  }
  matrix_data <- x$get()
  inv <- solve(matrix_data, ...)
  x$setinv(inv)
  inv
}

## Test Instructions:
## 1. Create a normal matrix (e.g: a<-matrix(c(10,20,30,40),2,2) creates a 2x2 matrix)
## 2. Call makeCacheMatrix by passing the matrix created in step 1 to create the list (i.e. b<-makeCacheMatrix(a))
## 3. Retrieve the matrix using the get function of list b. (b$get())
## 4. Now Test the caching:
##    a. cacheSolve(b) - No Caching in first run. The inverse gets computed
##    b. cacheSolve(b) - Cached inverse gets fetched.