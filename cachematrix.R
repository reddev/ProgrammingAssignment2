## makeCacheMatrix and cacheSolve matrix together calculate the inverse of a matrix
## To improve speed of processing the inverse is first checked if present in memory if present the same is returned

##  makeCacheMarix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##  cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) 
{
  list<-makeCacheMatrix(x)
  m <- list$getinverse()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  data <- list$get()
  m <- solve(data)
  list$setinverse(m)
  m
}