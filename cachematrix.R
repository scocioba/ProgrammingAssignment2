#The makeCacheMatric function c

makeCacheMatrix <- function(x = matrix()) 
{
  #declares the dataCache variable as null initially. Will hold cached values after 
  dataCache <- NULL
  
  #creates the "matrix" in the current (working) environment
  set <- function(y) 
    {
      x <<- y
      dataCache <<- NULL
    }
  
  #gets the value of the matrix
  get <- function() x
  
  #inverts the matrix and stores it in the variable dataCache
  setMatrix <- function(inverse) dataCache <<- inverse
  
  #obtains the inverted matrix from cache
  getInverse <- function() dataCache
  
  #returns the created functions back to the working environment
  list( get = get,
        set = set,
        getInverse = getInverse,
        setMatrix = setMatrix)    
}



cacheSolve <- function(x, ...) 
{
  #recall the inverted "matrix" from the cache
  dataCache <- x$getInverse()
  
  #determine if a cache of the matrix exists and return it
  if (!is.null(dataCache)) 
    {
        message("getting cached data")
        return(dataCache)
    }
  
  #if the above is false, make a new matrix called "newMatrix"
  newMatrix <- x$get()
  
  #compute the inverse of the matrix
  dataCache <- solve(newMatrix, ...)
  
  #set the value of the inverted matrix to the cache 
  x$setMatrix(dataCache)
  
  #displays the cached matrix
  dataCache
}
