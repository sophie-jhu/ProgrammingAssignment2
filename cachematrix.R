## The following two functions aim to reduce the time taken to run computations on matrices. They will cache the inverse of a matrix, which will allow the user to look up the inverse value of a matrix in that cache (if such a cache exists) instead of having to recompute them.

## The first function, makeCacheMatrix, creates a matrix object that can cache the inverse of its input.

makeCacheMatrix <- function(x = matrix()) { #defines the default object as a matrix
  y <- NULL #creates an object that will hold the inverse matrix
  set <- function(z) { ## Defines the set function to assign
    x <<- z           ## a new value for the matrix, x, in the parent environment
    y <<- NULL #if a new matrix is entered, clears the object holding the old inverse matrix
  }
  get <- function() x #defines the get function as one that returns the value of the initial matrix argument
  setinverse <-function(solve) y <<- solve #assigns the value of y in the parent environment (see <<)
  getinverse <-function() y #gets the value of the inverse matrix, y, where it is called
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) ##builds out four functions allowing you to later call the 
  ##function using the $ operator
}


## The second function, cacheSolve, calculates the inverse of a matrix but first checks to see if that inverse matrix already exists as a cache, in order to save computing time. If a cache exists, it takes the inverse matrix from there and skips computation. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  y <-x$getinverse() #Checks to see if there already exists a cache copy of the inverse matrix
  if(!is.null(y)){ #If there IS an existing cache
    message("getting cached data") #Function lets user know it is pulling cached data
    return(y)
  }
  data <-x$get() #If there IS NOT an existing cache
  y <-solve(data,...) #Return the inverse of the provided matrix, x
  x$setinverse(y) #Store that inverse matrix in an object y
  y #Print the inverse matrix
}
