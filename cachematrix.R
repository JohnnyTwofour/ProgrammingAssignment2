##Caches a matrix for use in a later function

makeCacheMatrix <- function(x = matrix()) { ##initialize x as a function argument
  m <- NULL                                 ##initialized as an empty object to be used later in the code
  
  set <- function(y) {  ##takes argument y
          x <<- y       ##the <<- assigns the y value to x in the parent environment (ie it can still be used once the set() function is complete)
          m <<- NULL    ##assigns m as an empty object in the parent environment, this clears any values that may be residing in the m variable
  }
  get  <- function() x                          ##the symbol x is not defined so is retreived from the parent environment
  setmatrix <- function(inverse) m <<- inverse  ##assigns the input matrix to the parent environment in the variable m
  getmatrix <- function() m                     ##assigns the getter for variable m
  
  list(set = set, get = get,  ##assigns each element to a list and returns it to the parent environment,
       setmatrix = setmatrix, ##naming the list elements allows for the use of $ to access functions once a matrix is stored in an element
       getmatrix = getmatrix) 
}

##Checks if the matrix is already inverted in variable m and, if FALSE, 
##inverts the matrix from the makeCacheMatrix function output storing the result in m

cacheSolve <- function(x, ...) {
  
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
  
## Return a matrix that is the inverse of 'x'
}
