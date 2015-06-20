##The function "makeCacheMatrix" 
  #a. gets/sets the value of the matrix from/to the catche
  #b. gets/sets the inverse of the matrix from/to the catche

###Returns a list with the below 4 functions...
  #1. set: It stores the matrix in the catche
  #2. get: It returns the matrix
  #3. setInverse: It stores the inverse of the matrix in the catche
  #4. getInverse: Returns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

  matInverse <- NULL
  
  ##Set the matrix 
  set <- function(mat)
  {
    x <<- mat
    matInverse <<- NULL
  }
  
  ##get the matrix
  get <- function() x
  
  setInverse <- function(i){
    matInverse <<- i
  }
  
  getInverse <- function() matInverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## "cacheSolve" function tries to get the inverse of the matrix.
#If the inverse is computed it returns from the catche
#else it computes & stores the value in the catche and returns the value

cacheSolve <- function(x, ...) {
  
  ##Check if the matrix inverse is already computed
  matInvesre <- x$getInverse()
  
  if(!is.null(matInvesre))
  {
    ##Value is already computed, so just return it
    message("Returning the catched value")
    return(matInvesre)
  }
  
  ##if we have reached here the inverse is NOT already computed.
  #Get the matrix
  mat <- x$get()
  
  #Compute the inverse
  matInvesre <- solve(mat)
  
  #Cache the inverse
  x$setInverse(matInvesre)
  
        
  ## Return a matrix that is the inverse of 'x'
  matInvesre
    
}
