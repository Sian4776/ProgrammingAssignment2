## These functions calculate and return the inverse of a matrix supplied by parameter
## The inverse is stored in the cache in the first function, and this cache is 
## checked by the second function before attempting to calculate the inverse.

## create an environment in the parent of this environment to store the list of all 
## matrix and inverse pairs
inverse.env <- new.env()

## create the list to hold the matrices and their inverses - initially empty
assign('inverseCache', list(), envir=inverse.env)

## Take the matrix supplied and save the inverse in the cache
makeCacheMatrix <- function(x = matrix()) {
  
  ##calculate the inverse
  y <- solve(x)
  
  ## get the current size of the list, add one for next position
  lenCache <- length(get('inverseCache', envir=inverse.env)) + 1
  
  ##add this pair to the list of matrices and inverses in the next position
  inverse.env$inverseCache[[lenCache]] <- list(x,y)
  
  ## return the inverse
  return (y)
  
}

## This function is given a matrix.  If the matrix is found in the cache, then return 
## the pre-calcuated value of inverse.  If not, then calculate it (and save this value
## in the cache for future use).
cacheSolve <- function(x, ...) {

  ## set the boolean before starting to check the list
  bFound <- FALSE
  
  ## get the current size of the list, add one for next position
  lenCache <- length(get('inverseCache', envir=inverse.env)) + 1
  
  ## check the list of matrices to see if this matrix has already been calculated
  if (lenCache > 0)  {
    
    ## check each of the matrices in the list
    for (i in 1:lenCache) {
    
      ## does this matrix match?
      if (inverse.env$inverseCache[[i]][[1]] == x) {
        ## found match - return it's inverse
        bFound = TRUE
        y <- inverse.env$inverseCache[[i]][[2]]
        break
      }
    }
  }
  
  if (bFound == FALSE) {
    ## cache value was not found - calculate and add this to the cache for using next time 
    y <- makeCacheMatrix(x)
  }
  
  return (y)
}
