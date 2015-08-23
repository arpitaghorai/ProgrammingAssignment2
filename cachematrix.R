## The two functions below computes the inverse of a matrix. 
## Compared to computing the inverse directly using the function solve, the two functions enable caching the inverse so that 
## if the contents of a matrix of interest are not changing, the inverse can just be looked up in the cache rather than recomputed.
## This is really beneficial since matrix inversion is usually a costly computation. 



## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## The returned object is a list of functions used to set and read a stored matrix and also to set and read the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {	
## Usage: A<-makeCacheMatrix(matrixname)
## Each function in the list can be accessed by A$functioname(args), example A$set(newmatrixname) or A$get()
				
	invmat <- NULL						## Assign NULL to the inverse when a new matrix is called
      set <- function(y) {					## set function sets a new matrix in the main function (makeCacheMatix) using <<- operator      			
		x <<- y					
            invmat <<- NULL
        	}
  	get <- function() x					## get function returns the matrix stored in the main function
     	setinv <- function(inv) invmat <<- inv		## setinv sets inv to invmat in the main function. 
									## Note: It doesn't calculate the inverse, simply store the matrix inv (which is not necessarily the inverse)      
	getinv <- function() invmat				## getinv returns the invmat matrix stored in the main function
      list(set = set, get = get,				## list returned by makeCacheMatrix
      	setinv = setinv,
            getinv = getinv)
}


## cacheSolve takes the object where makeCacheMatrix is stored and returns the inverse of the matrix that has been set in makeCacheMatrix

cacheSolve <- function(x, ...) {
   	## Return a matrix that is the inverse of 'x'
	invmat <- x$getinv()					## Read the inverse matrix
      if(!is.null(invmat)) {
      	message("getting cached data")		## Inverse matrix not null implies the matrix (and hence its inverse) has not been modified.  
            return(invmat)					## Therefore the stored inverse is returned without actual calculation
        	}
   	data <- x$get()						## Else, read the matrix that has been set in makeCacheMatrix
      invmat <- solve(data)					## Compute the inverse
      x$setinv(invmat)						## Set the inverse matrix in makeCacheMatrix to the computed inverse
      invmat							## Returns the inverse matrix
}
