# -------------
#  to Test the code, very easy : 
#  source("cachematrix.R")
#  myMatrix = makecache(matrix(0:10, 10, 10))





# makecache(): 
# This function help us create a cache for any matrix given as input 
	makecache <- function(x = matrix()) {

	# Initialisation of our cache
	# Same as with vectors, we make sure that our entity is set to Null
	cache <- NULL 

 
	# ---------
	# We are going to create seperatly Getters and Setters for : 
	# 1. The Matrix (Get,Set)
	# 2. The cache  (Get,Set)
	# ---------

	 # Function getMatrix to get the given matrix 
  	getMatrix <- function() x

	# Function setMatrix for editing the matrix
	setMatrix <- function(y) {
    x <<- y
    cache <<- NULL
    }

  	# function to help us get teh cache  
  	getCache <- function() cache

  	# function for getting the inverse of the cache (x) 
  	setCache <- function(inverse) cache <<- inverse

}

# cacheSolve(): 
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {


    ## Return a matrix that is the inverse of 'x'

    cache <- x$getCache() 

    # We verify if the cash is not Null, otherwise we display the cache
    if (!is.null(cache)) 
    {
    	message("Problem 201: The cache could not be loaded")
    	return(cache)
   } else {
    	
    	#we now can get the content of the matrix
    	myMatrix <- x$getMatrix()
    	#we make sure thanks to the the solve function to update our matrix in the cache
    	cache <- solve(myMatrix, ...)
    	# Same as before, we now get the inverse of the cache
    	x$setCache(cache)
   		# and dislay the cache
   		return(cache)
  }
  
}
