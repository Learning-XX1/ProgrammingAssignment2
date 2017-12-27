## Put comments here that give an overall description of what your
## functions do
#
## Write a short comment describing this function
## l'objectif est de pouvoir mettre en cache le contenu d'une matrice. 





## -------------
#  to Test the code : 
#  source("cachematrix.R")
#  myMatrix = makecache(matrix(0:10, 10, 10))
# 



makecache <- function(x = matrix()) {

	# Initialisation du cache 
	cache <- NULL 


	setMatrix <- function(y) {
    x <<- y
    cache <<- NULL
  }

  getMatrix <- function() x
  
  getCache <- function() cache
  
  setCache <- function(inverse) cache <<- inverse

  list(setMatrix = setMatrix, getMatrix = getMatrix, setCache = setCache, getCache = getCache)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    cache <- x$getCache()
         
    if (!is.null(cache)) 
    {
    	message("Problem 201: The cache could not be loaded")
    	return(cache)
   } else {
    	dMatrix <- x$getMatrix()
    	cache <- solve(dMatrix, ...)
    	x$setCache(cache)
   		return(cache)
  }
  
}
