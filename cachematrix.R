# R Programming - Assignment 2
# The purpose of the makeCacheMatrix and cacheSolve custom functions are to cache the inverse of a matrix.
# The advantage in caching this function is that it avoids repeating computationally expensive functions.
# This is possible in R since it uses lexical scoping, which allows the creation of distinct and interdependent user defined objects.
# The setInverse and getInverse functions are defined using the <<- assignment operator so they are not exposed to the outside environment. 

  makeCacheMatrix <- function(x = matrix()) {

      xinv <- NULL 				# by setting xinv to null as a 'default' value, it can act as a flag to check if the matrix that is to be inversed, is already stored
      set <- function(y) {			# This function will set the matrix created by makeCacheMatrix function
	  x <<- y
	  xinv <<- NULL 			# As above, this is used as a flag to test if the matrix exists already. Defined twice as it's used by both the functions
      }

      get <- function() x 			# gets the input matrix
      setInverse <- function(inv) xinv <<- inv 	# sets the matrix one it's inversed
      getInverse <- function() xinv 		# gets the matrix one it's inversed				
      list(set = set, get = get,		# lists that the functions so that they can be used by the makeCacheMatrix
	       setInverse = setInverse,
	       getInverse = getInverse)
  }


  cacheSolve <- function(x, ...) {
      m <- x$getInverse() 			# gets the inversed matrix from object x
      if(!is.null(m)) { 			# if matix m is not null (!is.null) then return cached matrix
	  message("getting cached data")	# prints this notification is the cached matrix is being used
	  return(m)				# returns matrix m
      }
      data <- x$get() 				# if matrix m is null, this means that it hasn't been solved already and won't be in the cache
      m <- solve(data)				# solves matrix m
      x$setInverse(m) 				# sets matrix m in object x
      m 					# returns the inverse of matrix m
  }

  # The following code cane be used to test the functions

  testMatrix <- matrix(c(45.42738,15.28983,4.254731,
			 50.60238,20.82734,91.395909,
			 63.81244,47.90663,37.283218)
			,3,3)	# Create a sequence of numbers to create a test matrix
  print(testMatrix)				# prints the test matrix
  cachedMatrix <- makeCacheMatrix(testMatrix)	# generate the makeCacheMatrix object with this matrix
  inversedMatrix <- cacheSolve(cachedMatrix)	# from now on calculate or retrieve calculated inversion using the cacheSolve function
  print(inversedMatrix)				# prints the inversed test matrix

