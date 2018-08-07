## Put comments here that give an overall description of what your
## functions do
##this is a repurposed version of the makevector and cachemean functions 
##published by Roger Peng as part of week 3 of the R programming coursera course


#makeCacheMatrix produces 4 functions set, get, setinverse, and getinverse
##the set functions safes the input 
##the get function allows to retreive input that has been safed
##the setinverse function allows the safing of the inverse once calculated
##the get inverserse returns the inverse if calculated. else it returns the 
##pre-set value NULL
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##cacheSOlve takes the list outputted by makeCacheMatrix and retreives the safed
##solution and return the soluation if solution != NULL.
##If no solution has been calculated yet the solution will be NULL
##If solution == NULL the inverse will be calculated.
##TO do so data is retreived using get and this data is passed to the solve func
##results from the solve function are safed using setinverse
cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im)) {
      message("getting cached data")
      return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}

##the use of the <<- operator is because the makeCacheMatrix is defined outside 
##of the cacheSolve function. otherwise the assignment of the im and x variables 
##would occur outside of cachesolve in the enviroment were makeCacheMatrix was 
##created. 