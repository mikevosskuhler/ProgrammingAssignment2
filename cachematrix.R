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
    im <- NULL #setting the cache to NULL
    set <- function(y) {
      x <<- y #safing the input matrix
      im <<- NULL 
    }
    get <- function() x #function returns the input data
    setinverse <- function(inverse) im <<- inverse #safe the result to the cache
    getinverse <- function() im #returns the content of the cache, will be NULL 
                                #if nothing has been safed
    list(set = set, get = get,  #return a list of functions
         setinverse = setinverse, #functions will be executed once the function
         getinverse = getinverse) #is called by cacheSolve
}


##cacheSOlve takes the list outputted by makeCacheMatrix and retreives the safed
##solution and return the soluation if solution != NULL.
##If no solution has been calculated yet the solution will be NULL
##If solution == NULL the inverse will be calculated.
##TO do so data is retreived using get and this data is passed to the solve func
##results from the solve function are safed using setinverse
cacheSolve <- function(x, ...) {
    im <- x$getinverse() #this function call requests the cache content
    if(!is.null(im)) {   #if the function has been called previously
      message("getting cached data") # the cache will contain the answer previously
      return(im)                      #calculated
    } #otherwise the function continues to
    data <- x$get() #request the data passed to makeCacheMatrix
    im <- solve(data, ...) #the data is passed to solve()-which calculates inverse
    x$setinverse(im) #the answer is safed to the cache using the setinverse() func
    im #then the answer is returned. 
}

##the matrix should be passed to the makeCacheMatrix function
##once the cacheSolve function is called with the output of makeCacheMatrix
##the inverse will be calculated and safed. Every next call will return the 
##cache content with a message: getting cached data


##the use of the <<- operator is because the makeCacheMatrix is defined outside 
##of the cacheSolve function. otherwise the assignment of the im and x variables 
##would occur outside of cachesolve in the enviroment were makeCacheMatrix was 
##created. 