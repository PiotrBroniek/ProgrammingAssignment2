## This function takes matrix and creates functions : "set", "get", "setinverse", 
## "getinverse" which are then passed as a list. Function "set" makes variable m
## that is used to cache inverse to NULL in global environment and changes variable x
## to y whenever new variable is input. Function "get" returns matrix x without changes.
## Function "setinverse" makes calculation of inverse matrix. Function "getinverse" 
## returns inverted matrix that was cached. 

makeCacheMatrix <- function(x = matrix()) {
              
              m <- NULL
              set <- function(y){
                                  x <<- y
                                  m <<- NULL
              }
              get <- function() x
              setinverse <- function(solve) m<<-solve 
              getinverse <- function() m
              list(set = set, get = get, setinverse = setinverse,
              getinverse = getinverse)
}


## This function computes the inverse of the inversable "matrix" 
## by using inputs from makeCacheMatrix above. Firstly function tries to use 
## "getinverse" function from cache. If cache is not NULL it sends message 
##"getting data" and returns inverted matrix under variable m from cache. If m is 
## empty it gets matrix with "get" and inverts it by function "solve". In last two 
## lines it stores result in cache by "setinverse" and returns inverted matrix to user

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                        message("getting data")
                        return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
