## These pair of functions catches the inverse of an invertible matrix
 

## makeCatchMatrix creates a list of four functions
## set function takes the input matrix as arguments. The makeCatchMatrix function takes the same arguments
## get function gets the matrix which was set by set function 
## setinverse sets the inverse in the enclosing enviroment i.e. the enviroment in which makeCatchMatrix was defined
## getinverse gets the inverse value from the setinverse function

makeCatchMatrix<-function(x=matrix()){
        m <- matrix(NA,nrow(x),ncol(x))
        set <- function(y) {
                x <<- y
                
                m <<- matrix(NA,nrow(x),ncol(x))
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function performs two main operations
## First it checks if inverse matrix(m) is null or not. If its null then compute the inverse and assign it to m.
## If inverse matrix is not null then recycle that matrix instead of computing inverse again.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(length(which(complete.cases(m)))!=0) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

