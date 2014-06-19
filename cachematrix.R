## These pair of functions catches the inverse of an invertible matrix
## 

## makeCatchMatrix creates a list of four functions
## set function sets the input matrix which is given as an argument to makeCatchMatrix function. It doesn't do anything. 
#  Even if you remove this function your overall result will not change
## get function takes the matrix which was set by set function 
## setinverse stores the inverse in the enclosing enviroment i.e. the enviroment in which makeCatchMatrix was defined
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

