# These functions, when applied together, are capable of taking a square matrix as an argment
# and obtaining its inverse matrix. To avoid the repeated calculations of inverses, after the 
# first time a matrix inverse is computed, that inverse will be stored as cached data. Next time,
# if a matrix for which the inverse has already been calculaetd is provided as an argument again,
# the function won't calculate the inverse rather the cached data will be retrieved. This may reduce
# the computation burden of recomputing an already computed inverse.

makeCacheMatrix<- function (x = matrix()) { 
        inv <- NULL
        set <-function (y){
          x <<- y
          inv <-NULL
          }
        get <-function () x
        setinv<-function () inv <<- solve (x)
        getinv <- function () inv
        list(set=set,get=get, setinv=setinv, getinv=getinv)
} 

# This first function, makeCacheMatrix(), creates a special "matrix" object as an output 
# which can cache a previously calculated inverse for a matrix supplied as an argument. 
# This function takes an argument x and calculates its inverse through the solve(x) function.
# The output is the list of functions which can be utilized as arguments when defining other
# functions, such as the case with the second function here (line 33).

cacheSolve<- function (mcm,...){
        inv<-mcm$getinv()
        if (!is.null(inv)){
          message ("getting cached data")
          return(inv)
        } else {
          data<-mcm$get()
          inv<-solve(data,...)
          mcm$setinv()
          inv
        }
}

# The second function, CacheSolve(mcm,...), takes argument "mcm". mcm is any name 
# assigned to the output from cakeCacheMatrixand(x). x is the matrix  we are 
# interested in obtaining the inverse of. CacheSolve() first verifies if the 
# iverse has been previuosly computed and cached. If so, the function return a message and 
# caches the inverse. Else, it retrives the matrix data from the makeCacheMatrix() function
# (using get()) computes the inverse and then stores it in the cache. 

##Example of how to use these functions together
A=matrix(1:4, 2,2)
mcm=makeCacheMatrix(A) # "mcm" is just the acronym for "makeCacheMatrix", it can be any name you prefer.
cacheSolve(mcm)
cacheSolve(mcm) # should be obtained from cached data 
