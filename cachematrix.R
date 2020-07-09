#Here we create an example 2x2 matrix
ex <- matrix(c(1,2,3,4), 2, 2)
ex
solve(ex)
?solve

#This function will save in cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){ #Here I stablish getters and setters
                x <<- matrix(y) #We are telling the values in the parent enviromment
                i <<- NULL      #Same here
        }
        get <- function() x #Use the values of X from the parent environment
        setinverse <- function() i <<- solve(x) #Here is where I tell R to calculate de inverse of a matrix
        getinverse <- function() i #We actually give the value of the operation to getinverse
        list(set = set,
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
                     
}
#This function checks if the matrix makeCacheMatrix have stored the inverse of the matrix
#If it doesn't the functions perform that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()#Calls for the value calculated in getinverse
        if(!is.null(i)){#If it existes....
                message("getting cache data") #You get the message
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse()
        i
}
ex1 <- makeCacheMatrix(ex)
#Here it the result wasn't in cache.
cacheSolve(ex1)
#Here the message pop ups because it is already cached.
cacheSolve(ex1)