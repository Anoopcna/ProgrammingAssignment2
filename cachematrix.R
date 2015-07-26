## Here are two functions makeCacheMatrix which sets the initial values,
#assign the values of the vector,and outpting the list of functions used
#within the main function
#The later function cacheSolve uses the above function,to compute whether its 
#Inverse computation for the first time or is already cached ,accordingly,
#the 2nd function decides whether computation is required or not.


##########makeCacheMatrix################

#i stands for the value for inverse of the matrix.
#to set the value for the matrix and inverse.
#to get the value for the matrix.


makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

###############cacheSolve##################

## Return a matrix that is the inverse of 'x'
#if the value is not null and is present alredy hence dont compute;
#but printh result from the cache.
#to check whether the matix is singular,accordingly either display the message of the Inverse.

cacheSolve <- function(x, ...) {
        
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        
        
        if(det(data)==0)
        {
                message("Singular matrix hence,Inverse is not possible")
        }
                
        
        else if (det(data)!=0)
        {
        
        i <- solve(data, ...)
        x$setinv(i)
        i
}
}

        
        