## The makeCacheMatrix function takes a matrix x as the input and:
## sets the value of the matrix 
## gets the value of the matrix
## sets the value of the inverse
## gets the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){  #this function is used if the value of the matrix is changed by calling $set 
                x <<- y      #it assigns the new value (y) to x
                i <<- NULL   #and resets i to NULL (clears the previously cached inverse)
        }
        get<-function() x  #this function just sets 'get' equal to the value of the matrix (x)
        setinverse <-function(solve) i<<-solve #this function calculates the inverse of the matrix (x) and sets it to i
        getinverse <- function() i     #this function just sets 'getinverse' equal to the value of the inverse (i)
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #this list is what is returned by the makeCacheMatrix function
}


## The cacheSolve function calculates the inverse of a matrix x, but first checks to see if the inverse has already been calculated.
## If not, it calculates the inverse and then saves it to the makeCacheMatrix function, so the next time the inverse of the same matrix

cacheSolve <- function(x, ...) {
        i<- x$getinverse() #Uses the makeCacheMatrix function to see if the inverse has already been calculated
        if(!is.null(i)){ #If there is a value for the inverse already, it returns the cached inverse
                message("getting cached data")
                return(i)
        }
        data<-x$get() #Otherwise, it sets data equal to the matrix
        i<-solve(data,...) #And then calculates the inverse
        x$setinverse(i)    #Then it caches the inverse in the makeCacheMatrix function
        i         #and returns the calculated inverse of the matrix.
}