######## Getting Started 

myFunction<-function(doorthing, doorthing2, x){
  doorthing1<-doorthing2<-sample(1:3, 1)
  if (doorthing1==doorthing2){ x<-TRUE } else { x==FALSE }
  x
}
myFunction(sample(1:3, 1), sample(1:3, 1))
# Should return a TRUE if these samples are equal and
# a false if they are not

debug(myFunction)

#fixed function 

montyHall <- function(ChoiceOfDoor, LocationOfCar){  #input which door you choose, and where the car is.
  
  if (ChoiceOfDoor == LocationOfCar){ #if door choice = car location, return TRUE
    return(TRUE)
  }
  else{ #if door choice does not equal car location, return FALSE
    return(FALSE)
  }
}

montyHall(3,3)



##### Question 1

setClass(Class="door",
         representation = representation(
           chosenDoor = "numeric",
           carDoor = "numeric",
           switch = "logical"
         ),
         prototype = prototype(
           chosenDoor = c(),
           carDoor = c(),
           switch = c()
         )
)



setValidity("door", function(object){ #ensures door is one number between 1-3
  ChosenTest = (object@chosenDoor ==1 | object@chosenDoor ==2 | object@chosenDoor == 3)
  ChosenLength = (length(object@chosenDoor) == 1)
  
  CarTest = (object@carDoor ==1 | object@carDoor ==2 | object@carDoor == 3)
  CarLength = (length(object@carDoor) == 1)
  
  switchTest = (is.logical(object@switch == TRUE))
  switchLength = (length(object@switch == 1))
  
  winnerTest = (object@chosenDoor == object@carDoor)
  
  if(!ChosenTest | !ChosenLength){
    return("@chosenDoor not valid")
  }
  if(!CarTest | !CarLength){
    return("@carDoor not valid")
  }
  if(!switchTest | !switchLength){
    return("@switch not valid")
  }
  if( winnerTest){
    return(winner = TRUE)
  }
}
)


setMethod("initialize", "door", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})



test <- new("door", chosenDoor = 2, carDoor = 2, switch = FALSE)

##What to do about winner??

###### Question 2


setGeneric("PlayGame", #sets generic function in S4
           function(object="door") {
             standardGeneric("PlayGame")
           } )

setMethod("PlayGame", "door", 
          function(object){
            RandomCarDoor = as.numeric(sample(1:3,1))
            DoorChosenFirst = as.numeric(sample(1:3,1))
            x = new("door", chosenDoor = DoorChosenFirst, carDoor = RandomCarDoor, switch = FALSE)
            
          return(x)
            
          } )

blankDoor <- new("door", chosenDoor = 3, carDoor = 2, switch = FALSE)


PlayGame(blankDoor)
