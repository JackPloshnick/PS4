######## Getting Started 

myFunction<-function(doorthing, doorthing2, x){#used traceback() to see that this code is very broken
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

setClass(Class="door",  #Sets S4 class of door
         representation = representation(
           chosenDoor = "numeric", # three slots as specified in problem set
           carDoor = "numeric", #Numeric, Numeric, Logical
           switch = "logical"
         ),
         prototype = prototype(
           chosenDoor = c(), #default values are empty 
           carDoor = c(),
           switch = c()
         )
)



setValidity("door", function(object){ 
  ChosenTest = (object@chosenDoor ==1 | object@chosenDoor ==2 | object@chosenDoor == 3) #ensures chosendoor is between 1-3
  ChosenLength = (length(object@chosenDoor) == 1) #ensures chosendoor is length of 1
  
  CarTest = (object@carDoor ==1 | object@carDoor ==2 | object@carDoor == 3) #ensures cardoor is between 1-3
  CarLength = (length(object@carDoor) == 1)# enusres cardoor is length of 1
  
  switchTest = (is.logical(object@switch == TRUE)) #ensures switch is a logical
  switchLength = (length(object@switch == 1))#ensures switch is of length 1
  
  
  if(!ChosenTest | !ChosenLength){ #returns error if chosen door is broken        
    return("@chosenDoor not valid")
  }
  if(!CarTest | !CarLength){ #returns error if cardoor is not valid
    return("@carDoor not valid")
  }
  if(!switchTest | !switchLength){ #returns error if switch is not valid
    return("@switch not valid")
  }
  
}
)


setMethod("initialize", "door", function(.Object, ...) { #initilize method 
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
            RandomCarDoor = as.numeric(sample(1:3,1)) #creates random location for cardoor
            DoorChosenFirst = as.numeric(sample(1:3,1))#creates random location for the door initially chosen
      
            #the following code decides which door is opened by the gameshow host, after inital door is chosen by contestent
            
            newDoor = c(1,2,3)
            notCar = newDoor[!newDoor %in% RandomCarDoor] #Gets rid of car door
            neither = notCar[!notCar %in% DoorChosenFirst] #Gets rid of chosen door
           
            if(length(neither)== 2){ #if cardoor and chosen door are the same, randomly choose between the two remaining doors 
              openedDoor = as.numeric(sample(c(neither),1))
            }
             
            if(length(neither)==1){#if not the same, open remaining door
              openedDoor = as.numeric(neither)#sample does not work if there is only one remaining numeric,
            #I used debug to determine this, and write this if, if code. 
            }
            
          pickedDoor= as.numeric(newDoor[-c(openedDoor, DoorChosenFirst)]) #the picked door, final door at end of the show,
          #is the door that was neither picked first, nor opened by the host used browser() to ensure this worked properly, which it does 
       
            
          
            if(object@switch == FALSE){ #if switch in the input door is FALSE, don't switch
            object@carDoor <- RandomCarDoor
            object@chosenDoor <- DoorChosenFirst
            
          }
          if(object@switch == TRUE){#if switch in the input door is TRUE, switch 
            object@carDoor <- RandomCarDoor
            object@chosenDoor <- pickedDoor
            
          }  
       winner = (object@carDoor == object@chosenDoor)
       return(winner)
} )

blankDoor <- new("door", chosenDoor = 1, carDoor = 2, switch = TRUE)




PlayGame(blankDoor)

#####Simulation 

Switch <- new("door", chosenDoor = 1, carDoor = 1, switch = TRUE)# input door has value of switch=TRUE. values of chosendoor and cardoor are overwritten by function 

ListofSwitch <- rep( c(Switch, Switch), times = 500)

SwitchApplied<- sapply(ListofSwitch, PlayGame)

table(SwitchApplied)

###

NoSwitch <- new("door", chosenDoor = 1, carDoor = 1, switch = FALSE)# input door has value of switch=FALSE. values of chosendoor and cardoor are overwritten by function 

ListofNoSwitch <- rep( c(NoSwitch, NoSwitch), times = 500)

NoSwitchApplied <- sapply(ListofNoSwitch, PlayGame)

table(NoSwitchApplied)




