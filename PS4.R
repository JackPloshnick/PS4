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
