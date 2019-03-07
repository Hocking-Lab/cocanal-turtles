
#### Wolverine Example in "Spatial Capture-Recapture" Book ####

load("~/Desktop/Thesis/cocanal-turtles/Exampledata/scrbook/data/wolverine.rda") #contains a list of EDF and TDF

##EDF
wolverine$wcaps[1:5,]
# session will respresent site in turtle script

## Convert to 3-d array of encounter frequency matrix, then sum trap totals
#y3d <- SCR23array(wolverine$wcaps, wolverine$wtraps)
#y <- apply(y3d, c(1,2), sum)
