
printHi <- function(x){
  paste0("Hi ",x)
}


### Functions to generate bread experiments

simTestParticipants <- function(n){

}


### Functions to generate mouse experiments

simTreatedMouseCage <- function(n, treatmenteffect=0, sdcage=1, sdmouse=1){
  cageeffect <- rnorm(1,10 + treatmenteffect, sdcage )
  rnorm(n,cageeffect, sdmouse )
}

simControlMouseCage <- function(n, treatmenteffect=0, sdcage=1,sdmouse=1){
  simTreatedMouseCage(n=n,treatmenteffect = treatmenteffect, sdcage=sdcage, sdmouse=sdmouse)
}

simControlMice <- function(ncages, micepercage){
    do.call(rbind,lapply(1:ncages,function(i) data.frame(treated="C",cage=paste0("C",i),outcome=simControlMouseCage(micepercage))))
}

simTreatedMice <- function(ncages, micepercage,treatmenteffect=0){
    do.call(rbind,lapply(1:ncages,function(i) data.frame(treated="T",cage=paste0("T",i),outcome=simTreatedMouseCage(micepercage,treatmenteffect = treatmenteffect))))
}

simMouseExperiment <- function(cagesPerTreatment, micePerCage, treatmenteffect=0){
  rbind(simControlMice(cagesPerTreatment, micePerCage),
        simTreatedMice(cagesPerTreatment, micePerCage,treatmenteffect))
}





