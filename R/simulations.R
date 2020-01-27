

### Functions to generate bread experiments

simTestParticipants <- function(n, treatmenteffect=50, withinpatientsd=40,betweenpatientsd=40){
    rnorm(n, treatmenteffect + 500 , sqrt(betweenpatientsd^2 + withinpatientsd^2) )
  }

simControlParticipants <- function(n, treatmenteffect=0, withinpatientsd=40,betweenpatientsd=40){
    simTestParticipants(n, treatmenteffect=0, withinpatientsd=withinpatientsd, betweenpatientsd=betweenpatientsd)
  }

### Generate paired bread experiment

simPairedBreadExperiment <- function(npairs, treatmenteffect=50, withinpatientsd=40,betweenpatientsd=40){
  patientmeans <- rnorm(npairs, mean=500, betweenpatientsd)
  data.frame(patid = 1:npairs,test=rnorm(npairs,patientmeans+treatmenteffect,withinpatientsd),
             control=rnorm(npairs,patientmeans,withinpatientsd))
}

### Generate unpaired bread experiment

simUnpairedBreadExperiment <- function(npairs, treatmenteffect=50, withinpatientsd=40,betweenpatientsd=40){
  data.frame(treatment=rep(c("C","T"),each=npairs),iAUC=c(simTestParticipants(npairs,treatmenteffect),
             simControlParticipants(npairs)))

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





