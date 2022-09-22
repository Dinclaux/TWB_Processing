#2022-07-27 mickael.dinclaux@inrae.fr

#v1.0 intial script 28/07/2022

#script for position reassignment + generation of a cherry picking template

# Reinitialize the session
rm(list=ls(all=TRUE))


#########################################################
###       Installing and loading required packages    ###
#########################################################


if (!require("xlsx")){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}
if (!require("dplyr")){
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}
if (!require("stringr")){
  install.packages("stringr", dependencies = TRUE)
  library(stringr)
}

#########################################################
###              working directory                    ###
#########################################################


setwd("C:/Users/mdinclaux/Documents/Script/TWB_Robot/Rearrangement/")


data <- read.csv("2022_07_22 picking plaque 2.csv", header = TRUE, sep = ",",
                 dec = ".", stringsAsFactors = FALSE, skip = 9)

rearrangement = TRUE #rearrangement for cherry picking (1 plate / 1 replica)

volume = 50 #volume for cherry picking

count_replicate = TRUE #Counting replicas ?


#########################################################
###              Do not modify                        ###
#########################################################
#########################################################
###       Source file processing                      ###
#########################################################

data <-data[,-3]
data <-data[,-3]
data$Source.Barcode <-as.factor(data$Source.Barcode)
data$Destination.Barcode <-as.factor(data$Destination.Barcode)

qtray <- levels(data$Source.Barcode)
microplate <- levels(data$Destination.Barcode)

Qtray_alphanum <- t(data.frame("A1","A2","A3","A4","B1","B2","B3","B4","C1",
                               "C2","C3","C4","D1","D2","D3","D4","E1","E2",
                               "E3","E4","F1","F2","F3","F4","A5","A6","A7",
                               "A8","B5","B6","B7","B8","C5","C6","C7","C8",
                               "D5","D6","D7","D8","E5","E6","E7","E8","F5",
                               "F6","F7","F8"))
MP1_alphanum <- t(data.frame("A1","B1","C1","D1","E1","F1","G1","H1","A2",
                             "B2","C2","D2","E2","F2","G2","H2","A3","B3",
                             "C3","D3","E3","F3","G3","H3","A4","B4","C4",
                             "D4","E4","F4","G4","H4","A5","B5","C5","D5",
                             "E5","F5","G5","H5","A6","B6","C6","D6","E6",
                             "F6","G6","H6"))
MP2_alphanum <- t(data.frame("A7","B7","C7","D7","E7","F7","G7","H7","A8",
                             "B8","C8","D8","E8","F8","G8","H8","A9","B9",
                             "C9","D9","E9","F9","G9","H9","A10","B10","C10",
                             "D10","E10","F10","G10","H10","A11","B11","C11",
                             "D11","E11","F11","G11","H11","A12","B12","C12",
                             "D12","E12","F12","G12","H12"))
ref_alpahnum= cbind(Qtray_alphanum,MP1_alphanum,MP2_alphanum)
colnames(ref_alpahnum)<- c("Qtray","MP1","MP2")

#########################################################
###             Checking the number of qtray          ###
#########################################################

if(length(qtray)>2){
  qtray_pair <- qtray[2]
  qtray_impair <- qtray[1]
  for(i in 1:length(qtray)){
    num = as.integer(i)
    if((num %% 2) == 0) {
      data[data == qtray[i]] <- qtray_pair
      print(paste(num,"is Even"))
    } else {
      data[data == qtray[i]] <- qtray_impair
      print(paste(num,"is Odd"))
    }
  }
    
}


#########################################################
###             Reallocation                          ###
#########################################################


for ( i in 1:nrow(data)) {
  if (data[i,1] == qtray[1]){
    for (j in 1:nrow(ref_alpahnum)) {
      if (data[i,2] == ref_alpahnum[j,1]){
        data[i,2] <- ref_alpahnum[j,2]
        break
      }
    }
  }
  else 
    for (j in 1:nrow(ref_alpahnum)) {
      if (data[i,2] == ref_alpahnum[j,1]){
        data[i,2] <- ref_alpahnum[j,3]
        break
      
    }
  }
}


for (m in 1:length(qtray)) {
  data$Source.Barcode <-sub(qtray[m],paste("Qtray", m, sep = ""),
                            data$Source.Barcode)
}

for (n in 1:length(microplate)) {
  data$Destination.Barcode <-sub(microplate[n],paste("MP", n, sep = ""),
                                 data$Destination.Barcode)
}


#########################################################
###                  Save                             ###
#########################################################

filename_save<- paste(Sys.Date(),"_output.xlsx", sep = "")
write.xlsx(data, file = filename_save ,sheetName="Reattribution", 
           col.names=TRUE, row.names=FALSE, append=FALSE)
browseURL(filename_save)

#########################################################
###         Reallocation for cherry picking          ###
#########################################################

if(rearrangement == TRUE){
  nbplate <-  length(unique(data$Destination.Barcode))
  
  for (i in 1:nrow(data)) {
  data[i,2] <- paste0(data[i,2],"alpha")
}


data$Source.Region <-as.factor(data$Source.Region)
cheery <- levels(data$Source.Region)
replicate <- 0
data$Volume_source_well <- volume


for (x in cheery) {
  verif <- count(data %>% filter(grepl(x, Source.Region,fixed = F,
                                       perl = F)),"Source.Barcode")
  if(verif[2] > replicate){
    replicate <- verif[1,2]
  }
}

if (count_replicate == TRUE){
data$replica <- NA
for (x in cheery) {
test <-  as.character(count(data[data$Source.Region == x,]))
 for(z in 1:nrow(data)){
   if (data[z,2] == x) {
     data[z,6] <- test
     
     
   }
 }
  
}
}

for(b in 1:replicate){
  filename <- paste0("file_",b)
  columns <- paste0("colnames(file_",b,")")
  assign(filename,tibble((matrix(nrow = 0, ncol = length(colnames(data))))))
}


for (x in cheery) {

  datfilter <- data %>% filter(grepl(x, Source.Region,fixed = F, perl = F))
  for(z in 1:nrow(datfilter)){
    test <- paste0("file_",z)
    assign(test,rbind(get(test),datfilter[z,]))
    
  }
}

for(b in 1:replicate){
  filename <- paste0("file_",b)
  destination <- paste0("D",b)
  assign(filename,cbind(tibble((matrix(data = destination ,
                                           nrow = nrow(get(filename)), 
                                           ncol = 1))),get(filename)))
  assign(filename,select(get(filename),-Source.Barcode))
  assign(filename, get(filename) %>% relocate(Destination.Barcode,
                                              Destination.Well))
  assign(filename, lapply(get(filename), gsub, pattern = "alpha",
                          replacement = "", fixed = TRUE))
  assign(filename,as.data.frame(get(filename) ))

  for(d in 1:nbplate){
  assign(filename, lapply(get(filename), gsub, pattern = paste0("MP",d),
                          replacement = paste0("S",d), fixed = TRUE))
  assign(filename,as.data.frame(get(filename) ))
  }
  if (count_replicate == TRUE){
    assign(filename, setNames(get(filename),c("Source plate ","Source well ",
                                              "Destination plate ID",
                                              "Destination well",
                                              "Volume source well",
                                              "replica")))  
  }
  if (count_replicate == FALSE){
   assign(filename, setNames(get(filename),c("Source plate ","Source well ",
                                            "Destination plate ID",
                                            "Destination well",
                                            "Volume source well"))) 
  }
  
  
#########################################################
###                      Save                         ###
#########################################################

filename_save<- paste(Sys.Date(),"MP",b,"_output_rearengement.xls", sep = "")
write.xlsx(get(filename), file = filename_save ,sheetName="combinations", 
           col.names=TRUE, row.names=FALSE, append=FALSE)
browseURL(filename_save)
}
}
