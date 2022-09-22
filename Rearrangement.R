# Reinitialize the session
rm(list=ls(all=TRUE))

library(xlsx)
# Go to the working directory
setwd("C:/Users/mdinclaux/Documents/Script/TWB_Robot/Rearrangement/")

data <- read.csv("data.csv", header = FALSE, sep = ";", dec = ".",stringsAsFactors = FALSE)
colnames(data) <- c("a","b",seq(1:12))
nb_plate = 5
nb_destin = 3
vol = 20

myfunc <- function(v1) {
  deparse(substitute(v1))
}

plate_template = c()

for (i in LETTERS[1:8]) {
 temp <- paste(i, 1:12, sep = "")
 plate_template <- rbind(plate_template, temp)
}
plate_template <- as.data.frame(plate_template)
colnames(plate_template) <- seq(1:12)
rownames(plate_template) <- LETTERS[1:8]

S1 <- data[2:9,3:ncol(data)]
S2 <- data[12:19,3:ncol(data)]
S3 <- data[22:29,3:ncol(data)]
S4 <- data[32:39,3:ncol(data)]
S5 <- data[42:49,3:ncol(data)]

D1 <- data[52:59,3:ncol(data)]
D2 <- data[62:69,3:ncol(data)]
D3 <- data[72:79,3:ncol(data)]
D4 <- data[82:89,3:ncol(data)]
D5 <- data[92:99,3:ncol(data)]


output <- matrix("Z",ncol = 5)
for (d in  seq(1:nb_destin)) {
  for (row in  seq(1:8) ) {
    a = (eval(parse(text = paste("D",d,sep = ""))))[row,]
    for (col in seq(1:12)) {
      fact = as.character(a[col])
      coo_dest<-which( (eval(parse(text = paste("D",d,sep = ""))))== fact,arr.ind=TRUE)
      l_dest <- nrow(coo_dest)
      for (c in seq(1:nb_plate)) {
        if (is.integer(which( (eval(parse(text = paste("S",c,sep = ""))))== fact)) && length(
          which((eval(parse(text = paste("S",c,sep = ""))))== fact)) != 0) {
          coo_ini<-which( (eval(parse(text = paste("S",c,sep = ""))))== fact,arr.ind=TRUE)
          Source_plate = paste("S",c,sep = "")
          Source_well = as.character(plate_template[coo_ini[1,1],coo_ini[1,2]])
          Destination_plate = paste("D",d,sep = "")
          Destination_well = as.character(plate_template[row,col])
         
          Volume_source = vol
          
          output <-  rbind(output,cbind(Source_plate,Source_well,Destination_plate,
                                        Destination_well,Volume_source))
        }
       
      }
    }
  }
}
output <- rbind(c("Source plate","Source well","Destination plate",
                  "Destination well","Volume source"),
                output[-1,])

write.xlsx(output, file = "output.xlsx",sheetName="combinations", 
           col.names=FALSE, row.names=FALSE, append=FALSE)
browseURL("output.xlsx")

