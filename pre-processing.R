###split VI time series .csv files by species and site 

rm(list = ls())

library(tidyverse)
library(readxl)
library(here) 
library(kableExtra)
library(dplyr)

####### split whole VI composite csv by each species (It take around 3min)################################################################################################################
# Set the output directory(change it according to your local machine setting)
in_dir <- "C:/Users/xg1m22/OneDrive - University of Southampton/Desktop/Australia_classification"
pattern_regex <- paste0('.csv')
excel_files <- list.files(path=in_dir,pattern=pattern_regex,full.names=T)

for(i in 1:length(excel_files) ) {
  # i=3
  index_1<-read.csv(excel_files[i])
  index<- index_1%>% filter(RELIABILIT == 1|RELIABILIT == 2|RELIABILIT == 3)
  index_name<-substr(excel_files[i], 94,97)
  dir.create(file.path(in_dir,index_name))
  for (j in 1:length(unique(index$ACTIVITY_N))){
    # j=5
    name=unique(index$ACTIVITY_N)[j]
    #Subset the data by species
    tmp=subset(index,ACTIVITY_N==name)
    #Create a new filename for each species - the folder 'index' should already exist
    fn=paste(in_dir,'\\',index_name,'\\',gsub(' ','',name),'.csv',sep='')
    #Save the CSV file containing separate VI data for each species
    
    write.csv(tmp,fn,row.names=FALSE)
  }
  }


###################### split each species by ID and year (It takes around 1.5h)#########################################################################################################
in_dir <- "C:/Users/xg1m22/OneDrive - University of Southampton/Desktop/Australia_classification"
# Set the output directory
pattern_regex <- paste0('.csv')
excel_files <- list.files(path=in_dir,pattern=pattern_regex,full.names=T)
#each polygon each year each index
index<- c('CIre','ndvi','mtc2','mtci','EVI2','evi_','lci_')
Specisename<-c("Apples","Avocado","Bananas","CitrusFruit","Mangoes","Oranges","Winegrapes" )
for(i in 1:length(index) ) {
  # i=2
  index_name<-index[i]
  in_dir_index<-paste("C:/Users/xg1m22/OneDrive - University of Southampton/Desktop/Australia_classification/new_index_new/",gsub(' ','', index_name),sep='')
  Index_files <- list.files(path=in_dir_index,pattern=pattern_regex,full.names=T)
  for (z in 1:length(Index_files)){
    # z=7
  name= Specisename[z]
  dir.create(file.path(in_dir_index, name))
  indexfiles <- read.csv(Index_files[z], na.strings = c("","NA"))
    for(k in 1: length(unique(indexfiles$ID))){
      # k=3780
    id <- unique(indexfiles$ID)[k]
    index_unique <-  indexfiles %>% filter(ID == id)
    output_df <- data.frame(index_unique$Date,index_unique[16],index_unique[20],index_unique[21],index_unique$unique_id,index_unique$ID,index_unique$ACTIVITY_N
)
    column_names <- c('Date',  index_name, 'coords_X','coords_Y','unique_id','ID','Species')
    colnames(output_df) <- column_names
    output_string<-c(paste0(in_dir_index,'/', name,'/',id,'.csv'))
    write.csv(output_df, output_string)
    }}}
    
    
    


  
  