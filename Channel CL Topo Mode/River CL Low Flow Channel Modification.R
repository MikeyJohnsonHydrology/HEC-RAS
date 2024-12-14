# River CL Low Flow Channel Modification

# Loading Libraries
library(readxl)
library(writexl)
library(dplyr)
library(zoo)
library(ggplot2)


### Set working directory ######################################################
sfl <- dirname(rstudioapi::getActiveDocumentContext()$path)  # this is the source file location
setwd(sfl)

### Loading CL Data ############################################################
River_CL_El <- read_excel("River CL Elevations from HEC-RAS.xlsx")

ggplot(River_CL_El,aes(Station_ft, Elevation_ft))+
  geom_point()

tmp <- River_CL_El

### Moving Window Min Elevation Calculator 
ceiling_height <- 110# Filter points above this elevation (feet)
window_size <- 60 # Moving Window Size (number of cells)
shrub_height <- 0.30 # Filtering Shrub Points (feet)

### Step 1: Filtering Duplicate Values #########################################

tmp1 <- tmp %>% distinct(Station_ft, .keep_all = TRUE)
print(paste("Number of duplicate points removed =",nrow(tmp)-nrow(tmp1))) #Number of Points Removed


### Step 2: filtering the high points ########################################## 
tmp2 <- tmp1

#Flagging the high points
tmp2$high_point_flag <- ifelse(tmp2$Elevation_ft>ceiling_height,1,0)

#Filtering the high points
tmp2 <- tmp2[which(tmp2$high_point_flag==0),]

# Plotting the New Data
ggplot(tmp2,aes(Station_ft,Elevation_ft))+geom_point()
print(paste("Number of high points removed =",nrow(tmp1)-nrow(tmp2)))


### Step 3: Filtering the Low Points ###########################################
tmp3 <- tmp2[,1:3]

# Moving Window Minimum Value 
tmp3$Rolling_Avg_Left <- rollapply(tmp3$Elevation_ft,
                                   width = window_size,
                                   FUN = min,
                                   fill = NA,
                                   align = "left")

tmp3$Rolling_Avg_Right <- rollapply(tmp3$Elevation_ft,
                                    width = window_size,
                                    FUN = min,
                                    fill = NA,
                                    align = "right")
  

# Minimum of the two window passes  
tmp3 <- tmp3 %>%  rowwise() %>%
  mutate(Min_Elevation = min(Rolling_Avg_Left, Rolling_Avg_Right, na.rm=T))

# Flagging the Low Points
tmp3$Low_Point_Flag <- ifelse(tmp3$Elevation_ft==tmp3$Min_Elevation,1,0)


# Filtering the Low Points
tmp3 <- tmp3[which(tmp3$Low_Point_Flag==1),]

# Plotting the New Data
ggplot(tmp3,aes(Station_ft,Elevation_ft))+geom_point()
print(paste("Number of low points identified =",nrow(tmp3)))


### Step 4: Filtering the vegetation points ####################################
tmp4 <- tmp3[,1:3]

# Loop to identify all the brush points
i<-1
while(i > 0){

  # Create lag and lead differences
  tmp4$El_diff <- tmp4$Elevation_ft-lag(tmp4$Elevation_ft)


  # flag data greater than the flag_val
  tmp4$brush_flag <- ifelse(tmp4$El_diff > shrub_height,1,0)
  tmp4$brush_flag[which(is.na(tmp4$brush_flag))] <- 0

  # Counting the number of flags
  i <- sum(tmp4$brush_flag)

  # Filtering the vegetation points
  tmp4 <- tmp4[which(tmp4$brush_flag==0),] 

  print(paste(i))
}
  
#Plotting the New Data
ggplot(tmp4,aes(Station_ft,Elevation_ft))+geom_point()
print(paste("Number of brush points removed =",nrow(tmp3)-nrow(tmp4)))

### Step 5: Finalizing the Data Set ############################################
tmp5 <- tmp4[,1:3]


# Adding the First and Last Data Points from the original data
# HEC-RAS need information for the full extent of the modification line
tmp_final <- rbind(tmp[1,],tmp5,tmp[nrow(tmp),])

#Plotting the final data
ggplot(tmp_final,aes(Station_ft,Elevation_ft))+geom_point()


#Saving the data to a new excel document
write_xlsx(tmp_final, "River CL Elevations for HEC-RAS.xlsx")

# Full Summary
print(paste("Number of duplicate points removed =",nrow(tmp)-nrow(tmp1))) #Number of Points Removed
print(paste("Number of high points removed =",nrow(tmp1)-nrow(tmp2)))
print(paste("Number of low points identified =",nrow(tmp3)))
print(paste("Number of brush points removed =",nrow(tmp3)-nrow(tmp4)))
print(paste("Number of endpoints added =",nrow(tmp_final)-nrow(tmp4)))

