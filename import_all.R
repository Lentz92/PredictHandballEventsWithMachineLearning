#This functions purpose was to import all subjects 100hz data from two different paths.
#IMU path was the 100hz data and label path was a csv file with labels for the subjects movements.

import_all <- function(IMUpath, labelPath){
  csvlist <- c("SubjectShirt02.csv", "SubjectShirt04.csv", 
               "SubjectShirt05.csv", "SubjectShirt06.csv",
               "SubjectShirt07.csv", "SubjectShirt08.csv", 
               "SubjectShirt09.csv", "SubjectShirt10.csv",
               "SubjectShirt11.csv", "SubjectShirt15.csv", 
               "SubjectShirt16.csv", "SubjectShirt17.csv")
  
  match1 <- c("SubjectShirt04.csv", "SubjectShirt07.csv", "SubjectShirt08.csv",
              "SubjectShirt09.csv", "SubjectShirt10.csv", "SubjectShirt11.csv")
  
  match2 <- c("SubjectShirt02.csv", "SubjectShirt05.csv", "SubjectShirt06.csv",
              "SubjectShirt15.csv", "SubjectShirt16.csv", "SubjectShirt17.csv")
  
  
  IMU = list()
  TAGS = list()
  j = 1
  for (i in csvlist){
    #Creating path where I am adding the "IMUpath" to the subject, 
    #example could path could be: ~/User/RelaventData/ which then would be added with i in csv list
    # ~/User/RelaventData/SubjectShrit02.csv
    IMUpaste <- paste(IMUpath, i, sep = "")
    labelsPaste <- paste(labelPath, i, sep = "")
    imudata <- data.table::fread(IMUpaste, skip = 6, header = TRUE) %>% 
      mutate(Timestamp = str_replace(Timestamp, '\\:(\\d+$)', '\\.\\1'),
             # use libridate::hms() to convert to hour, minute, second
             Timestamp = lubridate::hms(Timestamp),
             #Convert this to numeric count of seconds
             Timestamp = lubridate::period_to_seconds(Timestamp)) %>% 
      rename(forward = imuAcceleration.forward, up = imuAcceleration.up, 
             side = imuAcceleration.side, roll = Rotation.roll, 
             pitch = Rotation.pitch, yaw = Rotation.yaw,
             rawForward = Acceleration.forward,  rawSide = Acceleration.side,  
             rawUp = Acceleration.up, IMUforward = imuOrientation.forward, 
             IMUside = imuOrientation.side)
    
    #For correct synchronization an if statement was made as there were 2 different matches.
    if (i %in% match1){
      imudata <- imudata %>% 
        #Remove the first 28.25 sec to synchronize with the TAGS from video analysis
        filter(Timestamp > 28.25) %>% 
        #Synchronise the Timestamps after removing the first 28.25s for match 1
        mutate(Timestamp = Timestamp - 28.25,
               #This part is a very very special case as there was some issues with the labeling.
               Subject = str_sub(i, -6, -5))
    } else if (i %in% match2) {
      imudata <- imudata %>% 
        #Synchronise the Timestamps after removing the first 26s for match 2
        filter(Timestamp > 26) %>% 
        mutate(Timestamp = Timestamp - 26,
               Subject = str_sub(i, -6, -5))
    }
    
    labeldata <- data.table::fread(labelsPaste, header = TRUE) %>% 
      select(Tag, OffsetIn, OffsetOut) %>% 
      arrange(OffsetIn) %>% 
      #The lead function creates either a lead or a lagged version of the vector.
      #In this case it creates an offset of 1 relative to "offsetIn".
      mutate(OffsetOut = lead(OffsetIn, 1, default = max(OffsetIn)+1))
    
    labeldata$Tag[labeldata$Tag %in% c("Soft","Medium","Hard")] <- "Throw"
    
    labeldata <- labeldata %>% 
      group_by(Tag) %>% 
      mutate(Occurence = 1:n()) %>% 
      ungroup()
    
    IMU[[i]] <- imudata
    TAGS[[i]] <- labeldata
    j = j + 1
  }
  
  return(list(IMU = IMU, TAGS = TAGS))
}