## Luke Ozsanlav-Harris
## Created: 20/06/2022

## These functions are required by the script called "App- visualize GPS data"
## They serve to run a HMM on tracking data tat is read in by the shiny app
## Starting parameters and the number of hidden states are set in the app user interface


# Code to read in data and test functions
# setwd("C:/Users/lo288/OneDrive - University of Exeter/Documents/PhD Documents/Courses/R shiny course/Tacking data app")
# data = read.csv("Two_elephants.csv")
# data = dplyr::filter(data, individual.local.identifier == "AM239")



## Two function in this script
## 1. split_at_gap: split GPS data into separate tracks if there are gaps over a certain threshold
## 2. HMMforShiny: Runs a hidden markov model on the imputed GPS data and outputs the model object and the tracking data with decoded states

split_at_gap <- function(data, max_gap = max_gap, shortest_track = shortest_track) {
  # Number of tracks
  n_tracks <- length(unique(data$ID))
  
  # Save old ID and reinitialise ID column
  data$ID_old <- data$ID
  data$ID <- character(nrow(data))
  
  # Loop over tracks (i.e., over IDs)
  for(i_track in 1:n_tracks) {
    # Indices for this track
    ind_this_track <- which(data$ID_old == unique(data$ID_old)[i_track])
    track_length <- length(ind_this_track)
    
    # Time intervals in min
    dtimes <- difftime(data$time[ind_this_track[-1]], 
                       data$time[ind_this_track[-track_length]],
                       units = "mins")
    
    # Indices of gaps longer than max_gap
    ind_gap <- c(0, which(dtimes > max_gap), track_length)
    
    # Create new ID based on split track
    subtrack_ID <- rep(1:(length(ind_gap) - 1), diff(ind_gap))
    data$ID[ind_this_track] <- paste0(data$ID_old[ind_this_track], "-", subtrack_ID)
  }
  
  # Only keep sub-tracks longer than some duration
  track_lengths <- sapply(unique(data$ID), function(id) {
    ind <- which(data$ID == id)
    difftime(data$time[ind[length(ind)]], data$time[ind[1]], units = "min")
  })
  ID_keep <- names(track_lengths)[which(track_lengths >= shortest_track)]
  data <- subset(data, ID %in% ID_keep)
  
  return(data)
}


## This function takes in tracking data, breaks it into segments with similar sampling intervals and then runs a 2/3 state HMM
## Finally it decodes the states and adds then back to the tracking data 
HMMforShiny <- function(data=data, No_States=No_States, SL_mean1=SL_mean1, SL_sd1=SL_sd1, SL_mean2=SL_mean2, SL_sd2=SL_sd2,
                        SL_mean3=SL_mean3, SL_sd3=SL_sd3, Angle_mean1=Angle_mean1, Angle_mean2=Angle_mean2,
                        Angle_mean3=Angle_mean3){

  library(ggplot2)
  library(momentuHMM)
  library(adehabitatLT)
  library(sf)
  library(sp)
  
  
  # Keep relevant columns: ID, time, longitude, latitude, temperature
  data_short <- data[, c("individual.local.identifier", "timestamp", "location.long", "location.lat")]
  colnames(data_short) <- c("ID", "time", "lon", "lat")
  data_short$time <- as.POSIXct(data_short$time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  
  
  # Project to UTM
  llcoord <- st_as_sf(data_short[, c("lon", "lat")], coords = c("lon", "lat"), 
                      crs = CRS("+proj=longlat +datum=WGS84"))
  utmcoord <- st_transform(llcoord, crs = CRS("+proj=utm +zone=35 +datum=WGS84"))
  
  
  # Add Easting-Northing to data (in km)
  data_short[, c("x", "y")] <- st_coordinates(utmcoord)/1000
  
  
  # Use function from utility_function.R to split data_short at gaps > 2 hours
  data_split <- split_at_gap(data = data_short, max_gap = 2*60, shortest_track = 24*60)


  # Create adehabitat trajectory padded with NAs
  data_ade <- setNA(ltraj = as.ltraj(xy = data_split[, c("x", "y")], 
                                     date = data_split$time, 
                                     id = data_split$ID), 
                                     date.ref = data_split$time[1], 
                                     dt = 30, tol = 5, units = "min")
  
  
  # Transform back to dataframe
  data_na <- ld(data_ade)[, c("id", "x", "y", "date")]
  colnames(data_na) <- c("ID", "x", "y", "time")
  
  
  # Prepare data for HMM (compute step lengths and turning angles)
  data_hmm1 <-prepData(data_na, type = "UTM")
  
  
  # Observation distributions (step lengths and turning angles)
  dist <- list(step = "gamma", angle = "vm")
  
  
  # Initial parameters for 3-state model
  # for step length we have mean1, mean2, mean3, sd1, sd2, sd3 / for angle we have mean1, mean2, mean2
  
  
  # Run the model with either 2 or 3 hidden states below
  if(No_States==3){
    
    # create list of starting parameters
    # Par0_3s <- list(step = c(0.02, 0.1, 0.3, 0.02, 0.1, 0.3), 
    #                  angle = c(0.01, 0.1, 3))
    Par0_3s <- list(step = c(SL_mean1, SL_mean2, SL_mean3, SL_sd1, SL_sd2, SL_sd3), 
                   angle = c(Angle_mean1, Angle_mean2, Angle_mean3))
    
    # Fit 3-state HMM
    hmm2 <- fitHMM(data_hmm1, nbStates = 3, dist = dist, Par0 = Par0_3s)
    
    # Save most likely state sequences from 2-state and 3-state models
    data_hmm1$state_3st <- factor(viterbi(hmm2))
    
    # Add Easting-Northing to data (in km)
    coords <- data_hmm1[, c("x", "y")]*1000
    coords2 <- st_as_sf(coords[is.na(coords$x)==F,], coords = c("x", "y"),
                        crs = CRS("+proj=utm +zone=35 +datum=WGS84"))
    llcoord <- st_transform(coords2, crs = CRS("+proj=longlat +datum=WGS84"))
    data_hmm1$long <- NA
    data_hmm1$lat <- NA
    data_hmm1[is.na(data_hmm1$x)==F, c("long", "lat")] <- st_coordinates(llcoord)
    
    # re assign the decoded states
    HMMOutput <- data_hmm1
    
    
  }
  
  
  if(No_States==2){
    
    # create list of starting parameters
    Par0_3s <- list(step = c(SL_mean1, SL_mean2,SL_sd1, SL_sd2), 
                    angle = c(Angle_mean1, Angle_mean2))
    
    # Fit 3-state HMM
    hmm2 <- fitHMM(data_hmm1, nbStates = 2, dist = dist, Par0 = Par0_3s)
    
    # Save most likely state sequences from 2-state and 3-state models
    data_hmm1$state_3st <- factor(viterbi(hmm2))
    
    # Add Easting-Northing to data (in km)
    coords <- data_hmm1[, c("x", "y")]*1000
    coords2 <- st_as_sf(coords[is.na(coords$x)==F,], coords = c("x", "y"),
                        crs = CRS("+proj=utm +zone=35 +datum=WGS84"))
    llcoord <- st_transform(coords2, crs = CRS("+proj=longlat +datum=WGS84"))
    data_hmm1$long <- NA
    data_hmm1$lat <- NA
    data_hmm1[is.na(data_hmm1$x)==F, c("long", "lat")] <- st_coordinates(llcoord)
    
    # re assign the decoded states
    HMMOutput <- data_hmm1
    
  }
  
  
  
  # return the fitted markov model and the decoded states in a list
  AllOutput <- list(Decoded = HMMOutput, model =hmm2)
  return(AllOutput)
  
  
}





