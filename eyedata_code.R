library(gazepath)
library(dplyr)

#Need to have this in environment before running algorithm
names = c("height", "width", "resx", "resy")
specs = c(294.46,	527.04,	1920,	1080)
screen = data.frame(names, specs)

#Set directory to folder where all the files are 
ground_truth <- data.frame()
SD_event_data = data.frame()
RMSE_values = c()
RMSE_data = data.frame()
count_files = 0 

#Creating the RMSE function
RMSE = function(m,o){
  col = m %>% pull(o)
  avg = mean(col, na.rm = T)
  sqrt(avg)
}

event_count = 0
csv_count = 0

main_path = "~/Desktop/Data/RAW_DATA/RAW_DATA_OrigRes_DomEye"
setwd(main_path)
event_folders = list.dirs('.', recursive = F) #event (43) folders

for (i in 8) {
  csv_count = 0
  event_count = event_count + 1
  ext1 = event_folders[i]
  sub_ext1 = substr(ext1, 2, nchar(ext1))
  event_path = paste(getwd(), sub_ext1, sep = "")
  event_path = paste(event_path, paste(sub_ext1,"_FV", sep = ""), sep = "") # uncomment for dominant data
  setwd(event_path)
  
  file_list = list.files(getwd(), pattern="*.csv")
  
  for (l in 1:length(file_list)){ #iterate through every file for dominant data
    alg_file = file_list[l]
    eye_data = read.csv(alg_file, header = FALSE)
    
    eye_data = eye_data%>%
      mutate(trial = 1)%>%
      mutate(distance = 1000)
    
    #GAZEPATH
    alg_results_gaze = gazepath(eye_data, x1 = 8, y1 = 9, d1 = 12, trial = 11, height_mm = screen[1,2],
                                width_mm = screen[2,2], width_px = screen[3,2], height_px = screen[4,2], method = "gazepath",
                                samplerate = 1000, thres_dur = 80)
    s_gaze = summary(alg_results_gaze)
    
    if (s_gaze == "There were no fixations or saccades classified, probably data quality of this particpant is very low") { #exclude poor quality data
      csv_count = csv_count + 1
      next
    }
    
    #Getting fixation metrics:
    fix_filtered_gaze = s_gaze%>%
      filter(Value == "f")
    
    fix_count_set_gaze = fix_filtered_gaze[1]
    alg_fix_count_gaze = length(fix_count_set_gaze[,1])
    alg_fix_dur_sum_gaze = sum(fix_filtered_gaze$Duration)
    alg_fix_dur_avg_gaze = mean(fix_filtered_gaze$Duration, na.rm = T)
    
    # #Getting saccade metrics:
    sac_filtered_gaze = s_gaze%>%
      filter(Value == "s")
    
    sac_count_set_gaze = sac_filtered_gaze[1]
    alg_sac_count_gaze = length(sac_count_set_gaze[,1])
    alg_sac_dur_sum_gaze = sum(sac_filtered_gaze$Duration)
    alg_sac_dur_avg_gaze = mean(sac_filtered_gaze$Duration, na.rm = T)
    alg_sac_dur_adj_avg_gaze = alg_sac_dur_avg_gaze / 10
    
    
    #DISPERSION
    alg_results_dis = gazepath(eye_data, x1 = 8, y1 = 9, d1 = 12, trial = 11, height_mm = screen[1,2],
                                width_mm = screen[2,2], width_px = screen[3,2], height_px = screen[4,2], method = "dispersion",
                                samplerate = 1000, thres_dur = 80)
    s_dis = summary(alg_results_dis)
    
    #Getting fixation metrics:
    fix_filtered_dis = s_dis%>%
      filter(Value == "f")
    
    fix_count_set_dis = fix_filtered_dis[1]
    alg_fix_count_dis = length(fix_count_set_dis[,1])
    alg_fix_dur_sum_dis = sum(fix_filtered_dis$Duration)
    alg_fix_dur_avg_dis = mean(fix_filtered_dis$Duration, na.rm = T)
    
    # #Getting saccade metrics:
    sac_filtered_dis = s_dis%>%
      filter(Value == "s")
    
    sac_count_set_dis = sac_filtered_dis[1]
    alg_sac_count_dis = length(sac_count_set_dis[,1])
    alg_sac_dur_sum_dis = sum(sac_filtered_dis$Duration)
    alg_sac_dur_avg_dis = mean(sac_filtered_dis$Duration, na.rm = T)
    alg_sac_dur_adj_avg_dis = alg_sac_dur_avg_dis / 10
    
    #VELOCITY
    
    alg_results_vel = gazepath(eye_data, x1 = 8, y1 = 9, d1 = 12, trial = 11, height_mm = screen[1,2],
                                width_mm = screen[2,2], width_px = screen[3,2], height_px = screen[4,2], method = "velocity",
                                samplerate = 1000, thres_dur = 80)
    s_vel = summary(alg_results_vel)
    
    #Getting fixation metrics:
    fix_filtered_vel = s_vel%>%
      filter(Value == "f")
    
    fix_count_set_vel = fix_filtered_vel[1]
    alg_fix_count_vel = length(fix_count_set_vel[,1])
    alg_fix_dur_sum_vel = sum(fix_filtered_vel$Duration)
    alg_fix_dur_avg_vel = mean(fix_filtered_vel$Duration, na.rm = T)
    
    # #Getting saccade metrics:
    sac_filtered_vel = s_vel%>%
      filter(Value == "s")
    
    sac_count_set_vel = sac_filtered_vel[1]
    alg_sac_count_vel = length(sac_count_set_vel[,1])
    alg_sac_dur_sum_vel = sum(sac_filtered_vel$Duration)
    alg_sac_dur_avg_vel = mean(sac_filtered_vel$Duration, na.rm = T)
    
    #adding to averaging data set
    alg_data = c(alg_fix_count_gaze, alg_fix_count_dis, alg_fix_count_vel,
                 alg_fix_dur_sum_gaze, alg_fix_dur_sum_dis, alg_fix_dur_sum_vel,
                 alg_fix_dur_avg_gaze, alg_fix_dur_avg_dis, alg_fix_dur_avg_vel,
                 alg_sac_count_gaze, alg_sac_count_dis, alg_sac_count_vel,
                 alg_sac_dur_sum_gaze, alg_sac_dur_sum_dis, alg_sac_dur_sum_vel,
                 alg_sac_dur_avg_gaze, alg_sac_dur_adj_avg_gaze, alg_sac_dur_avg_dis, alg_sac_dur_adj_avg_dis, alg_sac_dur_avg_vel)
    
    #Getting the subject's fixation and saccade groundtruth values
    
    #Fixation
    #Get to the right folder of 30 csvs
    setwd("~/Desktop/Data/FIXATIONS")
    event_folders_fix = list.dirs('.', recursive = F)
    ext_fix1 = event_folders_fix[i]
    ext_fix1 = substr(ext_fix1, 2, nchar(ext_fix1))
    ext_fix2 = paste(ext_fix1,"_FV", sep = "")
    event_path_fix = paste(getwd(), ext_fix1, ext_fix2, "/Dominant", sep = "") #Change Binocular to Dominant when doing Dominant data
    setwd(event_path_fix)
    file_list_fix = list.files(getwd(), pattern="*.csv")
    
    #Choose the respective folder that matches the subject from the algorithm 
    fix_file = file_list_fix[l] # change index to l if dominant data
    if(substr(alg_file,1,length(alg_file)-4) != substr(fix_file,1,length(alg_file)-4)){
      stop(c(alg_file,fix_file))
    }
    eye_data_fix = read.csv(fix_file, header = FALSE)
    fix_count = length(eye_data_fix[,1])
    fix_dur_avg = mean(eye_data_fix[,6], na.rm = T)
    fix_dur_sum = sum(eye_data_fix[,6])
    ground_data = c(rep(fix_count,3), rep(fix_dur_sum,3), rep(fix_dur_avg,3))
    
    #Saccade
    #Get to the right folder of 30 csvs
    setwd("~/Desktop/Data/SACCADES")
    event_folders_sac = list.dirs('.', recursive = F)
    ext_sac1 = event_folders_sac[i]
    ext_sac1 = substr(ext_sac1, 2, nchar(ext_sac1))
    ext_sac2 = paste(ext_sac1,"_FV", sep = "")
    event_path_sac = paste(getwd(), ext_sac1, ext_sac2, "/Dominant", sep = "") #Change Binocular to Dominant when doing Dominant data
    setwd(event_path_sac)
    file_list_sac = list.files(getwd(), pattern="*.csv");
    
    #Choose the respective folder that matches the subject from the algorithm
    sac_file = file_list_sac[l] # change index to l if dominant data
    if(substr(alg_file,1,length(alg_file)-4) != substr(sac_file,1,length(alg_file)-4)){
      stop(c(alg_file,sac_file))
    }
 
    eye_data_sac = read.csv(sac_file, header = FALSE)
    sac_count = length(eye_data_sac[,1])
    sac_dur_avg = mean(eye_data_sac[,6], na.rm = T)
    sac_dur_sum = sum(eye_data_sac[,6])
    ground_data = c(ground_data, rep(sac_count,3), rep(sac_dur_sum,3), rep(sac_dur_avg,5))
    
    
    #Comparing algorithm and groundtruth values
    squared_differences = (ground_data - alg_data)^2
    
    SD_event_data = rbind(SD_event_data, squared_differences) #30 values of each subject's squared diff values for four metrics 
    
    setwd(event_path)
    
    csv_count = csv_count + 1
    print(c(event_count, csv_count))
  }
  
  if(csv_count != 30) {
    stop(c("Incorrect # of CSV's:", ext1))
  }
  RMSE_values = ext1
  
  for (i in 1:20) {
    RMSE_value = RMSE(SD_event_data, i)
    RMSE_values = c(RMSE_values, RMSE_value)
  }
  
  RMSE_data = rbind(RMSE_data, RMSE_values)
  
  SD_event_data = data.frame()
  RMSE_values = c()
  alg_data = data.frame()
  
  setwd(main_path)
}

names(RMSE_data) <- c("Filename", "RMSE gazepath fixation count", "RMSE dispersion fixation count", "RMSE velocity fixation count",
                      "RMSE gazepath fixation cumulative duration", "RMSE dispersion fixation cumulative duration", "RMSE velocity fixation cumulative duration",
                      "RMSE gazepath fixation avg duration", "RMSE dispersion fixation avg duration", "RMSE velocity fixation avg duration",
                      "RMSE gazepath saccade count", "RMSE dispersion saccade count", "RMSE velocity saccade count",
                      "RMSE gazepath saccade cumulative duration", "RMSE dispersion saccade cumulative duration", "RMSE velocity saccade cumulative duration",
                      "RMSE gazepath saccade avg duration", "RMSE gazepath saccade adj avg duration", "RMSE dispersion saccade avg duration", "RMSE dispersion saccade adj avg duration", "RMSE velocity saccade avg duration") 

write.csv(RMSE_data,"/Users/Tarsus/Desktop/Data/RMSE/Dominant/StreetBasketball.csv", row.names = FALSE) #change this to your where you want to output the csv with the same title

