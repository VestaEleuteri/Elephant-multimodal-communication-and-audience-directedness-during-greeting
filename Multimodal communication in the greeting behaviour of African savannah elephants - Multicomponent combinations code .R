#Multimodal communication in the greeting behaviour of African savannah elephants - Multi-signal combinations code

#Required packages
library(dplyr)


#Sort dataset of overlapping vocalizations and body acts for Gries Collocation analysis script ----
##Sort data 
xdata=read.table("Elephant multimodal communication_greet_data.csv", header=T, sep=";", 
  fill=T, dec=",", stringsAsFactors=T)
str(xdata)
xdata=data.frame(xdata)

###Subset vocalizations data
xdata_voc_ms=subset(xdata, ! Signal %in% c("Gesture")) #exclude gestures (i.e., body acts/signals)

colnames(xdata_voc_ms)[colnames(xdata_voc_ms) == "S_start_T"] ="S_start_T_V" #rename 
colnames(xdata_voc_ms)[colnames(xdata_voc_ms) == "S_end_T"] ="S_end_T_V"
colnames(xdata_voc_ms)[colnames(xdata_voc_ms) == "S_duration"] ="S_duration_V"
str(xdata_voc_ms) #ok renamed
write.csv(x=xdata_voc_ms, file="Vocalization_data.csv") #Save dataset of only vocalizations

###Sort vocalization data
xdata_voc_ms_2=read.table("Vocalization_data.csv", header=T, sep=",", fill=T, dec=".", 
                          stringsAsFactors=T)
freq_vocs_dur=table(xdata_voc_ms_2$Signal, xdata_voc_ms_2$S_Dur_ana) #check durations
xdata_voc_ms_3=subset(xdata_voc_ms_2, ! S_Dur_ana %in% c("Exclude", 
 "NV", "Exclude_start")) #Exclude all rows for signals where start time or whole duration not clear

freq_voc_frame=table(xdata_voc_ms_3$Signal, xdata_voc_ms_3$V_Sgn_in_frame) #check signaller in frame 
                                                                           #when vocalizing
xdata_voc_ms_3=subset(xdata_voc_ms_3, ! V_Sgn_in_frame %in% c("No")) #remove cases where 
                                                                     #signaller not in frame
str(xdata_voc_ms_3) #check data                           

###Subset body act data
xdata_bodys_ms=subset(xdata, ! Signal %in% c("Vocalisation")) #exclude vocalizations
colnames(xdata_bodys_ms)[colnames(xdata_bodys_ms) == "S_start_T"] ="S_start_T_BS" #rename
colnames(xdata_bodys_ms)[colnames(xdata_bodys_ms) == "S_end_T"] ="S_end_T_BS"
colnames(xdata_bodys_ms)[colnames(xdata_bodys_ms) == "S_duration"] ="S_duration_BS"
write.csv(x=xdata_bodys_ms, file="Body act_data.csv") #Save dataset of only body acts
str(xdata_bodys_ms)

###Sort body act data
xdata_bodys_ms_2=read.table("Body act_data.csv", header=T, sep=";", 
                            fill=T, dec=".", stringsAsFactors=T)
str(xdata_bodys_ms_2)

xdata_bodys_ms_3=subset(xdata_bodys_ms_2, ! S_Dur_ana %in% c("Exclude", "NV", 
  "Exclude_start")) #Exclude all rows for signals where start time or whole duration not clear 
str(xdata_bodys_ms_3) #check data
freq_bodys_dur=table(xdata_bodys_ms_3$Signal, xdata_bodys_ms_3$S_Dur_ana)

###Join vocalization and body act datasets to check for overlapping multi-signal combinations
overlap_signals=inner_join(xdata_voc_ms_3, xdata_bodys_ms_3, by="Com_number") %>% 
  filter(S_start_T_V < S_start_T_BS | S_end_T_V > S_end_T_BS) #join data 

overlap_signals=overlap_signals%>%
  group_by(Com_number) %>%
  mutate(diff_V_B = S_start_T_V - S_start_T_BS) #Add column with time differences
                                                #between voc and body signals

write.csv(x=overlap_signals, file="Overlapping_signals.csv") #Then sorted data for MDCA1 and MDCA2 analyses
                                                             #in Excel file "Overlapping_signals_sorted" 
                                                             #(see file for details)

##Frequency overlapping vocalizations and body acts MDCA1
xdata_overlap=read.table("MDCA1_data.csv", header=T, sep=";", fill=T, dec=".", 
  stringsAsFactors=T) 
str(xdata_overlap)

overlap_voc_gest_freq=table(xdata_overlap$Signal_record_1,xdata_overlap$Signal_record_2) 
write.csv(x=overlap_voc_gest_freq, file="MDCA1_Overlapping_Signals_freq.csv") 

##Frequency overlapping vocalizations and body acts MDCA2
xdata_overlap2=read.table("MDCA2_data.csv", header=T, sep=";", fill=T, dec=".", 
  stringsAsFactors=T) 
str(xdata_overlap2)

overlap_voc_gest_freq2=table(xdata_overlap2$Signal_record_1,xdata_overlap2$Signal_record_2) 
write.csv(x=overlap_voc_gest_freq2, file="MDCA2_Overlapping_Signals_freq.csv") 

#To access the Collocation Analyses script developed by Gries: https://www.stgries.info/teaching/groningen/index.html 
