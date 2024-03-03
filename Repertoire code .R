#Multimodal communication in the greeting behaviour of African savannah elephants - Repertoire code

#Required packages
library(plyr)
library(dplyr)
library(ggplot2)
library(carData)
library(ggpattern)


#Sort main dataset----
xdata=read.table("Main_data.csv", header=T, sep=";", 
  fill=T, dec=",", stringsAsFactors=T)
str(xdata)
xdata2=subset(xdata, ! Goal %in% c("Aff_contact","Aff_contact_rcp",
  "Trv_sgn","Unk")) #Exclude all rows for goals other than greeting
xdata3=subset(xdata2, ! Signal_record %in% c("Unk", "Unclear", 
  "Other")) #Exclude all rows where Signal record Unknown, Unclear, Other

levels(xdata3$Distance_rcp)
xdata4=subset(xdata3, ! Distance_rcp %in% c(">100", "Out_of_sight")) #Exclude signals 
                                                                    #where recipient out of sight or above 100m
levels(xdata4$Signal_record) #Check levels Signal_record


#Sort greeting dataset----
xdata5=subset(xdata4, ! Signal_record %in% c("Bite-Other", "Ear-Slide", "Ear-Waving", 
  "Ear-Touch", "Face-Touch", "Foot-Lift", "Foot-Swinging", "Trunk-Grasp", "Head-Dip", "Head-Low", 
  "Head-Shake", "Head-Swinging", "Head-Touch", "Head-Waggling", "Periscope-Trunk", "Rump-Touch", 
  "T-Gland-Present-Present", "Trunk-Reach-Over", "Trunk-Self-Touch", "Trunk-Slap", "Snort", 
  "Suck-Self", "Tail-Reach", "Back-limb-Reach", "T-Gland-Present", "Trunk-Beckon",
  "Trunk-Drag", "Trunk-Flick","Trunk-Fling", "Trunk-Raise", "Trunk-Touch")) #Exclude signals not used 
                                                                           #by at least 2 individuals 
                                                                           #at least twice
Sgn_sgnler_freq=table(xdata5$Signal_record, xdata5$Signaller) 
setwd("~/Desktop/PhD/Analysis/Datasets/Descriptives") 
write.csv(x=xdata5, file="Greet_data.csv")


#Descriptives----
xdata=read.table("Elephant multimodal communication_greet_data.csv", header=T,
  sep=";", fill=T, dec=".", stringsAsFactors=T)
str(xdata)

##Frequency all signals x sex
voc_bodysg_sex_freq=table(xdata$Signal_record, xdata$Signaller_sex) 

##Frequency all signals x duration
Signals_dur_freq=table(xdata$Signal_record, xdata$S_Dur_ana) 

##Frequency vocalisations and body signals (in article=body acts)
voc_bodysg_n=table(xdata$Signal) #frequency signals (vocalisations and body signals)

voc_bodysg_freq=table(xdata$Signal_record, xdata$Signal) #frequency signal records x signal

voc_bodysg_sgn_freq=table(xdata$Signal_record, xdata$Signaller) #frequency signal records x signaller

##Data body signals
xdata_bodysignal=subset(xdata, Signal=="Gesture") #subset to only body signals (in dataset=gestures)

##Data vocalizations
xdata_voc=subset(xdata, Signal=="Vocalisation") #subset to only vocalisations

##Frequency body signals x modality 
bod_sig_mod_freq=table(xdata_bodysignal$Signal_record,
  xdata_bodysignal$Signal_modality) #check modalities x body signal records

bod_sig_mod_overall=table(xdata_bodysignal$Signal, 
  xdata_bodysignal$Signal_modality) #check body signal modalities

##Frequency body signal modalities x sex and sex dyad
Modality_sigsex_sexdyad=table(xdata_bodysignal$Signaller_sex, 
  xdata_bodysignal$Sex_Dyad, xdata_bodysignal$Signal_modality)

##Frequency body signals and distances
bodysg_dist_rcp_freq=table(xdata_bodysignal$Signal_record, xdata_bodysignal$Distance_rcp) 

##Sort signal modalities x distance 
xdata=xdata_bodysignal
levels(xdata$Distance_rcp)
levels(xdata$Distance_rcp)[levels(xdata$Distance_rcp)=="0"] <- "0-1M" #releveled 0 as 0-1M
levels(xdata$Distance_rcp)[levels(xdata$Distance_rcp)=="u_0"] <- "0-1M" #releveled u_0 as 0-1M
levels(xdata$Distance_rcp)[levels(xdata$Distance_rcp)=="0_1"] <- "0-1M" #releveled 0_1 as 0-1M

levels(xdata$Distance_rcp)[levels(xdata$Distance_rcp)=="1_3"] <- "1-3M" #releveled
levels(xdata$Distance_rcp)[levels(xdata$Distance_rcp)=="3_5"] <- "3-5M" #releveled
levels(xdata$Distance_rcp)[levels(xdata$Distance_rcp)=="5_10"] <- "5-10M" #releveled
levels(xdata$Distance_rcp)[levels(xdata$Distance_rcp)=="10_20"] <- "10-20M" #releveled
levels(xdata$Distance_rcp)[levels(xdata$Distance_rcp)=="20_50"] <- "20-50M" #releveled
levels(xdata$Distance_rcp)[levels(xdata$Distance_rcp)=="50_100"] <- "50-100M" #releveled
levels(xdata$Distance_rcp)

xdata$Distance_rcp<-factor(xdata$Distance_rcp, levels=c("0-1M", "1-3M", "3-5M",
  "5-10M", "10-20M", "20-50M", "50-100M"))
levels(xdata$Distance_rcp) #check

##Frequency modalities x distance (including tail body signals)
mod_dist_freq=table(xdata$Distance_rcp, xdata$Signal_modality) 

##Frequency body signals and modalities x distance (including tail body signals)
bodysg_dist_mod_rcp_freq=table(xdata$Signal_record, xdata$Distance_rcp, xdata$Signal_modality) 
write.csv(x=bodysg_dist_mod_rcp_freq, file="Bodysgn_dist_rcp_mod.csv") #used for calculations for 
                                                                       #Plot stacked histogram 
                                                                       #body acts - distance below

##Frequency modalities x distance (excluding tail body signals)
levels(xdata$Signal_record)
xdata_no_tail=subset(xdata, ! Signal_record %in% c( "Tail-on-Side", 
  "Tail-Raise", "Tail-Stiff", "Tail-Waggling")) #Exclude tail body signals where modality=Unk

levels(xdata_no_tail$Signal_record)
mod_dist_no_tail_freq=table(xdata_no_tail$Distance_rcp, 
  xdata_no_tail$Signal_modality) #modality frequency excluding tail body signals


##Plot Stacked histogram body signals - distance (Supplementary Figure 4)
###Sort data
xdata=read.table("Supplementary Figure 4_data.csv", header=T, sep=";", fill=T, dec=",", stringsAsFactors=T) #dataset sorted in Excel 
                                                                                              #from Bodysg_dist_rcp_mod.csv above
levels(xdata$Distance) #check levels
xdata$Distance<-factor(xdata$Distance, levels=c("0-1M", "1-3M", "3-5M", "5-10M", 
  "10-20M", "20-50M", "50-100M"))
levels(xdata$Distance) 

###Plot
ce=ddply(xdata, "Signal_record", transform, percent_weight=Percentage/sum(Percentage)*100)

bp=ggplot(ce, aes(x=Signal_record, y=percent_weight, fill=Distance)) + geom_bar(stat="identity")

bp+theme(axis.text.x=element_text(angle=90, hjust=1, 
  vjust=.5), legend.title=element_text(colour="black", size=16),
  legend.text=element_text(colour="black", size=16)) + ylab("Percentage") 
  + xlab("Body act") + theme(axis.title=element_text(size = 18),
  axis.text=element_text(size = 16)) + theme()

##Data for Excel Plots exploring variation in modality according to recipient visual attention 
 #(following Hobaiter & Byrne, 2011)
mod_sgn_freq=table(xdata_bodysignal$Signaller, xdata_bodysignal$Signal_modality) #signaller-modality frequency

xdata_bodysignal_2=subset(xdata_bodysignal, ! Signaller %in% c("MS")) #Exclude MS (Masuwe) 
                                                                      #because she has 
                                                                      #no signals in tactile modality
xdata_bodysignal_3=subset(xdata_bodysignal_2, ! Rcp_Visual_att %in% c("Unclear", 
  "Unk", "Out_of_sight")) #Exclude from dataset above where recipient visual attention unclear/unknown/out of sight
levels(xdata_bodysignal_3$Rcp_Visual_att)[levels(xdata_bodysignal_3$Rcp_Visual_att)=="VA_front"] <- "VA" #relevel to attending=VA
levels(xdata_bodysignal_3$Rcp_Visual_att)[levels(xdata_bodysignal_3$Rcp_Visual_att)=="VA_90"] <- "VA" #relevel to attending=VA
levels(xdata_bodysignal_3$Rcp_Visual_att)[levels(xdata_bodysignal_3$Rcp_Visual_att)=="Looking_Back"] <- "VA" #relevel to attending=VA
levels(xdata_bodysignal_3$Rcp_Visual_att) #check

##Frequency recipient visual attention x signaller
RcpVA_sgn_freq=table(xdata_bodysignal_3$Rcp_Visual_att, 
  xdata_bodysignal_3$Signaller) #all body signals included

##Frequency modality x recipient visual attention
mod_RcpVA_freq=table(xdata_bodysignal_3$Signal_modality, 
  xdata_bodysignal_3$Rcp_Visual_att) #all body signals included
write.csv(x=mod_RcpVA_freq, file="Mod_RcpVA_freq.csv") #used for Excel plot in Figure 3 exploring variation 
                                                       #in modality according to recipient visual attention


##Frequency recipient visual attention x tail body signals
xdata_bodysignal_only_tail=subset(xdata_bodysignal_3, Signal_record %in% c( "Tail-on-Side", 
  "Tail-Raise", "Tail-Stiff", "Tail-Waggling")) #Exclude from dataset all body signal except tail ones
levels(xdata_bodysignal_only_tail$Signal_record)

RcpVA_tail_freq=table(xdata_bodysignal_only_tail$Rcp_Visual_att)  
write.csv(x=RcpVA_tail_freq, file="RcpVA_tail_freq.csv")         #Data used for Excel plot in Supplementary 
                                                                 #Figure 3 exploring if tail body acts may
                                                                 #be silent-visual body acts by comparing 
                                                                 #distribution of tail body acts 
                                                                 #according to recipient visual attention 
                                                                 #as compared to distribution 
                                                                 #of established silent-visual body acts 
                                                                 #according to recipient visual attention



##Frequency body signals and recipient visual attention (all signallers included)
xdata_bodysignal_4=subset(xdata_bodysignal, ! Rcp_Visual_att %in% c("Unclear", 
  "Unk", "Out_of_sight")) #Excluded from dataset above levels where recipient visual attention not clear
levels(xdata_bodysignal_4$Rcp_Visual_att)[levels(xdata_bodysignal_4$Rcp_Visual_att)=="VA_front"] <- "VA" #relevel to attending VA
levels(xdata_bodysignal_4$Rcp_Visual_att)[levels(xdata_bodysignal_4$Rcp_Visual_att)=="VA_90"] <- "VA" #relevel to attending VA
levels(xdata_bodysignal_4$Rcp_Visual_att)[levels(xdata_bodysignal_4$Rcp_Visual_att)=="Looking_Back"] <- "VA" #relevel to attending VA
levels(xdata_bodysignal_4$Rcp_Visual_att)

BodySgn_RcpVA_freq=table(xdata_bodysignal_4$Signal_record, 
  xdata_bodysignal_4$Rcp_Visual_att) #Frequency recipient visual attention x body signal
write.csv(x=BodySgn_RcpVA_freq, file="Bodysgn_RcpVA-NoVA_freq.csv") #used for Excel plot in Figure 3 exploring variation 
                                                                    #in modality according to recipient visual attention


##Plot Stacked histogram body acts - recipient visual attention (Figure 3)
xdata=read.table("Figure 3_data.csv", header=T, sep=";", fill=T, dec=",", stringsAsFactors=T) #dataset sorted in Excel from Bodysgn_RcpVA-NoVA_freq.csv above
ce=ddply(xdata, "Signal_record", transform, percent_weight=Percentage/sum(Percentage)*100)

bp=ggplot(ce, aes(x=Signal_record, y=percent_weight, fill=Rcp_Visual_Att)) + geom_bar(stat="identity")

bp+theme(axis.text.x=element_text(angle=90, hjust=1, 
  vjust=.5), legend.title = element_text(colour="black", size=16),
  legend.text=element_text(colour="black", size=16)) + ylab("Percentage") 
  + xlab("Body act") + labs(fill="Recipient visual attention") + 
  theme(axis.title = element_text(size = 18),
  axis.text = element_text(size = 16)) + theme()


##Frequency body signals x signaller gaze 
xdata_bodysignal_5=subset(xdata_bodysignal, ! Sig_gaze %in% c("Unclear", "Unk",
  "Out_of_sight")) #Exclude from main bodysignal dataset Sig_gaze levels where gaze signaller not clear
levels(xdata_bodysignal_5$Sig_gaze)[levels(xdata_bodysignal_5$Sig_gaze)=="VA_front"] <- "VA" #relevel to VA
levels(xdata_bodysignal_5$Sig_gaze)[levels(xdata_bodysignal_5$Sig_gaze)=="VA_90"] <- "VA" #relevel to VA
levels(xdata_bodysignal_5$Sig_gaze)[levels(xdata_bodysignal_5$Sig_gaze)=="Looking_Back"] <- "VA" #relevel to VA
BodySgn_Sig_gaze_freq=table(xdata_bodysignal_5$Signal_record, xdata_bodysignal_5$Sig_gaze)
write.csv(x=BodySgn_Sig_gaze_freq, file="Bodysgn_SigVA-NoVA_freq.csv") #used for Excel plot in Supplementary Figure 2 exploring variation 
                                                                      #in modality according to signaller visual attention



##Plot Stacked histogram body acts - signaller visual attention (Supplementary Figure 2)
xdata=read.table("Supplementary Figure 2_data.csv", header=T, sep=";", fill=T, dec=",", 
  stringsAsFactors=T) #dataset sorted in Excel
                     #from Bodysgn_SigVA-NoVA_freq.csv above

ce=ddply(xdata, "Signal_record", transform, percent_weight=Percentage/sum(Percentage)*100)

bp=ggplot(ce, aes(x=Signal_record, y=percent_weight, fill=Sgn_gaze)) + geom_bar(stat="identity")

bp+theme(axis.text.x=element_text(angle=90, hjust=1, 
  vjust=.5),legend.title=element_text(colour="black", size=16),
  legend.text=element_text(colour="black", size=16)) 
  + ylab("Percentage") + xlab("Body act") + 
  labs(fill="Signaller visual attention") +
  theme(axis.title=element_text(size = 18),
  axis.text=element_text(size = 16)) + theme()


##Plot Signal Repertoire (Supplementary Figure 1)
xdata=read.table("Greet_data.csv",
  header=T, sep=",", fill=T, dec=".", stringsAsFactors=T) 

###Order data by Com_number
xdata_sorted <- xdata[order(xdata$Com_number), ]

###Define a sequence of numbers in seq
#with intervals of 10
#over the number of rows in xdata
n_sequence <- seq(0, nrow(xdata_sorted), by = 10)

###Signaltypecount is updated every iteration
n_signals_types <- c()
signaltypecount <- 0

###Loop through n_sequence
for (i in n_sequence) { 
  
  ###Select the subset of xdata with n_sequence number of rows
  xdata_subset <- subset(xdata_sorted[1:i, ], select = Signal_record)
  
  ###Count the number of distinct signal record types
  signaltypecount <- n_distinct(xdata_subset$Signal_record)
  
  ###Add the number of unique signals coded in xdata_subset
  n_signals_types <- c(n_signals_types, signaltypecount)
  
  ###Reset signaltypecount
  signalcount <- 0
}

n_signals_types #check
  
###Create a dataframe of n signals coded and n signal types
cum_plot_data=data.frame(n_sequence, n_signals_types)          
cum_plot_data #check


###Plot data
cum_plot=ggplot(cum_plot_data, aes(x = n_sequence, y = n_signals_types)) +
  geom_point(size=3)

cum_plot+theme(legend.title = element_text(colour="black", size=16),
  legend.text=element_text(colour="black", size=16)) + 
  ylab("Number of signal types") + xlab("Number of signals coded") 
  + theme_classic() + scale_x_continuous(breaks=seq(0, 1300, 100)) +
  theme(axis.title = element_text(size = 18),
  axis.text = element_text(size = 16)) + theme()


##Use of olfactory behaviours: defecating, urinating, temporal gland secretions
xdata=read.table("Greet_data.csv", header=T, sep=",", 
  fill=T, dec=",", stringsAsFactors=T)

levels(xdata$Sgn_olf_signals) #check levels
Olf_behaviours=table(xdata$Com_number, xdata$Sgn_olf_signals)

