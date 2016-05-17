#######################################################################
########################## DATA PREPARATION ###########################
#######################################################################

# Define Variables:
ID<- NULL # numbers refer to the reference list at the bottom
Paper<- NULL
Effect<-NULL
Boundary<- NULL
Language<- NULL
N <- NULL   # number of subjects

#N
FFD_N_val<-NULL; FFD_N_inval<-NULL                     # First fixation duration (N)- mean
FFD_N_val_SD<-NULL; FFD_N_inval_SD<-NULL               # First fixation duration (N)- SD
Gaze_N_val<-NULL; Gaze_N_inval<-NULL                   # Gaze duration (N)- mean
Gaze_N_val_SD<-NULL; Gaze_N_inval_SD<-NULL             # Gaze duration (N)- SD (Stan. Dev.)
Total_N_val<-NULL; Total_N_inval<-NULL                 # Total fixations duration (N)- mean
Total_N_val_SD<-NULL; Total_N_inval_SD<-NULL           # Total fixations duration (N)- SD
SFD_N_val<- NULL; SFD_N_inval<- NULL                   # Single fixation duration (N)- mean
SFD_N_val_SD<- NULL; SFD_N_inval_SD<- NULL             # Single fixation duration (N)- SD
FixProb_N_val<- NULL; FixProb_N_inval<- NULL           # Fixation probability (N)- mean
FixProb_N_val_SD<- NULL; FixProb_N_inval_SD<- NULL     # Fixation probability (N)- SD
LandPos_N_val<- NULL; LandPos_N_inval<- NULL           # Landing Position (N)- mean
LandPos_N_val_SD<- NULL; LandPos_N_inval_SD<- NULL     # Landing Position (N)- SD

#N+1
FFD_N1_val<-NULL; FFD_N1_inval<-NULL                   # First fixation duration (N+1)- mean
FFD_N1_val_SD<-NULL; FFD_N1_inval_SD<-NULL             # First fixation duration (N+1)- SD
Gaze_N1_val<-NULL; Gaze_N1_inval<-NULL                 # Gaze duration (N+1)- mean
Gaze_N1_val_SD<-NULL; Gaze_N1_inval_SD<-NULL           # Gaze duration (N+1)- SD (Stan. Dev.)
Total_N1_val<-NULL; Total_N1_inval<-NULL               # Total fixations duration (N+1)- mean
Total_N1_val_SD<-NULL; Total_N1_inval_SD<-NULL         # Total fixations duration (N+1)- SD
FixProb_N1_val<- NULL; FixProb_N1_inval<- NULL         # Fixation probability (N+1)- mean
FixProb_N1_val_SD<- NULL; FixProb_N1_inval_SD<- NULL   # Fixation probability (N+1)- SD
LandPos_N1_val<- NULL; LandPos_N1_inval<- NULL         # Landing Position (N+1)- mean
LandPos_N1_val_SD<- NULL; LandPos_N1_inval_SD<- NULL   # Landing Position (N+1)- SD
#N+2
FFD_N2_val<-NULL; FFD_N2_inval<-NULL                   # First fixation duration (N+2)- mean
FFD_N2_val_SD<-NULL; FFD_N2_inval_SD<-NULL             # First fixation duration (N+2)- SD
Gaze_N2_val<-NULL; Gaze_N2_inval<-NULL                 # Gaze duration (N+2)- mean
Gaze_N2_val_SD<-NULL; Gaze_N2_inval_SD<-NULL           # Gaze duration (N+2)- SD
Total_N2_val<-NULL; Total_N2_inval<-NULL               # Total fixations duration (N+2)- mean
Total_N2_val_SD<-NULL; Total_N2_inval_SD<-NULL         # Total fixations duration (N+2)- SD
FixProb_N2_val<- NULL; FixProb_N2_inval<- NULL         # Fixation probability (N+2)- mean
FixProb_N2_val_SD<- NULL; FixProb_N2_inval_SD<- NULL   # Fixation probability (N+2)- SD
LandPos_N2_val<- NULL; LandPos_N2_inval<- NULL         # Landing Position (N+2)- mean
LandPos_N2_val_SD<- NULL; LandPos_N2_inval_SD<- NULL   # Landing Position (N+2)- SD

#########
#Studies:
#########


####
#1
####
ID[1]<-1
Paper[1]<- "Rayner, Juhasz, & Brown (2007), Exp. 1"
Effect[1]<-"PB"
Boundary[1]<- 'N+2'
Language[1]<-'English'
N[1]<-30
#N
FFD_N_val<-NA; FFD_N_inval<-NA  
FFD_N_val_SD<-NA; FFD_N_inval_SD<-NA
Gaze_N_val<-NA; Gaze_N_inval<-NA
Gaze_N_val_SD<-NA; Gaze_N_inval_SD<-NA
Total_N_val<-NA; Total_N_inval<-NA
Total_N_val_SD<-NA; Total_N_inval_SD<-NA
SFD_N_val<-NA; SFD_N_inval<-NA
SFD_N_val_SD<-NA; SFD_N_inval_SD<-NA
FixProb_N_val<-NA; FixProb_N_inval<-NA
FixProb_N_val_SD<-NA; FixProb_N_inval_SD<-NA
LandPos_N_val<- NA; LandPos_N_inval<- NA
LandPos_N_val_SD<- NA; LandPos_N_inval_SD<- NA
# N+1 
FFD_N1_val<-NA; FFD_N1_inval<-NA  # Table 2, non-word preview
FFD_N1_val_SD<-NA; FFD_N1_inval_SD<-NA
Gaze_N1_val<-NA; Gaze_N1_inval<-NA
Gaze_N1_val_SD<-NA; Gaze_N1_inval_SD<-NA
Total_N1_val<-NA; Total_N1_inval<-NA
Total_N1_val_SD<-NA; Total_N1_inval_SD<-NA
SFD_N1_val<-NA; SFD_N1_inval<-NA
SFD_N1_val_SD<-NA; SFD_N1_inval_SD<-NA
FixProb_N1_val<-NA; FixProb_N1_inval<-NA
FixProb_N1_val_SD<-NA; FixProb_N1_inval_SD<-NA
LandPos_N1_val<- NA; LandPos_N1_inval<- NA
LandPos_N1_val_SD<- NA; LandPos_N1_inval_SD<- NA
#N+2  
FFD_N2_val<-283; FFD_N2_inval<-290 # Table 3, non-word preview
FFD_N2_val_SD<-34; FFD_N2_inval_SD<-33
Gaze_N2_val<-322; Gaze_N2_inval<-315
Gaze_N2_val_SD<-51; Gaze_N2_inval_SD<-37
Total_N2_val<-368; Total_N2_inval<-358
Total_N2_val_SD<-84; Total_N2_inval_SD<-72
SFD_N2_val<-NA; SFD_N2_inval<-NA
SFD_N2_val_SD<-NA; SFD_N2_inval_SD<-NA
FixProb_N2_val<- 0.96; FixProb_N2_inval<- 0.958
FixProb_N2_val_SD<- 0.099; FixProb_N2_inval_SD<- 0.082
LandPos_N2_val<- 3.44; LandPos_N2_inval<- 3.51
LandPos_N2_val_SD<- 0.9; LandPos_N2_inval_SD<- 0.6

########
#2
########
ID[2]<-2
Paper[2]<- "Rayner, Juhasz, & Brown (2007), Exp. 2"
Effect[2]<-"PB"
Boundary[2]<- 'N+2'
Language[2]<- 'English'
N[2]<-36
#N
FFD_N_val[2]<-NA; FFD_N_inval[2]<-NA  
FFD_N_val_SD[2]<-NA; FFD_N_inval_SD[2]<-NA
Gaze_N_val[2]<-NA; Gaze_N_inval[2]<-NA
Gaze_N_val_SD[2]<-NA; Gaze_N_inval_SD[2]<-NA
Total_N_val[2]<-NA; Total_N_inval[2]<-NA
Total_N_val_SD[2]<-NA; Total_N_inval_SD[2]<-NA
SFD_N_val[2]<-NA; SFD_N_inval[2]<-NA
SFD_N_val_SD[2]<-NA; SFD_N_inval_SD[2]<-NA
FixProb_N_val[2]<-NA; FixProb_N_inval[2]<-NA
FixProb_N_val_SD[2]<-NA; FixProb_N_inval_SD[2]<-NA
LandPos_N_val[2]<- NA; LandPos_N_inval[2]<- NA
LandPos_N_val_SD[2]<- NA; LandPos_N_inval_SD[2]<- NA
# N+1
FFD_N1_val[2]<-NA; FFD_N1_inval[2]<-NA      
FFD_N1_val_SD[2]<-NA; FFD_N1_inval_SD[2]<-NA
Gaze_N1_val[2]<-NA; Gaze_N1_inval[2]<-NA
Gaze_N1_val_SD[2]<-NA ; Gaze_N1_inval_SD[2]<- NA
Total_N1_val[2]<-NA; Total_N1_inval[2]<-NA
Total_N1_val_SD[2]<-NA; Total_N1_inval_SD[2]<-NA
SFD_N1_val[2]<-NA; SFD_N1_inval[2]<-NA
SFD_N1_val_SD[2]<-NA; SFD_N1_inval_SD[2]<-NA
FixProb_N1_val[2]<-NA; FixProb_N1_inval[2]<-NA
FixProb_N1_val_SD[2]<-NA; FixProb_N1_inval_SD[2]<-NA
LandPos_N1_val[2]<-NA ; LandPos_N1_inval[2]<-NA
LandPos_N1_val_SD[2]<-NA ; LandPos_N1_inval_SD[2]<-NA
#N+2  
FFD_N2_val[2]<-291; FFD_N2_inval[2]<-290  # Table 8, non-word
FFD_N2_val_SD[2]<-47; FFD_N2_inval_SD[2]<-59
Gaze_N2_val[2]<-312; Gaze_N2_inval[2]<-312
Gaze_N2_val_SD[2]<-67; Gaze_N2_inval_SD[2]<-77
Total_N2_val[2]<-335; Total_N2_inval[2]<-347
Total_N2_val_SD[2]<-79; Total_N2_inval_SD[2]<-92
SFD_N2_val[2]<-NA; SFD_N2_inval[2]<-NA
SFD_N2_val_SD[2]<-NA; SFD_N2_inval_SD[2]<-NA
FixProb_N2_val[2]<- 0.752; FixProb_N2_inval[2]<-0.74
FixProb_N2_val_SD[2]<- 0.21; FixProb_N2_inval_SD[2]<-0.212
LandPos_N2_val[2]<-2.08 ; LandPos_N2_inval[2]<-2.04
LandPos_N2_val_SD[2]<-0.5 ; LandPos_N2_inval_SD[2]<-0.6 

#####
#3      
#####

# Separate stats are reported for content and function words, so I take the mean of the two

ID[3]<-3
Paper[3]<- "Kliegl, Risse, & Laubrock (2007)"
Effect[3]<-"PB+PoF"
Boundary[3]<- 'N+2'
Language[3]<- 'German'
N[3]<-30
#N
FFD_N_val[3]<-(200+195)/2; FFD_N_inval[3]<-(205+196)/2  # Table 3
FFD_N_val_SD[3]<-(59+62)/2; FFD_N_inval_SD[3]<-(61+60)/2
Gaze_N_val[3]<-(237+218)/2; Gaze_N_inval[3]<-(252+219)/2
Gaze_N_val_SD[3]<-(101+89)/2; Gaze_N_inval_SD[3]<-(116+85)/2
Total_N_val[3]<-NA; Total_N_inval[3]<-NA
Total_N_val_SD[3]<-NA; Total_N_inval_SD[3]<-NA
SFD_N_val[3]<-NA; SFD_N_inval[3]<-NA
SFD_N_val_SD[3]<-NA; SFD_N_inval_SD[3]<-NA
FixProb_N_val[3]<-(1-(0.03+0.05)/2); FixProb_N_inval[3]<-(1-(0.03+0.04)/2)
FixProb_N_val_SD[3]<-(1-(0.18+0.22)/2); FixProb_N_inval_SD[3]<-(1-(0.16+0.20)/2)
LandPos_N_val[3]<- NA; LandPos_N_inval[3]<- NA
LandPos_N_val_SD[3]<- NA; LandPos_N_inval_SD[3]<- NA
# N+1 
FFD_N1_val[3]<-(190+203)/2; FFD_N1_inval[3]<-(201+212)/2 # Table 1
FFD_N1_val_SD[3]<-(56+58)/2; FFD_N1_inval_SD[3]<-(55+60)/2 # 
Gaze_N1_val[3]<-(196+209)/2 ; Gaze_N1_inval[3]<- (207+219)/2
Gaze_N1_val_SD[3]<-(70+65)/2 ; Gaze_N1_inval_SD[3]<- (65+65)/2
Total_N1_val[3]<-NA; Total_N1_inval[3]<-NA
Total_N1_val_SD[3]<-NA; Total_N1_inval_SD[3]<-NA
SFD_N1_val[3]<-NA; SFD_N1_inval[3]<-NA
SFD_N1_val_SD[3]<-NA; SFD_N1_inval_SD[3]<-NA
FixProb_N1_val[3]<- 1- ((0.42+0.56)/2); FixProb_N1_inval[3]<- 1-((0.40+0.55)/2)
FixProb_N1_val_SD[3]<- 1- ((0.49+0.50)/2); FixProb_N1_inval_SD[3]<- 1-((0.49+0.50)/2)
LandPos_N1_val[3]<- NA ; LandPos_N1_inval[3]<-NA
LandPos_N1_val_SD[3]<- NA ; LandPos_N1_inval_SD[3]<-NA
#N+2  
FFD_N2_val[3]<-(206+204)/2; FFD_N2_inval[3]<- (206+202)/2
FFD_N2_val_SD[3]<-(68+62)/2; FFD_N2_inval_SD[3]<- (72+65)/2
Gaze_N2_val[3]<-(238+240)/2; Gaze_N2_inval[3]<- (247+240)/2
Gaze_N2_val_SD[3]<-(107+100)/2; Gaze_N2_inval_SD[3]<- (119+109)/2
Total_N2_val[3]<-NA; Total_N2_inval[3]<-NA
Total_N2_val_SD[3]<-NA; Total_N2_inval_SD[3]<-NA
SFD_N2_val[3]<-NA; SFD_N2_inval[3]<-NA
SFD_N2_val_SD[3]<-NA; SFD_N2_inval_SD[3]<-NA
FixProb_N2_val[3]<- 1-((0.11+0.11)/2) ; FixProb_N2_inval[3]<- 1-((0.12+0.11)/2) 
FixProb_N2_val_SD[3]<- 1-((0.31+0.31)/2) ; FixProb_N2_inval_SD[3]<- 1-((0.32+0.31)/2) 
LandPos_N2_val[3]<- NA; LandPos_N2_inval[3]<-NA 
LandPos_N2_val_SD[3]<- NA; LandPos_N2_inval_SD[3]<-NA 

#####
#4
#####

ID[4]<-4
Paper[4]<- "Yang, Wang, Xu, & Rayner (2009)"
Effect[4]<-"PB + PoF"
Boundary[4]<- 'N+2'
Language[4]<- 'Chinese'
N[4]<-42
#N
FFD_N_val[4]<-NA; FFD_N_inval[4]<-NA  
FFD_N_val_SD[4]<-NA; FFD_N_inval_SD[4]<-NA
Gaze_N_val[4]<-NA; Gaze_N_inval[4]<-NA
Gaze_N_val_SD[4]<-NA; Gaze_N_inval_SD[4]<-NA
Total_N_val[4]<-NA; Total_N_inval[4]<-NA
Total_N_val_SD[4]<-NA; Total_N_inval_SD[4]<-NA
SFD_N_val[4]<-NA; SFD_N_inval[4]<-NA
SFD_N_val_SD[4]<-NA; SFD_N_inval_SD[4]<-NA
FixProb_N_val[4]<-NA; FixProb_N_inval[4]<-NA
FixProb_N_val_SD[4]<-NA; FixProb_N_inval_SD[4]<-NA
LandPos_N_val[4]<- NA; LandPos_N_inval[4]<- NA
LandPos_N_val_SD[4]<- NA; LandPos_N_inval_SD[4]<- NA
# N+1
FFD_N1_val[4]<-210; FFD_N1_inval[4]<-224 # Table 2
FFD_N1_val_SD[4]<-81; FFD_N1_inval_SD[4]<-88
Gaze_N1_val[4]<-217; Gaze_N1_inval[4]<-233
Gaze_N1_val_SD[4]<-93; Gaze_N1_inval_SD[4]<- 96
Total_N1_val[4]<-NA; Total_N1_inval[4]<-NA
Total_N1_val_SD[4]<-NA; Total_N1_inval_SD[4]<- NA
SFD_N1_val[4]<-NA; SFD_N1_inval[4]<-NA
SFD_N1_val_SD[4]<-NA; SFD_N1_inval_SD[4]<-NA
FixProb_N1_val[4]<-1-0.57; FixProb_N1_inval[4]<- 1-0.56
FixProb_N1_val_SD[4]<-1-0.48; FixProb_N1_inval_SD[4]<-1-0.49
LandPos_N1_val[4]<-NA ; LandPos_N1_inval[4]<-NA
LandPos_N1_val_SD[4]<-NA ; LandPos_N1_inval_SD[4]<-NA
#N+2  
FFD_N2_val[4]<-229; FFD_N2_inval[4]<-236     # Table 2
FFD_N2_val_SD[4]<-68; FFD_N2_inval_SD[4]<-83
Gaze_N2_val[4]<-242; Gaze_N2_inval[4]<-251
Gaze_N2_val_SD[4]<-77; Gaze_N2_inval_SD[4]<-94
Total_N2_val[4]<-NA; Total_N2_inval[4]<-NA
Total_N2_val_SD[4]<-NA; Total_N2_inval_SD[4]<-NA
SFD_N2_val[4]<-NA; SFD_N2_inval[4]<-NA
SFD_N2_val_SD[4]<-NA; SFD_N2_inval_SD[4]<-NA
FixProb_N2_val[4]<- 1-0.65; FixProb_N2_inval[4]<-1-0.61
FixProb_N2_val_SD[4]<- 1-0.5; FixProb_N2_inval_SD[4]<-1-0.5
LandPos_N2_val[4]<-NA ; LandPos_N2_inval[4]<-NA
LandPos_N2_val_SD[4]<-NA ; LandPos_N2_inval_SD[4]<-NA


#####
#5
#####
# separate stats for two one-word charachters and one two-word characters; I take the mean of the two

ID[5]<-5
Paper[5]<- "Yang, Wang, Xu, & Rayner (2009), Exp.2"
Effect[5]<-"PB+PoF"
Boundary[5]<- 'N+2'
Language[5]<- 'Chinese'
N[5]<-66
#N
FFD_N_val[5]<-NA; FFD_N_inval[5]<-NA  
FFD_N_val_SD[5]<-NA; FFD_N_inval_SD[5]<-NA
Gaze_N_val[5]<-NA; Gaze_N_inval[5]<-NA
Gaze_N_val_SD[5]<-NA; Gaze_N_inval_SD[5]<-NA
Total_N_val[5]<-NA; Total_N_inval[5]<-NA
Total_N_val_SD[5]<-NA; Total_N_inval_SD[5]<-NA
SFD_N_val[5]<-NA; SFD_N_inval[5]<-NA
SFD_N_val_SD[5]<-NA; SFD_N_inval_SD[5]<-NA
FixProb_N_val[5]<-NA; FixProb_N_inval[5]<-NA
FixProb_N_val_SD[5]<-NA; FixProb_N_inval_SD[5]<-NA
LandPos_N_val[5]<- NA; LandPos_N_inval[5]<- NA
LandPos_N_val_SD[5]<- NA; LandPos_N_inval_SD[5]<- NA
# N+1
FFD_N1_val[5]<-NA; FFD_N1_inval[5]<-NA  # Table 4
FFD_N1_val_SD[5]<-NA; FFD_N1_inval_SD[5]<-NA
Gaze_N1_val[5]<-NA ; Gaze_N1_inval[5]<- NA
Gaze_N1_val_SD[5]<-NA; Gaze_N1_inval_SD[5]<- NA
Total_N1_val[5]<-NA; Total_N1_inval[5]<-NA
Total_N1_val_SD[5]<-NA; Total_N1_inval_SD[5]<- NA
SFD_N1_val[5]<-NA; SFD_N1_inval[5]<-NA
SFD_N1_val_SD[5]<-NA; SFD_N1_inval_SD[5]<-NA
FixProb_N1_val[5]<-NA; FixProb_N1_inval[5]<- NA
FixProb_N1_val_SD[5]<-NA; FixProb_N1_inval_SD[5]<-NA
LandPos_N1_val[5]<-NA ; LandPos_N1_inval[5]<-NA
LandPos_N1_val_SD[5]<-NA ; LandPos_N1_inval_SD[5]<-NA
#N+2  
FFD_N2_val[5]<-(238+229)/2; FFD_N2_inval[5]<-(245+241)/2 # Table 4
FFD_N2_val_SD[5]<-(68+69)/2; FFD_N2_inval_SD[5]<-(70+78)/2
Gaze_N2_val[5]<-(277+302)/2; Gaze_N2_inval[5]<-(300+330)/2
Gaze_N2_val_SD[5]<-(120+154)/2; Gaze_N2_inval_SD[5]<-(137+166)/2
Total_N2_val[5]<-NA; Total_N2_inval[5]<-NA
Total_N2_val_SD[5]<-NA; Total_N2_inval_SD[5]<-NA
SFD_N2_val[5]<-NA; SFD_N2_inval[5]<-NA
SFD_N2_val_SD[5]<-NA; SFD_N2_inval_SD[5]<-NA
FixProb_N2_val[5]<- 1-(0.11+0.11)/2; FixProb_N2_inval[5]<-1-(0.07+0.09)/2
FixProb_N2_val_SD[5]<- 1-(0.31+0.31)/2; FixProb_N2_inval_SD[5]<-1-(0.25+0.29)/2
LandPos_N2_val[5]<-NA ; LandPos_N2_inval[5]<-NA
LandPos_N2_val_SD[5]<-NA ; LandPos_N2_inval_SD[5]<-NA

#####
#6
#####
# Also has SFD
ID[6]<-6
Paper[6]<- "Yang, Rayner, Li, & Wang (2012)"
Effect[6]<-"PB"
Boundary[6]<- 'N+2'
Language[6]<- 'Chinese'
N[6]<-36
#N
FFD_N_val[6]<-NA; FFD_N_inval[6]<-NA  # Table 
FFD_N_val_SD[6]<-NA; FFD_N_inval_SD[6]<-NA
Gaze_N_val[6]<-NA; Gaze_N_inval[6]<-NA
Gaze_N_val_SD[6]<-NA; Gaze_N_inval_SD[6]<-NA
Total_N_val[6]<-NA; Total_N_inval[6]<-NA
Total_N_val_SD[6]<-NA; Total_N_inval_SD[6]<-NA
SFD_N_val[6]<-NA; SFD_N_inval[6]<-NA
SFD_N_val_SD[6]<-NA; SFD_N_inval_SD[6]<-NA
FixProb_N_val[6]<-NA; FixProb_N_inval[6]<-NA
FixProb_N_val_SD[6]<-NA; FixProb_N_inval_SD[6]<-NA
LandPos_N_val[6]<- NA; LandPos_N_inval[6]<- NA
LandPos_N_val_SD[6]<- NA; LandPos_N_inval_SD[6]<- NA
# N+1
FFD_N1_val[6]<-260; FFD_N1_inval[6]<-266 # Table 1
FFD_N1_val_SD[6]<-46; FFD_N1_inval_SD[6]<-46
Gaze_N1_val[6]<-286 ; Gaze_N1_inval[6]<-278
Gaze_N1_val_SD[6]<-62; Gaze_N1_inval_SD[6]<- 55
Total_N1_val[6]<-NA; Total_N1_inval[6]<-NA
Total_N1_val_SD[6]<-NA; Total_N1_inval_SD[6]<- NA
SFD_N1_val[6]<-NA; SFD_N1_inval[6]<-NA
SFD_N1_val_SD[6]<-NA; SFD_N1_inval_SD[6]<-NA
FixProb_N1_val[6]<-NA; FixProb_N1_inval[6]<- NA
FixProb_N1_val_SD[6]<-NA; FixProb_N1_inval_SD[6]<-NA
LandPos_N1_val[6]<-NA ; LandPos_N1_inval[6]<-NA
LandPos_N1_val_SD[6]<-NA ; LandPos_N1_inval_SD[6]<-NA
#N+2  
FFD_N2_val[6]<-254; FFD_N2_inval[6]<-258
FFD_N2_val_SD[6]<-41; FFD_N2_inval_SD[6]<-41
Gaze_N2_val[6]<-323; Gaze_N2_inval[6]<-323
Gaze_N2_val_SD[6]<-69; Gaze_N2_inval_SD[6]<-87
Total_N2_val[6]<-NA; Total_N2_inval[6]<-NA
Total_N2_val_SD[6]<-NA; Total_N2_inval_SD[6]<-NA
SFD_N2_val[6]<-255; SFD_N2_inval[6]<-266
SFD_N2_val_SD[6]<-48; SFD_N2_inval_SD[6]<-49
FixProb_N2_val[6]<- NA; FixProb_N2_inval[6]<-NA
FixProb_N2_val_SD[6]<-NA; FixProb_N2_inval_SD[6]<-NA
LandPos_N2_val[6]<-NA ; LandPos_N2_inval[6]<-NA
LandPos_N2_val_SD[6]<-NA ; LandPos_N2_inval_SD[6]<-NA

#####
#7
#####

ID[7]<-7
Paper[7]<- "Angele, Slattery, Yang, Kliegl, & Rayner (2008)"
Effect[7]<-"PB"
Boundary[7]<- 'N+1/N+2'
Language[7]<- 'English'
N[7]<-32
#N
FFD_N_val[7]<-229; FFD_N_inval[7]<-226  # Table 2
FFD_N_val_SD[7]<-32; FFD_N_inval_SD[7]<-30
Gaze_N_val[7]<-286; Gaze_N_inval[7]<-278
Gaze_N_val_SD[7]<-65; Gaze_N_inval_SD[7]<-62
Total_N_val[7]<-NA; Total_N_inval[7]<-NA
Total_N_val_SD[7]<-NA; Total_N_inval_SD[7]<-NA
SFD_N_val[7]<-234; SFD_N_inval[7]<-231
SFD_N_val_SD[7]<-38; SFD_N_inval_SD[7]<-37
FixProb_N_val[7]<-NA; FixProb_N_inval[7]<-NA
FixProb_N_val_SD[7]<-NA; FixProb_N_inval_SD[7]<-NA
LandPos_N_val[7]<- NA; LandPos_N_inval[7]<- NA
LandPos_N_val_SD[7]<- NA; LandPos_N_inval_SD[7]<- NA
# N+1
FFD_N1_val[7]<-252; FFD_N1_inval[7]<-254 # Table 3
FFD_N1_val_SD[7]<-41; FFD_N1_inval_SD[7]<-45
Gaze_N1_val[7]<-290 ; Gaze_N1_inval[7]<-298
Gaze_N1_val_SD[7]<-60 ; Gaze_N1_inval_SD[7]<- 62
Total_N1_val[7]<-NA; Total_N1_inval[7]<-NA
Total_N1_val_SD[7]<-NA; Total_N1_inval_SD[7]<- NA
SFD_N1_val[7]<-259; SFD_N1_inval[7]<-249
SFD_N1_val_SD[7]<-46; SFD_N1_inval_SD[7]<-46
FixProb_N1_val[7]<-NA; FixProb_N1_inval[7]<- NA
FixProb_N1_val_SD[7]<-NA; FixProb_N1_inval_SD[7]<-NA
LandPos_N1_val[7]<-NA ; LandPos_N1_inval[7]<-NA
LandPos_N1_val_SD[7]<-NA ; LandPos_N1_inval_SD[7]<-NA
#N+2  
FFD_N2_val[7]<-239; FFD_N2_inval[7]<-243 # Table 4
FFD_N2_val_SD[7]<-36; FFD_N2_inval_SD[7]<-41
Gaze_N2_val[7]<-284; Gaze_N2_inval[7]<-285
Gaze_N2_val_SD[7]<-54; Gaze_N2_inval_SD[7]<-54
Total_N2_val[7]<-NA; Total_N2_inval[7]<-NA
Total_N2_val_SD[7]<-NA; Total_N2_inval_SD[7]<-NA
SFD_N2_val[7]<-245; SFD_N2_inval[7]<-249
SFD_N2_val_SD[7]<-39; SFD_N2_inval_SD[7]<-46
FixProb_N2_val[7]<- NA; FixProb_N2_inval[7]<-NA
FixProb_N2_val_SD[7]<- NA; FixProb_N2_inval_SD[7]<-NA
LandPos_N2_val[7]<-NA ; LandPos_N2_inval[7]<-NA
LandPos_N2_val_SD[7]<-NA ; LandPos_N2_inval_SD[7]<-NA


#####
#8
#####
# I take the grand mean for low an high frequency words
ID[8]<-8
Paper[8]<- "Yan, Kliegl, Shu, Pan, & Zhou (2010)"
Effect[8]<-"PB"
Boundary[8]<- 'N+2'
Language[8]<- 'Chinese'
N[8]<-74-6
#N
FFD_N_val[8]<-NA; FFD_N_inval[8]<-NA  # Table 1
FFD_N_val_SD[8]<-NA; FFD_N_inval_SD[8]<-NA
Gaze_N_val[8]<-NA; Gaze_N_inval[8]<-NA
Gaze_N_val_SD[8]<-NA; Gaze_N_inval_SD[8]<-NA
Total_N_val[8]<-NA; Total_N_inval[8]<-NA
Total_N_val_SD[8]<-NA; Total_N_inval_SD[8]<-NA
SFD_N_val[8]<-NA; SFD_N_inval[8]<-NA
SFD_N_val_SD[8]<-NA; SFD_N_inval_SD[8]<-NA
FixProb_N_val[8]<- NA; FixProb_N_inval[8]<- NA
FixProb_N_val_SD[8]<-NA; FixProb_N_inval_SD[8]<-NA
LandPos_N_val[8]<- NA; LandPos_N_inval[8]<- NA
LandPos_N_val_SD[8]<- NA; LandPos_N_inval_SD[8]<- NA
# N+1
FFD_N1_val[8]<-(246+290)/2; FFD_N1_inval[8]<-(260+301)/2 # Table 1 
FFD_N1_val_SD[8]<-(48+62)/2; FFD_N1_inval_SD[8]<-(83+63)/2
Gaze_N1_val[8]<-(249+293)/2 ; Gaze_N1_inval[8]<-(264+307)/2
Gaze_N1_val_SD[8]<-(53+63)/2 ; Gaze_N1_inval_SD[8]<- (86+63)/2
Total_N1_val[8]<-NA; Total_N1_inval[8]<-NA
Total_N1_val_SD[8]<-NA; Total_N1_inval_SD[8]<- NA
SFD_N1_val[8]<-NA; SFD_N1_inval[8]<-NA
SFD_N1_val_SD[8]<-NA; SFD_N1_inval_SD[8]<-NA
FixProb_N1_val[8]<-NA; FixProb_N1_inval[8]<- NA
FixProb_N1_val_SD[8]<-NA; FixProb_N1_inval_SD[8]<- NA
LandPos_N1_val[8]<-NA ; LandPos_N1_inval[8]<-NA
LandPos_N1_val_SD[8]<-NA ; LandPos_N1_inval_SD[8]<-NA
#N+2  
FFD_N2_val[8]<-(269+280)/2; FFD_N2_inval[8]<-(282+283)/2 # Table 1
FFD_N2_val_SD[8]<-(49+46)/2; FFD_N2_inval_SD[8]<-(43+50)/2
Gaze_N2_val[8]<-(306+328)/2; Gaze_N2_inval[8]<-(326+337)/2
Gaze_N2_val_SD[8]<-(63+77)/2; Gaze_N2_inval_SD[8]<-(60+75)/2
Total_N2_val[8]<-NA; Total_N2_inval[8]<-NA
Total_N2_val_SD[8]<-NA; Total_N2_inval_SD[8]<-NA
SFD_N2_val[8]<-NA; SFD_N2_inval[8]<-NA
SFD_N2_val_SD[8]<-NA; SFD_N2_inval_SD[8]<-NA
FixProb_N2_val[8]<- NA; FixProb_N2_inval[8]<-NA
FixProb_N2_val_SD[8]<- NA; FixProb_N2_inval_SD[8]<-NA
LandPos_N2_val[8]<-NA ; LandPos_N2_inval[8]<-NA
LandPos_N2_val_SD[8]<-NA ; LandPos_N2_inval_SD[8]<-NA


#####
#9
#####
# I take the grand mean of high and low predictable words
# I also use the letter mask cond. since it's similar to earlier studies
ID[9]<-9
Paper[9]<- "Radach, Inhoff, Glover, & Vorstius (2013)"
Effect[9]<-"PB"
Boundary[9]<- 'N+2'
Language[9]<- 'English'
N[9]<-36
#N
FFD_N_val[9]<-(222+222)/2; FFD_N_inval[9]<-(223+225)/2  # Table 3
FFD_N_val_SD[9]<-(72+72)/2; FFD_N_inval_SD[9]<-(77+77)/2
Gaze_N_val[9]<-(255+254)/2; Gaze_N_inval[9]<-(252+251)/2
Gaze_N_val_SD[9]<-(103+103)/2; Gaze_N_inval_SD[9]<-(104+104)/2
Total_N_val[9]<-NA; Total_N_inval[9]<-NA
Total_N_val_SD[9]<-NA; Total_N_inval_SD[9]<-NA
SFD_N_val[9]<-(224+226)/2; SFD_N_inval[9]<-(227+229)/2
SFD_N_val_SD[9]<-(73+73)/2; SFD_N_inval_SD[9]<-(75+75)/2
FixProb_N_val[9]<-NA; FixProb_N_inval[9]<-NA
FixProb_N_val_SD[9]<-NA; FixProb_N_inval_SD[9]<-NA
LandPos_N_val[9]<-NA; LandPos_N_inval[9]<- NA
LandPos_N_val_SD[9]<-NA; LandPos_N_inval_SD[9]<- NA
# N+1
FFD_N1_val[9]<-(204+190)/2; FFD_N1_inval[9]<-(192+201)/2 # Table 4
FFD_N1_val_SD[9]<-(83+64)/2; FFD_N1_inval_SD[9]<-(59+68)/2
Gaze_N1_val[9]<-(210+194)/2 ; Gaze_N1_inval[9]<-(204+212)/2
Gaze_N1_val_SD[9]<-(86+67)/2 ; Gaze_N1_inval_SD[9]<-(78+81)/2
Total_N1_val[9]<-NA; Total_N1_inval[9]<-NA
Total_N1_val_SD[9]<-NA; Total_N1_inval_SD[9]<- NA
SFD_N1_val[9]<-(210+194)/2; SFD_N1_inval[9]<-(204+212)/2
SFD_N1_val_SD[9]<-(86+67)/2; SFD_N1_inval_SD[9]<-(78+81)/2
FixProb_N1_val[9]<-NA; FixProb_N1_inval[9]<- NA
FixProb_N1_val_SD[9]<-NA; FixProb_N1_inval_SD[9]<-NA
LandPos_N1_val[9]<-NA ; LandPos_N1_inval[9]<-NA
LandPos_N1_val_SD[9]<-NA ; LandPos_N1_inval_SD[9]<-NA
#N+2  
FFD_N2_val[9]<- (231+213)/2; FFD_N2_inval[9]<-(238+225)/2 # Table 6
FFD_N2_val_SD[9]<-(78+66)/2; FFD_N2_inval_SD[9]<-(88+74)/2
Gaze_N2_val[9]<-(258+229)/2; Gaze_N2_inval[9]<-(272+248)/2
Gaze_N2_val_SD[9]<-(97+77)/2; Gaze_N2_inval_SD[9]<-(120+95)/2
Total_N2_val[9]<-NA; Total_N2_inval[9]<-NA
Total_N2_val_SD[9]<-NA; Total_N2_inval_SD[9]<-NA
SFD_N2_val[9]<-(256+252)/2; SFD_N2_inval[9]<-(249+251)/2
SFD_N2_val_SD[9]<-(105+102)/2; SFD_N2_inval_SD[9]<-(102+93)/2
FixProb_N2_val[9]<- NA; FixProb_N2_inval[9]<-NA
FixProb_N2_val_SD[9]<- NA; FixProb_N2_inval_SD[9]<-NA
LandPos_N2_val[9]<-NA ; LandPos_N2_inval[9]<-NA
LandPos_N2_val_SD[9]<-NA ; LandPos_N2_inval_SD[9]<-NA


#####
#10
#####

ID[10]<-10
Paper[10]<- "Risse & Kliegl (2011)"
Effect[10]<-"PB"
Boundary[10]<- 'N+2'
Language[10]<- 'German'
N[10]<-40  # I take the 'young' sample since it's more similar to previous studies
# Note, however, that the 'old' one could be included as another study (separate sample) if age
# is deemed to not be an important confounding factor here

#N  

# separate statistics for difficulty of word n+1 (content vs function word) and whether
# N+1 was fixated. I take the grand mean of them, since I'm interested in the general preview effect  
FFD_N_val[10]<- ((216+212)/2 + (206+197)/2)/2; FFD_N_inval[10]<-  ((204+196)/2 +(210+214)/2)/2
FFD_N_val_SD[10]<-((53+59)/2+ (65+77)/2)/2; FFD_N_inval_SD[10]<- ((61+59)/2 +(64+73)/2)/2
Gaze_N_val[10]<-((217+211)/2+ (253+250)/2)/2; Gaze_N_inval[10]<-((217+214)/2+ (248+251)/2)/2
Gaze_N_val_SD[10]<-((72+76)/2+ (98+109)/2)/2; Gaze_N_inval_SD[10]<-((79+78)/2+ (99+112)/2)/2
Total_N_val[10]<-NA; Total_N_inval[10]<-NA              
Total_N_val_SD[10]<-NA; Total_N_inval_SD[10]<-NA            # Table 1
SFD_N_val[10]<-NA; SFD_N_inval[10]<-NA
SFD_N_val_SD[10]<-NA; SFD_N_inval_SD[10]<-NA
FixProb_N_val[10]<-NA; FixProb_N_inval[10]<-NA
FixProb_N_val_SD[10]<-NA; FixProb_N_inval_SD[10]<-NA
LandPos_N_val[10]<- NA; LandPos_N_inval[10]<- NA
LandPos_N_val_SD[10]<- NA; LandPos_N_inval_SD[10]<- NA
# N+1
FFD_N1_val[10]<- (211+209)/2; FFD_N1_inval[10]<-(217+215)/2        # Table 1 
FFD_N1_val_SD[10]<-(72+65)/2; FFD_N1_inval_SD[10]<-(79+63)/2
Gaze_N1_val[10]<-(211+209)/2; Gaze_N1_inval[10]<-(218+217)/2
Gaze_N1_val_SD[10]<-(73+65)/2; Gaze_N1_inval_SD[10]<- (79+65)/2
Total_N1_val[10]<-NA; Total_N1_inval[10]<-NA
Total_N1_val_SD[10]<-NA; Total_N1_inval_SD[10]<- NA
SFD_N1_val[10]<-NA; SFD_N1_inval[10]<-NA
SFD_N1_val_SD[10]<-NA; SFD_N1_inval_SD[10]<-NA
FixProb_N1_val[10]<-NA; FixProb_N1_inval[10]<- NA
FixProb_N1_val_SD[10]<-NA; FixProb_N1_inval_SD[10]<-NA
LandPos_N1_val[10]<-NA ; LandPos_N1_inval[10]<-NA
LandPos_N1_val_SD[10]<-NA ; LandPos_N1_inval_SD[10]<-NA
#N+2  
FFD_N2_val[10]<-((198+220)/2+ (216+206)/2)/2; FFD_N2_inval[10]<-((190+226)/2+ (216+221)/2)/2
FFD_N2_val_SD[10]<-((74+59)/2+ (84+65)/2)/2; FFD_N2_inval_SD[10]<-((62+65)/2+ (81+69)/2)/2
Gaze_N2_val[10]<-((210+248)/2+ (226+242)/2)/2; Gaze_N2_inval[10]<-((195+266)/2+ (223+266)/2)/2
Gaze_N2_val_SD[10]<-((87+84)/2+ (98+95)/2)/2; Gaze_N2_inval_SD[10]<- ((69+86)/2+ (89+107)/2)/2
Total_N2_val[10]<-NA; Total_N2_inval[10]<-NA    
Total_N2_val_SD[10]<-NA; Total_N2_inval_SD[10]<-NA     # Table 1
SFD_N2_val[10]<-NA; SFD_N2_inval[10]<-NA
SFD_N2_val_SD[10]<-NA; SFD_N2_inval_SD[10]<-NA
FixProb_N2_val[10]<- NA; FixProb_N2_inval[10]<-NA
FixProb_N2_val_SD[10]<- NA; FixProb_N2_inval_SD[10]<-NA
LandPos_N2_val[10]<-NA ; LandPos_N2_inval[10]<-NA
LandPos_N2_val_SD[10]<-NA ; LandPos_N2_inval_SD[10]<-NA


#####
#11
#####

ID[11]<-11
Paper[11]<- " Angele & Rayner (2011) Exp. 2"
Effect[11]<-"PB"
Boundary[11]<- 'n+1/n+2'
Language[11]<- 'English'
N[11]<-32
# Manipulation of frequency of word n; as usual, I take the grand mean.

#N
FFD_N_val[11]<-(234+245)/2; FFD_N_inval[11]<-(231+246)/2  # Table 6
FFD_N_val_SD[11]<-(81+90)/2; FFD_N_inval_SD[11]<-(86+91)/2
Gaze_N_val[11]<-(256+266)/2; Gaze_N_inval[11]<-(258+289)/2
Gaze_N_val_SD[11]<-(105+108)/2; Gaze_N_inval_SD[11]<-(111+120)/2
Total_N_val[11]<-NA; Total_N_inval[11]<-NA
Total_N_val_SD[11]<-NA; Total_N_inval_SD[11]<-NA
SFD_N_val[11]<-NA; SFD_N_inval[11]<-NA
SFD_N_val_SD[11]<-NA; SFD_N_inval_SD[11]<-NA
FixProb_N_val[11]<-NA; FixProb_N_inval[11]<-NA
FixProb_N_val_SD[11]<-NA; FixProb_N_inval_SD[11]<-NA
LandPos_N_val[11]<- NA; LandPos_N_inval[11]<- NA
LandPos_N_val_SD[11]<- NA; LandPos_N_inval_SD[11]<- NA
# N+1
FFD_N1_val[11]<-(204+211)/2; FFD_N1_inval[11]<-(208+216)/2 # Table 6
FFD_N1_val_SD[11]<-(59+65)/2; FFD_N1_inval_SD[11]<-(51+82)/2
Gaze_N1_val[11]<-(207+218)/2 ; Gaze_N1_inval[11]<-(216+224)/2
Gaze_N1_val_SD[11]<-(65+73)/2 ; Gaze_N1_inval_SD[11]<- (68+88)/2
Total_N1_val[11]<-NA; Total_N1_inval[11]<-NA
Total_N1_val_SD[11]<-NA; Total_N1_inval_SD[11]<- NA
SFD_N1_val[11]<-NA; SFD_N1_inval[11]<-NA
SFD_N1_val_SD[11]<-NA; SFD_N1_inval_SD[11]<-NA
FixProb_N1_val[11]<-NA; FixProb_N1_inval[11]<- NA
FixProb_N1_val_SD[11]<-NA; FixProb_N1_inval_SD[11]<-NA
LandPos_N1_val[11]<-NA ; LandPos_N1_inval[11]<-NA
LandPos_N1_val_SD[11]<-NA ; LandPos_N1_inval_SD[11]<-NA
#N+2  
FFD_N2_val[11]<-(233+250)/2; FFD_N2_inval[11]<-(238+251)/2      # Table 6
FFD_N2_val_SD[11]<-(77+94)/2; FFD_N2_inval_SD[11]<-(82+85)/2
Gaze_N2_val[11]<-(261+277)/2; Gaze_N2_inval[11]<-(275+288)/2
Gaze_N2_val_SD[11]<-(102+114)/2; Gaze_N2_inval_SD[11]<-(116+114)/2
Total_N2_val[11]<-NA; Total_N2_inval[11]<-NA
Total_N2_val_SD[11]<-NA; Total_N2_inval_SD[11]<-NA
SFD_N2_val[11]<-NA; SFD_N2_inval[11]<-NA
SFD_N2_val_SD[11]<-NA; SFD_N2_inval_SD[11]<-NA
FixProb_N2_val[11]<- NA; FixProb_N2_inval[11]<-NA
FixProb_N2_val_SD[11]<- NA; FixProb_N2_inval_SD[11]<-NA
LandPos_N2_val[11]<-NA ; LandPos_N2_inval[11]<-NA
LandPos_N2_val_SD[11]<-NA ; LandPos_N2_inval_SD[11]<-NA


#####
#12
#####

#ID[12]<-12
#Paper[12]<- ""
#Effect[12]<-""
#Boundary[12]<- ''
#Language[12]<- ''
#N[12]<-NA
#N
#FFD_N_val[12]<-NA; FFD_N_inval[12]<-NA  # Table 
#FFD_N_val_SD[12]<-NA; FFD_N_inval_SD[12]<-NA
#Gaze_N_val[12]<-NA; Gaze_N_inval[12]<-NA
#Gaze_N_val_SD[12]<-NA; Gaze_N_inval_SD[12]<-NA
#Total_N_val[12]<-NA; Total_N_inval[12]<-NA
#Total_N_val_SD[12]<-NA; Total_N_inval_SD[12]<-NA
#SFD_N_val[12]<-NA; SFD_N_inval[12]<-NA
#SFD_N_val_SD[12]<-NA; SFD_N_inval_SD[12]<-NA
#FixProb_N_val[12]<-NA; FixProb_N_inval[12]<-NA
#FixProb_N_val_SD[12]<-NA; FixProb_N_inval_SD[12]<-NA
#LandPos_N_val[12]<- NA; LandPos_N_inval[12]<- NA
#LandPos_N_val_SD[12]<- NA; LandPos_N_inval_SD[12]<- NA
# N+1
#FFD_N1_val[12]<-NA; FFD_N1_inval[12]<-NA # Table 
#FFD_N1_val_SD[12]<-NA; FFD_N1_inval_SD[12]<-NA
#Gaze_N1_val[12]<-NA ; Gaze_N1_inval[12]<-NA
#Gaze_N1_val_SD[12]<-NA ; Gaze_N1_inval_SD[12]<- NA
#Total_N1_val[12]<-NA; Total_N1_inval[12]<-NA
#Total_N1_val_SD[12]<-NA; Total_N1_inval_SD[12]<- NA
#SFD_N1_val[12]<-NA; SFD_N1_inval[12]<-NA
#SFD_N1_val_SD[12]<-NA; SFD_N1_inval_SD[12]<-NA
#FixProb_N1_val[12]<-NA; FixProb_N1_inval[12]<- NA
#FixProb_N1_val_SD[12]<-NA; FixProb_N1_inval_SD[12]<-NA
#LandPos_N1_val[12]<-NA ; LandPos_N1_inval[12]<-NA
#LandPos_N1_val_SD[12]<-NA ; LandPos_N1_inval_SD[12]<-NA
#N+2  
#FFD_N2_val[12]<-NA; FFD_N2_inval[12]<-NA
#FFD_N2_val_SD[12]<-NA; FFD_N2_inval_SD[12]<-NA
#Gaze_N2_val[12]<-NA; Gaze_N2_inval[12]<-NA
#Gaze_N2_val_SD[12]<-NA; Gaze_N2_inval_SD[12]<-NA
#Total_N2_val[12]<-NA; Total_N2_inval[12]<-NA
#Total_N2_val_SD[12]<-NA; Total_N2_inval_SD[12]<-NA
#SFD_N2_val[12]<-NA; SFD_N2_inval[12]<-NA
#SFD_N2_val_SD[12]<-NA; SFD_N2_inval_SD[12]<-NA
#FixProb_N2_val[12]<- NA; FixProb_N2_inval[12]<-NA
#FixProb_N2_val_SD[12]<- NA; FixProb_N2_inval_SD[12]<-NA
#LandPos_N2_val[12]<-NA ; LandPos_N2_inval[12]<-NA
#LandPos_N2_val_SD[12]<-NA ; LandPos_N2_inval_SD[12]<-NA



#####
#13
#####

#ID[13]<-13
#Paper[13]<- ""
#Effect[13]<-""
#Boundary[13]<- ''
#Language[13]<- ''
#N[13]<-NA
#N
#FFD_N_val[13]<-NA; FFD_N_inval[13]<-NA  # Table 
#FFD_N_val_SD[13]<-NA; FFD_N_inval_SD[13]<-NA
#Gaze_N_val[13]<-NA; Gaze_N_inval[13]<-NA
#Gaze_N_val_SD[13]<-NA; Gaze_N_inval_SD[13]<-NA
#Total_N_val[13]<-NA; Total_N_inval[13]<-NA
#Total_N_val_SD[13]<-NA; Total_N_inval_SD[13]<-NA
#SFD_N_val[13]<-NA; SFD_N_inval[13]<-NA
#SFD_N_val_SD[13]<-NA; SFD_N_inval_SD[13]<-NA
#FixProb_N_val[13]<-NA; FixProb_N_inval[13]<-NA
#FixProb_N_val_SD[13]<-NA; FixProb_N_inval_SD[13]<-NA
#LandPos_N_val[13]<- NA; LandPos_N_inval[13]<- NA
#LandPos_N_val_SD[13]<- NA; LandPos_N_inval_SD[13]<- NA
# N+1
#FFD_N1_val[13]<-NA; FFD_N1_inval[13]<-NA # Table 
#FFD_N1_val_SD[13]<-NA; FFD_N1_inval_SD[13]<-NA
#Gaze_N1_val[13]<-NA ; Gaze_N1_inval[13]<-NA
#Gaze_N1_val_SD[13]<-NA ; Gaze_N1_inval_SD[13]<- NA
#Total_N1_val[13]<-NA; Total_N1_inval[13]<-NA
#Total_N1_val_SD[13]<-NA; Total_N1_inval_SD[13]<- NA
#SFD_N1_val[13]<-NA; SFD_N1_inval[13]<-NA
#SFD_N1_val_SD[13]<-NA; SFD_N1_inval_SD[13]<-NA
#FixProb_N1_val[13]<-NA; FixProb_N1_inval[13]<- NA
#FixProb_N1_val_SD[13]<-NA; FixProb_N1_inval_SD[13]<-NA
#LandPos_N1_val[13]<-NA ; LandPos_N1_inval[13]<-NA
#LandPos_N1_val_SD[13]<-NA ; LandPos_N1_inval_SD[13]<-NA
#N+2  
#FFD_N2_val[13]<-NA; FFD_N2_inval[13]<-NA
#FFD_N2_val_SD[13]<-NA; FFD_N2_inval_SD[13]<-NA
#Gaze_N2_val[13]<-NA; Gaze_N2_inval[13]<-NA
#Gaze_N2_val_SD[13]<-NA; Gaze_N2_inval_SD[13]<-NA
#Total_N2_val[13]<-NA; Total_N2_inval[13]<-NA
#Total_N2_val_SD[13]<-NA; Total_N2_inval_SD[13]<-NA
#SFD_N2_val[13]<-NA; SFD_N2_inval[13]<-NA
#SFD_N2_val_SD[13]<-NA; SFD_N2_inval_SD[13]<-NA
#FixProb_N2_val[13]<- NA; FixProb_N2_inval[13]<-NA
#FixProb_N2_val_SD[13]<- NA; FixProb_N2_inval_SD[13]<-NA
#LandPos_N2_val[13]<-NA ; LandPos_N2_inval[13]<-NA
#LandPos_N2_val_SD[13]<-NA ; LandPos_N2_inval_SD[13]<-NA


#############################################################################################
# Merge variables into a data matrix:
dataN2<- data.frame(ID, Paper, Effect, Boundary,Language,N,FFD_N_val, FFD_N_val_SD, FFD_N_inval,
       FFD_N_inval_SD, Gaze_N_val, Gaze_N_val_SD,Gaze_N_inval, Gaze_N_inval_SD, Total_N_val,
       Total_N_val_SD, Total_N_inval,Total_N_inval_SD, SFD_N_val, SFD_N_inval, SFD_N_val_SD,
       SFD_N_inval_SD, FixProb_N_val, FixProb_N_val_SD, FixProb_N_inval,FixProb_N_inval_SD,
       LandPos_N_val, LandPos_N_val_SD, LandPos_N_inval, LandPos_N_inval_SD, FFD_N1_val,
       FFD_N1_val_SD, FFD_N1_inval, FFD_N1_inval_SD, Gaze_N1_val, Gaze_N1_val_SD,Gaze_N1_inval,
       Gaze_N1_inval_SD, Total_N1_val, Total_N1_val_SD, Total_N1_inval,Total_N1_inval_SD,
       SFD_N1_val, SFD_N1_inval, SFD_N1_val_SD, SFD_N1_inval_SD, FixProb_N1_val, FixProb_N1_val_SD,
       FixProb_N1_inval,FixProb_N1_inval_SD, LandPos_N1_val, LandPos_N1_val_SD,
       LandPos_N1_inval, LandPos_N1_inval_SD, FFD_N2_val, FFD_N2_val_SD, FFD_N2_inval,
       FFD_N2_inval_SD, Gaze_N2_val, Gaze_N2_val_SD, Gaze_N2_inval, Gaze_N2_inval_SD,
       Total_N2_val, Total_N2_val_SD, Total_N2_inval, Total_N2_inval_SD, SFD_N2_val, SFD_N2_inval,
       SFD_N2_val_SD, SFD_N2_inval_SD, FixProb_N2_val, FixProb_N2_val_SD, FixProb_N2_inval,
       FixProb_N2_inval_SD, LandPos_N2_val, LandPos_N2_val_SD, LandPos_N2_inval, LandPos_N2_inval_SD)

#Save in a data file:
save(dataN2,file="Data/dataN2.Rda")

rm(Effect, Boundary, FFD_N_val, FFD_N_val_SD, FFD_N_inval,
   FFD_N_inval_SD, Gaze_N_val, Gaze_N_val_SD,Gaze_N_inval, Gaze_N_inval_SD, Total_N_val,
   Total_N_val_SD, Total_N_inval,Total_N_inval_SD, SFD_N_val, SFD_N_inval, SFD_N_val_SD,
   SFD_N_inval_SD, FixProb_N_val, FixProb_N_val_SD, FixProb_N_inval,FixProb_N_inval_SD,
   LandPos_N_val, LandPos_N_val_SD, LandPos_N_inval, LandPos_N_inval_SD, FFD_N1_val,
   FFD_N1_val_SD, FFD_N1_inval, FFD_N1_inval_SD, Gaze_N1_val, Gaze_N1_val_SD,Gaze_N1_inval,
   Gaze_N1_inval_SD, Total_N1_val, Total_N1_val_SD, Total_N1_inval,Total_N1_inval_SD,
   SFD_N1_val, SFD_N1_inval, SFD_N1_val_SD, SFD_N1_inval_SD, FixProb_N1_val, FixProb_N1_val_SD,
   FixProb_N1_inval,FixProb_N1_inval_SD, LandPos_N1_val, LandPos_N1_val_SD,
   LandPos_N1_inval, LandPos_N1_inval_SD, FFD_N2_val, FFD_N2_val_SD, FFD_N2_inval,
   FFD_N2_inval_SD, Gaze_N2_val, Gaze_N2_val_SD, Gaze_N2_inval, Gaze_N2_inval_SD,
   Total_N2_val, Total_N2_val_SD, Total_N2_inval, Total_N2_inval_SD, SFD_N2_val, SFD_N2_inval,
   SFD_N2_val_SD, SFD_N2_inval_SD, FixProb_N2_val, FixProb_N2_val_SD, FixProb_N2_inval,
   FixProb_N2_inval_SD, LandPos_N2_val, LandPos_N2_val_SD, LandPos_N2_inval, LandPos_N2_inval_SD)


######################################
# Effect sizes (N+2 preview benefit) #
######################################
FFD_ms<-NULL
FFD_es<- NULL
Gaze_ms<- NULL
Gaze_es<- NULL
SFD_ms<-NULL
SFD_es<-NULL
Fix_prob<- NULL
Fix_prob_es<-NULL
Total_ms<-NULL
Total_es<-NULL

#ms
for(i in 1:length(dataN2$ID)){
FFD_ms[i]<- (dataN2$FFD_N2_inval[i]- dataN2$FFD_N2_val[i]) 
Gaze_ms[i]<- (dataN2$Gaze_N2_inval[i]- dataN2$Gaze_N2_val[i])
Fix_prob[i]<- (dataN2$FixProb_N2_inval[i]- dataN2$FixProb_N2_val[i])
SFD_ms[i]<- (dataN2$SFD_N2_inval[i]- dataN2$SFD_N2_val[i])
Total_ms[i]<- (dataN2$Total_N2_inval[i]- dataN2$Total_N2_val[i])
}

#ES (Cohen's d)
for(i in 1:length(dataN2$ID)){
  FFD_es[i]<- (dataN2$FFD_N2_inval[i]- dataN2$FFD_N2_val[i])/ sqrt((dataN2$FFD_N2_inval_SD[i]^2+dataN2$FFD_N2_val_SD[i]^2)/2)
  Gaze_es[i]<- (dataN2$Gaze_N2_inval[i]- dataN2$Gaze_N2_val[i])/sqrt((dataN2$Gaze_N2_inval_SD[i]^2+dataN2$Gaze_N2_val_SD[i]^2)/2)
  Fix_prob_es[i]<- (dataN2$FixProb_N2_inval[i]- dataN2$FixProb_N2_val[i])/sqrt((dataN2$FixProb_N2_inval_SD[i]^2+dataN2$FixProb_N2_val_SD[i]^2)/2)
  SFD_es[i]<- (dataN2$SFD_N2_inval[i]- dataN2$SFD_N2_val[i])/ sqrt((dataN2$SFD_N2_inval_SD[i]^2+dataN2$SFD_N2_val_SD[i]^2)/2)
  Total_es[i]<- (dataN2$Total_N2_inval[i]- dataN2$Total_N2_val[i])/ sqrt((dataN2$Total_N2_inval_SD[i]^2+dataN2$Total_N2_val_SD[i]^2)/2)
}
ES_N2 <- data.frame(FFD_ms, FFD_es, Gaze_ms, Gaze_es, SFD_ms, SFD_es, Total_ms, Total_es, Fix_prob, Fix_prob_es)


######################
# Effect sizes (N+1) #
######################
FFD_ms_N1<-NULL
FFD_es_N1<- NULL
Gaze_ms_N1<- NULL
Gaze_es_N1<- NULL
SFD_ms_N1<-NULL
SFD_es_N1<-NULL
Fix_prob_N1<- NULL
Fix_prob_es_N1<-NULL
Total_ms_N1<-NULL
Total_es_N1<-NULL

#ms
for(i in 1:length(dataN2$ID)){
  FFD_ms_N1[i]<- (dataN2$FFD_N1_inval[i]- dataN2$FFD_N1_val[i]) 
  Gaze_ms_N1[i]<- (dataN2$Gaze_N1_inval[i]- dataN2$Gaze_N1_val[i])
  Fix_prob_N1[i]<- (dataN2$FixProb_N1_inval[i]- dataN2$FixProb_N1_val[i])
  SFD_ms_N1[i]<- (dataN2$SFD_N1_inval[i]- dataN2$SFD_N1_val[i])
  Total_ms_N1[i]<- (dataN2$Total_N1_inval[i]- dataN2$Total_N1_val[i])
}

#ES (Cohen's d)
for(i in 1:length(dataN2$ID)){
  FFD_es_N1[i]<- (dataN2$FFD_N1_inval[i]- dataN2$FFD_N1_val[i])/ sqrt((dataN2$FFD_N1_inval_SD[i]^2+dataN2$FFD_N1_val_SD[i]^2)/2)
  Gaze_es_N1[i]<- (dataN2$Gaze_N1_inval[i]- dataN2$Gaze_N1_val[i])/sqrt((dataN2$Gaze_N1_inval_SD[i]^2+dataN2$Gaze_N1_val_SD[i]^2)/2)
  Fix_prob_es_N1[i]<- (dataN2$FixProb_N1_inval[i]- dataN2$FixProb_N1_val[i])/sqrt((dataN2$FixProb_N1_inval_SD[i]^2+dataN2$FixProb_N1_val_SD[i]^2)/2)
  SFD_es_N1[i]<- (dataN2$SFD_N1_inval[i]- dataN2$SFD_N1_val[i])/ sqrt((dataN2$SFD_N1_inval_SD[i]^2+dataN2$SFD_N1_val_SD[i]^2)/2)
  Total_es_N1[i]<- (dataN2$Total_N1_inval[i]- dataN2$Total_N1_val[i])/ sqrt((dataN2$Total_N1_inval_SD[i]^2+dataN2$Total_N1_val_SD[i]^2)/2)
}
ES_N2 <- data.frame(ID, Paper, Language, N, FFD_ms, FFD_es, Gaze_ms, Gaze_es, SFD_ms, SFD_es, Total_ms, Total_es, Fix_prob, Fix_prob_es,
                    FFD_ms_N1, FFD_es_N1, Gaze_ms_N1, Gaze_es_N1, SFD_ms_N1, SFD_es_N1, Total_ms_N1, Total_es_N1,
                    Fix_prob_N1, Fix_prob_es_N1)

#Save in a data file:
save(ES_N2,file="Data/ES_N2.Rda")

rm(FFD_ms, FFD_es, Gaze_ms, Gaze_es, SFD_ms, SFD_es, Total_ms, Total_es, Fix_prob, Fix_prob_es,
   FFD_ms_N1, FFD_es_N1, Gaze_ms_N1, Gaze_es_N1, SFD_ms_N1, SFD_es_N1, Total_ms_N1, Total_es_N1,
   Fix_prob_N1, Fix_prob_es_N1)

#############################################################################################
# References:
# 1.Rayner, K., Juhasz, B. J., & Brown, S. J. (2007). Do readers obtain preview benefit 
#       from word n+ 2? A test of serial attention shift versus distributed lexical 
#       processing models of eye movement control in reading. Journal of Experimental 
#       Psychology:Human Perception and Performance, 33(1), 230-345.
#
# 2.Same as above(Exp.2)
#
# 3.Kliegl, R., Risse, S., & Laubrock, J. (2007). Preview benefit and parafoveal-on-foveal
#       effects from word n+ 2. Journal of Experimental Psychology: Human Perception 
#       and Performance, 33(5), 1250-1255.
#
# 4.Yang, J., Wang, S., Xu, Y., & Rayner, K. (2009). Do chinese readers obtain preview 
#       benefit from word n+ 2? Evidence from eye movements. Journal of Experimental 
#       Psychology: Human Perception and Performance, 35(4), 1192-1204.
#
# 5. same as above (Exp.2)    
#
# 6. Yang, J., Rayner, K., Li, N., & Wang, S. (2012). Is preview benefit from word n+ 2
#       a common effect in reading Chinese? Evidence from eye movements. Reading and writing,
#       25(5), 1079-1091
#
# 7. Angele, B., Slattery, T. J., Yang, J., Kliegl, R., & Rayner, K. (2008). Parafoveal 
#       processing in reading: Manipulating n+ 1 and n+ 2 previews simultaneously. Visual 
#       cognition, 16(6), 697-707.
#
# 8. Yan, M., Kliegl, R., Shu, H., Pan, J., & Zhou, X. (2010). Parafoveal load of word N+ 1
#       modulates preprocessing effectiveness of word N+ 2 in Chinese reading. Journal of 
#       Experimental Psychology: Human Perception and Performance, 36(6), 1669.
#   
# 9. Radach, R., Inhoff, A. W., Glover, L., & Vorstius, C. (2013). Contextual constraint and
#       N+ 2 preview effects in reading. The Quarterly Journal of Experimental Psychology, 66(3),
#       619-633.
#
# 10. Risse, S., & Kliegl, R. (2011). Adult age differences in the perceptual span during reading.
#       Psychology and Aging, 26(2), 451-460. doi:10.1037/a0021616
#
# 11. Angele, B., & Rayner, K. (2011). Parafoveal processing of word n + 2 during reading: 
#       Do the preceding words matter? Journal of Experimental Psychology: Human Perception and Performance,
#       37(4), 1210-1220. doi:10.1037/a0023096
#
# Wang, C. A., Inhoff, A. W., & Radach, R. (2009). Is attention confined to one word at a time?
#     The spatial distribution of parafoveal preview benefits during reading. Attention, Perception, 
#     & Psychophysics, 71(7), 1487-1494.
#
# Risse, S., & Kliegl, R. (2012). Evidence for delayed parafoveal-on-foveal effects
#       from word n+ 2 in reading. Journal of Experimental Psychology: Human Perception
#       and Performance, 38(4), 1026-1042.
#
# Same as above (Exp.2)
# http://www.fcrr.org/vip_lab/articles/Wang_Inhoff_Radach_APP_2009.pdf
