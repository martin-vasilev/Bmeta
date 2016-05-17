#----------------------------------------------------------------------
#                            DATA PREPARATION
#----------------------------------------------------------------------

# Script used for coding descriptive statistics of individual 
# n+1 studies and their effect sizes

# Martin R. Vasilev, 2015

##############
# PLEASE NOTE: the file is very big and it may take R a long time to execute the script
##############

rm(list=ls())


# Define Variables:
ID<- NULL # numbers refer to the reference list at the bottom
Paper<- NULL
Language<- NULL
N <- NULL     # number of subjects

#N
FFD_N_val<-NULL; FFD_N_inval<-NULL                     # First fixation duration (N)- mean
FFD_N_val_SD<-NULL; FFD_N_inval_SD<-NULL               # First fixation duration (N)- SD
FFD_N_inval_RAN<- NULL; FFD_N_inval_RAN_SD<- NULL;     # First fixation duration, random letter preview
FFD_N_inval_X<- NULL; FFD_N_inval_X_SD<- NULL;         # First fixation duration, x mask preview
FFD_N_inval_ORTH<- NULL; FFD_N_inval_ORTH_SD<- NULL;   # First fixation duration, orthographical preview
FFD_N_inval_UNREL<- NULL; FFD_N_inval_UNREL_SD<- NULL; # First fixation duration, Unrelated word preview
FFD_N_inval_SEM<- NULL; FFD_N_inval_SEM_SD<- NULL;     # First fixation duration, semantic word preview
FFD_N_inval_PHON<- NULL; FFD_N_inval_PHON_SD<- NULL;   # First fixation duration, phonological word preview
FFD_N_inval_PSEUD<- NULL; FFD_N_inval_PSEUD_SD<- NULL; # First fixation duration, pseudo word preview
GD_N_val<-NULL; GD_N_inval<-NULL                       # Gaze duration (N)- mean
GD_N_val_SD<-NULL; GD_N_inval_SD<-NULL                 # Gaze duration (N)- SD (Stan. Dev.)
GD_N_inval_RAN<- NULL; GD_N_inval_RAN_SD<- NULL;       # Gaze duration, random letter preview
GD_N_inval_X<- NULL; GD_N_inval_X_SD<- NULL;           # Gaze duration, x mask preview
GD_N_inval_ORTH<- NULL; GD_N_inval_ORTH_SD<- NULL;     # Gaze duration, orthographical preview
GD_N_inval_UNREL<- NULL; GD_N_inval_UNREL_SD<- NULL;   # Gaze duration, unrelated word preview
GD_N_inval_SEM<- NULL; GD_N_inval_SEM_SD<- NULL;       # Gaze duration, semantic word preview
GD_N_inval_PHON<- NULL; GD_N_inval_PHON_SD<- NULL;     # Gaze duration, phonological word preview
GD_N_inval_PSEUD<- NULL; GD_N_inval_PSEUD_SD<- NULL;   # Gaze duration, pseudo word preview
Total_N_val<-NULL; Total_N_inval<-NULL                 # Total fixations duration (N)- mean
Total_N_val_SD<-NULL; Total_N_inval_SD<-NULL           # Total fixations duration (N)- SD
Total_N_inval_RAN<- NULL; Total_N_inval_RAN_SD<- NULL; # Total fixations duration, random letter preview
Total_N_inval_X<- NULL; Total_N_inval_X_SD<- NULL;     # Total fixations duration, x mask preview
Total_N_inval_ORTH<- NULL; Total_N_inval_ORTH_SD<- NULL;# Total fixations duration, orthographical preview
Total_N_inval_UNREL<- NULL; Total_N_inval_UNREL_SD<- NULL;# Total fixations duration, unrelated word preview
Total_N_inval_SEM<- NULL; Total_N_inval_SEM_SD<- NULL;# Total fixations duration, semantic word preview
Total_N_inval_PHON<- NULL; Total_N_inval_PHON_SD<- NULL;# Total fixations duration, phonological word preview
Total_N_inval_PSEUD<- NULL; Total_N_inval_PSEUD_SD<- NULL;# Total fixations duration, pseudo word preview
SFD_N_val<- NULL; SFD_N_inval<- NULL                   # Single fixation duration (N)- mean
SFD_N_val_SD<- NULL; SFD_N_inval_SD<- NULL             # Single fixation duration (N)- SD
SFD_N_inval_RAN<- NULL; SFD_N_inval_RAN_SD<- NULL;     # Single fixation duration, random letter preview
SFD_N_inval_X<- NULL; SFD_N_inval_X_SD<- NULL;         # Single fixation duration, x mask preview
SFD_N_inval_ORTH<- NULL; SFD_N_inval_ORTH_SD<- NULL;   # Single fixation duration, orthographical preview
SFD_N_inval_UNREL<- NULL; SFD_N_inval_UNREL_SD<- NULL; # Single fixation duration, unrelated word preview
SFD_N_inval_SEM<- NULL; SFD_N_inval_SEM_SD<- NULL;     # Single fixation duration, semantic word preview
SFD_N_inval_PHON<- NULL; SFD_N_inval_PHON_SD<- NULL;   # Single fixation duration, phonological word preview
SFD_N_inval_PSEUD<- NULL; SFD_N_inval_PSEUD_SD<- NULL;   # Single fixation duration, pseudo word preview
FixProb_N_val<- NULL; FixProb_N_inval<- NULL           # Fixation probability (N)- mean
FixProb_N_val_SD<- NULL; FixProb_N_inval_SD<- NULL     # Fixation probability (N)- SD
LandPos_N_val<- NULL; LandPos_N_inval<- NULL           # Landing Position (N)- mean
LandPos_N_val_SD<- NULL; LandPos_N_inval_SD<- NULL     # Landing Position (N)- SD

#N+1
FFD_N1_val<-NULL; FFD_N1_inval<-NULL                   # First fixation duration (N+1)- mean
FFD_N1_val_SD<-NULL; FFD_N1_inval_SD<-NULL             # First fixation duration (N+1)- SD
FFD_N1_inval_RAN<- NULL; FFD_N1_inval_RAN_SD<- NULL;   # First fixation duration, random letter preview
FFD_N1_inval_X<- NULL; FFD_N1_inval_X_SD<- NULL;       # First fixation duration, x mask preview
FFD_N1_inval_ORTH<- NULL; FFD_N1_inval_ORTH_SD<- NULL; # First fixation duration, orthographical preview
FFD_N1_inval_UNREL<- NULL; FFD_N1_inval_UNREL_SD<- NULL; # First fixation duration, unrelated word preview
FFD_N1_inval_SEM<- NULL; FFD_N1_inval_SEM_SD<- NULL; # First fixation duration, semantic word preview
FFD_N1_inval_PHON<- NULL; FFD_N1_inval_PHON_SD<- NULL; # First fixation duration, phonological word preview
FFD_N1_inval_PSEUD<- NULL; FFD_N1_inval_PSEUD_SD<- NULL; # First fixation duration, pseudo word preview
GD_N1_val<-NULL; GD_N1_inval<-NULL                     # Gaze duration (N+1)- mean
GD_N1_val_SD<-NULL; GD_N1_inval_SD<-NULL               # Gaze duration (N+1)- SD (Stan. Dev.)
GD_N1_inval_RAN<- NULL; GD_N1_inval_RAN_SD<- NULL;     # Gaze duration, random letter preview
GD_N1_inval_X<- NULL; GD_N1_inval_X_SD<- NULL;         # Gaze duration, x mask preview
GD_N1_inval_ORTH<- NULL; GD_N1_inval_ORTH_SD<- NULL;   # Gaze duration, orthographical preview
GD_N1_inval_UNREL<- NULL; GD_N1_inval_UNREL_SD<- NULL; # Gaze duration, unrelated word preview
GD_N1_inval_SEM<- NULL; GD_N1_inval_SEM_SD<- NULL;     # Gaze duration, semantic word preview
GD_N1_inval_PHON<- NULL; GD_N1_inval_PHON_SD<- NULL;   # Gaze duration, phonological word preview
GD_N1_inval_PSEUD<- NULL; GD_N1_inval_PSEUD_SD<- NULL; # Gaze duration, pseudo word preview
Total_N1_val<-NULL; Total_N1_inval<-NULL               # Total fixations duration (N+1)- mean
Total_N1_val_SD<-NULL; Total_N1_inval_SD<-NULL         # Total fixations duration (N+1)- SD
Total_N1_inval_RAN<- NULL; Total_N1_inval_RAN_SD<- NULL;# Total fixations duration, random letter preview
Total_N1_inval_X<- NULL; Total_N1_inval_X_SD<- NULL;   # Total fixations duration, x mask preview
Total_N1_inval_ORTH<- NULL; Total_N1_inval_ORTH_SD<- NULL;   # Total fixations duration, orthographical preview
Total_N1_inval_UNREL<- NULL; Total_N1_inval_UNREL_SD<- NULL;   # Total fixations duration, unrelated word preview
Total_N1_inval_SEM<- NULL; Total_N1_inval_SEM_SD<- NULL;   # Total fixations duration, semantic word preview
Total_N1_inval_PHON<- NULL; Total_N1_inval_PHON_SD<- NULL;   # Total fixations duration, phonological word preview
Total_N1_inval_PSEUD<- NULL; Total_N1_inval_PSEUD_SD<- NULL;   # Total fixations duration, pseudo word preview
SFD_N1_val<- NULL; SFD_N1_inval<- NULL                 # Single fixation duration (N)- mean
SFD_N1_val_SD<- NULL; SFD_N1_inval_SD<- NULL           # Single fixation duration (N)- SD
SFD_N1_inval_RAN<- NULL; SFD_N1_inval_RAN_SD<- NULL;   # Single fixation duration, random letter preview
SFD_N1_inval_X<- NULL; SFD_N1_inval_X_SD<- NULL;       # Single fixation duration, x mask preview
SFD_N1_inval_ORTH<- NULL; SFD_N1_inval_ORTH_SD<- NULL;   # Single fixation duration, orthographical preview
SFD_N1_inval_UNREL<- NULL; SFD_N1_inval_UNREL_SD<- NULL; # Single fixation duration, unrelated word preview
SFD_N1_inval_SEM<- NULL; SFD_N1_inval_SEM_SD<- NULL;   # Single fixation duration, semantic word preview
SFD_N1_inval_PHON<- NULL; SFD_N1_inval_PHON_SD<- NULL;   # Single fixation duration, phonological word preview
SFD_N1_inval_PSEUD<- NULL; SFD_N1_inval_PSEUD_SD<- NULL;   # Single fixation duration, pseudo word preview
FixProb_N1_val<- NULL; FixProb_N1_inval<- NULL         # Fixation probability (N+1)- mean
FixProb_N1_val_SD<- NULL; FixProb_N1_inval_SD<- NULL   # Fixation probability (N+1)- SD
LandPos_N1_val<- NULL; LandPos_N1_inval<- NULL         # Landing Position (N+1)- mean
LandPos_N1_val_SD<- NULL; LandPos_N1_inval_SD<- NULL   # Landing Position (N+1)- SD


#########
#Studies:
#########


####
#1
####
ID[1]<-1
Paper[1]<- "Yan, Richter, Shu, & Kliegl (2009)"
Language[1]<- "Chinese"
N<-51-3
#N
FFD_N_val<-222; FFD_N_inval<- NA    # calculated from publicly available data
FFD_N_val_SD<- 64; FFD_N_inval_SD<-NA
FFD_N_inval_RAN<- NA; FFD_N_inval_RAN_SD<- NA;
FFD_N_inval_X<- NA; FFD_N_inval_X_SD<- NA;
FFD_N_inval_ORTH<- 227; FFD_N_inval_ORTH_SD<- 68;
FFD_N_inval_UNREL<- 228; FFD_N_inval_UNREL_SD<- 71;
FFD_N_inval_SEM<- 223; FFD_N_inval_SEM_SD<- 65;
FFD_N_inval_PHON<- 227; FFD_N_inval_PHON_SD<- 71;
FFD_N_inval_PSEUD<- NA; FFD_N_inval_PSEUD_SD<- NA;
GD_N_val<-255; GD_N_inval<-NA
GD_N_val_SD<-119; GD_N_inval_SD<-NA
GD_N_inval_RAN<- NA; GD_N_inval_RAN_SD<- NA;
GD_N_inval_X<- NA; GD_N_inval_X_SD<- NA;
GD_N_inval_ORTH<- 266; GD_N_inval_ORTH_SD<- 115;
GD_N_inval_UNREL<- 278; GD_N_inval_UNREL_SD<- 143;
GD_N_inval_SEM<- 255; GD_N_inval_SEM_SD<- 110;
GD_N_inval_PHON<- 276; GD_N_inval_PHON_SD<- 139;
GD_N_inval_PSEUD<- NA; GD_N_inval_PSEUD_SD<- NA;
Total_N_val<-NA; Total_N_inval<-NA
Total_N_val_SD<-NA; Total_N_inval_SD<-NA
Total_N_inval_RAN<- NA; Total_N_inval_RAN_SD<- NA;
Total_N_inval_X<- NA; Total_N_inval_X_SD<- NA;
Total_N_inval_ORTH<- NA; Total_N_inval_ORTH_SD<- NA;
Total_N_inval_UNREL<- NA; Total_N_inval_UNREL_SD<- NA;
Total_N_inval_SEM<- NA; Total_N_inval_SEM_SD<- NA;
Total_N_inval_PHON<- NA; Total_N_inval_PHON_SD<- NA;
Total_N_inval_PSEUD<- NA; Total_N_inval_PSEUD_SD<- NA;
SFD_N_val<-NA; SFD_N_inval<-NA
SFD_N_val_SD<-NA; SFD_N_inval_SD<-NA
SFD_N_inval_RAN<- NA; SFD_N_inval_RAN_SD<- NA;  
SFD_N_inval_X<- NA; SFD_N_inval_X_SD<- NA;
SFD_N_inval_ORTH<- NA; SFD_N_inval_ORTH_SD<- NA;
SFD_N_inval_UNREL<- NA; SFD_N_inval_UNREL_SD<- NA;
SFD_N_inval_SEM<- NA; SFD_N_inval_SEM_SD<- NA;
SFD_N_inval_PHON<- NA; SFD_N_inval_PHON_SD<- NA;
SFD_N_inval_PSEUD<- NA; SFD_N_inval_PSEUD_SD<- NA;
FixProb_N_val<-NA; FixProb_N_inval<-NA
FixProb_N_val_SD<-NA; FixProb_N_inval_SD<-NA
LandPos_N_val<- NA; LandPos_N_inval<- NA
LandPos_N_val_SD<- NA; LandPos_N_inval_SD<- NA
# N+1 
FFD_N1_val<-221; FFD_N1_inval<-NA  # Table 
FFD_N1_val_SD<-63; FFD_N1_inval_SD<-NA
FFD_N1_inval_RAN<- NA; FFD_N1_inval_RAN_SD<- NA; 
FFD_N1_inval_X<- NA; FFD_N1_inval_X_SD<- NA; 
FFD_N1_inval_ORTH<- 245; FFD_N1_inval_ORTH_SD<- 78; 
FFD_N1_inval_UNREL<- 258; FFD_N1_inval_UNREL_SD<- 88;
FFD_N1_inval_SEM<- 238; FFD_N1_inval_SEM_SD<- 77;
FFD_N1_inval_PHON<- 254; FFD_N1_inval_PHON_SD<- 82;
FFD_N1_inval_PSEUD<- NA; FFD_N1_inval_PSEUD_SD<- NA;
GD_N1_val<-263; GD_N1_inval<-NA
GD_N1_val_SD<-123; GD_N1_inval_SD<-NA
GD_N1_inval_RAN<- NA; GD_N1_inval_RAN_SD<- NA;
GD_N1_inval_X<- NA; GD_N1_inval_X_SD<- NA;   
GD_N1_inval_ORTH<- 316; GD_N1_inval_ORTH_SD<- 155; 
GD_N1_inval_UNREL<- 338; GD_N1_inval_UNREL_SD<- 181;
GD_N1_inval_SEM<- 312; GD_N1_inval_SEM_SD<- 149;
GD_N1_inval_PHON<- 319; GD_N1_inval_PHON_SD<- 152;
GD_N1_inval_PSEUD<- NA; GD_N1_inval_PSEUD_SD<- NA;
Total_N1_val<-NA; Total_N1_inval<-NA
Total_N1_val_SD<-NA; Total_N1_inval_SD<-NA
Total_N1_inval_RAN<- NA; Total_N1_inval_RAN_SD<- NA;
Total_N1_inval_X<- NA; Total_N1_inval_X_SD<- NA;
Total_N1_inval_ORTH<- NA; Total_N1_inval_ORTH_SD<- NA;
Total_N1_inval_UNREL<- NA; Total_N1_inval_UNREL_SD<- NA;
Total_N1_inval_SEM<- NA; Total_N1_inval_SEM_SD<- NA;
Total_N1_inval_PHON<- NA; Total_N1_inval_PHON_SD<- NA;
Total_N1_inval_PSEUD<- NA; Total_N1_inval_PSEUD_SD<- NA;
SFD_N1_val<-NA; SFD_N1_inval<-NA
SFD_N1_val_SD<-NA; SFD_N1_inval_SD<-NA
SFD_N1_inval_RAN<- NA; SFD_N1_inval_RAN_SD<- NA; 
SFD_N1_inval_X<- NA; SFD_N1_inval_X_SD<- NA;
SFD_N1_inval_ORTH<- NA; SFD_N1_inval_ORTH_SD<- NA;
SFD_N1_inval_UNREL<- NA; SFD_N1_inval_UNREL_SD<- NA;
SFD_N1_inval_SEM<- NA; SFD_N1_inval_SEM_SD<- NA;
SFD_N1_inval_PHON<- NA; SFD_N1_inval_PHON_SD<- NA;
SFD_N1_inval_PSEUD<- NA; SFD_N1_inval_PSEUD_SD<- NA;
FixProb_N1_val<-NA; FixProb_N1_inval<-NA
FixProb_N1_val_SD<-NA; FixProb_N1_inval_SD<-NA
LandPos_N1_val<- NA; LandPos_N1_inval<- NA
LandPos_N1_val_SD<- NA; LandPos_N1_inval_SD<- NA

####
#2
####
#
j=2;

ID[j]<-2
Paper[j]<- "Hohenstein & Kliegl (2014), Exp.3"
Language[j]<-'German'
N[j]<- 48 
#N
FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA
# N+1 
FFD_N1_val[j]<- 221; FFD_N1_inval[j]<-NA  # Table 8
FFD_N1_val_SD[j]<-70; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 260; FFD_N1_inval_UNREL_SD[j]<- 99; 
FFD_N1_inval_SEM[j]<- 239; FFD_N1_inval_SEM_SD[j]<- 100;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- 271; FFD_N1_inval_PSEUD_SD[j]<- 99;
GD_N1_val[j]<-235; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-84; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 281; GD_N1_inval_UNREL_SD[j]<- 104;
GD_N1_inval_SEM[j]<- 255; GD_N1_inval_SEM_SD[j]<- 110;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- 292; GD_N1_inval_PSEUD_SD[j]<- 103;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 222; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-71; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- 268; SFD_N1_inval_UNREL_SD[j]<- 100; 
SFD_N1_inval_SEM[j]<- 242; SFD_N1_inval_SEM_SD[j]<- 102;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- 278; SFD_N1_inval_PSEUD_SD[j]<- 99; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
#3
####
#
j=3;

ID[j]<-j
Paper[j]<- "Inhoff & Tousman (1990)"
Language[j]<-'English'
N[j]<- 16
#N
FFD_N_val[j]<- NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<- NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<-(268 + 240)/2; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-(48 + 53)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- (317 + 250)/2; FFD_N1_inval_X_SD[j]<- (61 + 33)/2; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (281 + 290)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (54 + 57)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- (319 + 343)/2; GD_N1_inval_X_SD[j]<- (65 + 82)/2;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
#4
####
#
j=4;

ID[j]<-j
Paper[j]<- "Rayner, Schotter, Drieghe (2014)"
Language[j]<-'English'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA
# N+1 
FFD_N1_val[j]<-NA; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 267; GD_N1_inval[j]<- NA
GD_N1_val_SD[j]<- 54; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 292; GD_N1_inval_ORTH_SD[j]<- 58.7;
GD_N1_inval_UNREL[j]<- 314; GD_N1_inval_UNREL_SD[j]<- 64;
GD_N1_inval_SEM[j]<- 311; GD_N1_inval_SEM_SD[j]<- 64;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-252; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-49; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- 278; SFD_N1_inval_ORTH_SD[j]<- 48.9; 
SFD_N1_inval_UNREL[j]<- 307; SFD_N1_inval_UNREL_SD[j]<- 59; 
SFD_N1_inval_SEM[j]<- 293; SFD_N1_inval_SEM_SD[j]<- 54;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
#5
####
#
j=5;

ID[j]<-j
Paper[j]<- "Rayner, Balota, & Pollatsek (1986)"
Language[j]<-'English'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 1
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 251; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- 251; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- 250; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 214; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 215; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 234; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- 230; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 246; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 251; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 290; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- 286; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 6
####
#
j=6;

ID[j]<-j
Paper[j]<- "Schotter (2013)"
Language[j]<-'English'
N[j]<-36
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<-223; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<-25.8; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 234; FFD_N1_inval_UNREL_SD[j]<- 32.4; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 247; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 33.6; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 267; GD_N1_inval_UNREL_SD[j]<- 42;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-286; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-56.4; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- 320; Total_N1_inval_UNREL_SD[j]<- 72;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 227; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 28.2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- 244; SFD_N1_inval_UNREL_SD[j]<- 38.4; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA



####
# 7
####
#
j=7;

ID[j]<-j
Paper[j]<- "Schotter (2013), Exp.2"
Language[j]<-'English'
N[j]<- 40
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 225; FFD_N1_inval[j]<-NA  # Table 5
FFD_N1_val_SD[j]<- 32.8; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 236; FFD_N1_inval_UNREL_SD[j]<- 37.3; 
FFD_N1_inval_SEM[j]<- 241; FFD_N1_inval_SEM_SD[j]<- 42.3;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 253; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 39.8; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 270; GD_N1_inval_UNREL_SD[j]<- 50.5;
GD_N1_inval_SEM[j]<- 273; GD_N1_inval_SEM_SD[j]<- 51.2;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 326; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 82.2; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- 351; Total_N1_inval_UNREL_SD[j]<- 75.8;
Total_N1_inval_SEM[j]<- 354; Total_N1_inval_SEM_SD[j]<- 88.5;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-232; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 32.8; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- 246; SFD_N1_inval_UNREL_SD[j]<- 40.4; 
SFD_N1_inval_SEM[j]<- 252; SFD_N1_inval_SEM_SD[j]<- 47.4;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA



####
# 8
####
#
j=8;

ID[j]<-j
Paper[j]<- "Rayner & Schotter (2014)"
Language[j]<-'English'
N[j]<-60
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 225; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 40.2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 231; FFD_N1_inval_UNREL_SD[j]<- 33.3; 
FFD_N1_inval_SEM[j]<- 234; FFD_N1_inval_SEM_SD[j]<- 38.7;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 245; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 48.02; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 259; GD_N1_inval_UNREL_SD[j]<- 47.2;
GD_N1_inval_SEM[j]<- 257; GD_N1_inval_SEM_SD[j]<- 53.4;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 298; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 75.1; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- 334; Total_N1_inval_UNREL_SD[j]<- 73.5;
Total_N1_inval_SEM[j]<- 328; Total_N1_inval_SEM_SD[j]<- 77.4;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 231; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 39.5; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- 238; SFD_N1_inval_UNREL_SD[j]<- 37.9; 
SFD_N1_inval_SEM[j]<- 244; SFD_N1_inval_SEM_SD[j]<- 48.7;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 9
####
#
j=9;

ID[j]<-j
Paper[j]<- "Tsai, Kliegl, & Yan (2012)"
Language[j]<-'Chinese'
N[j]<- 50
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 261; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<- 49.4; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 294; FFD_N1_inval_ORTH_SD[j]<- 49.4;
FFD_N1_inval_UNREL[j]<- 301; FFD_N1_inval_UNREL_SD[j]<- 49.4; 
FFD_N1_inval_SEM[j]<- 291; FFD_N1_inval_SEM_SD[j]<- 49.4;
FFD_N1_inval_PHON[j]<- 303; FFD_N1_inval_PHON_SD[j]<- 49.4;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 305; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 70.7; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 361; GD_N1_inval_ORTH_SD[j]<- 70.7;
GD_N1_inval_UNREL[j]<- 370; GD_N1_inval_UNREL_SD[j]<- 77.7;
GD_N1_inval_SEM[j]<- 350; GD_N1_inval_SEM_SD[j]<- 70.7;
GD_N1_inval_PHON[j]<- 374; GD_N1_inval_PHON_SD[j]<- 70.7;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 265; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 56.5; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- 299; SFD_N1_inval_ORTH_SD[j]<- 56.5; 
SFD_N1_inval_UNREL[j]<- 315; SFD_N1_inval_UNREL_SD[j]<- 56.5; 
SFD_N1_inval_SEM[j]<- 293; SFD_N1_inval_SEM_SD[j]<- 56.5;
SFD_N1_inval_PHON[j]<- 315; SFD_N1_inval_PHON_SD[j]<- 56.5; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA



####
# 10
####
#
j=10;

ID[j]<-j
Paper[j]<- "Yan, Zhou, Shu, Kliegl (2012)"
Language[j]<-'Chinese'
N[j]<- 48
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 252; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 20.7; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 293; FFD_N1_inval_UNREL_SD[j]<- 41.5; 
FFD_N1_inval_SEM[j]<- (279 + 281)/2; FFD_N1_inval_SEM_SD[j]<- 20.7; # same in both cond
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 289; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 41.5; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 366; GD_N1_inval_UNREL_SD[j]<- 62.3;
GD_N1_inval_SEM[j]<- (341 + 354)/2; GD_N1_inval_SEM_SD[j]<- 41.5; # same in both cond
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 252; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-  27.7; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- 298; SFD_N1_inval_UNREL_SD[j]<- 41.5; 
SFD_N1_inval_SEM[j]<- (280 + 284)/2; SFD_N1_inval_SEM_SD[j]<- 27.7; # same in both cond
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 11
####
#
j=11;

ID[j]<-j
Paper[j]<- "Hyona & Haikio (2005)"
Language[j]<- 'Finnish'
N[j]<- 22
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 226; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 39; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 245; FFD_N1_inval_UNREL_SD[j]<- 51; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 291; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 73; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 318; GD_N1_inval_UNREL_SD[j]<- 73;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 325; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 78; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- 357; Total_N1_inval_UNREL_SD[j]<- 81;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 12
####
#
j=12;

ID[j]<-j
Paper[j]<- "Rayner, Castelhano, & Yang (2010)"
Language[j]<-'English'
N[j]<- 36
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 265; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<- 40; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 282; FFD_N1_inval_RAN_SD[j]<- 48;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 306; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 49; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 343; GD_N1_inval_RAN_SD[j]<- 59;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 277; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 46; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- 301; SFD_N1_inval_RAN_SD[j]<- 57; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 13
####
#
j=13;

ID[j]<-j
Paper[j]<- "Rayner, Juhasz, & Brown (2007)"
Language[j]<-'English'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 288; FFD_N1_inval[j]<-NA  # Table 3 
FFD_N1_val_SD[j]<- 41; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 324; FFD_N1_inval_RAN_SD[j]<- 46;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 320; FFD_N1_inval_UNREL_SD[j]<- 43; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 327; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 64; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 369; GD_N1_inval_RAN_SD[j]<- 56;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 374; GD_N1_inval_UNREL_SD[j]<- 56;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 374; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 103; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- 417; Total_N1_inval_RAN_SD[j]<- 89;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- 426; Total_N1_inval_UNREL_SD[j]<- 102;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 14
####
#
j=14;

ID[j]<-j
Paper[j]<- "Rayner, Juhasz, & Brown (2007), Exp. 2"
Language[j]<-'English'
N[j]<- 36
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 293; FFD_N1_inval[j]<-NA  # Table 8
FFD_N1_val_SD[j]<- 50; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 328; FFD_N1_inval_RAN_SD[j]<- 51;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 322; FFD_N1_inval_UNREL_SD[j]<- 61; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 319; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 71; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 356; GD_N1_inval_RAN_SD[j]<- 73;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 337; GD_N1_inval_UNREL_SD[j]<- 65;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 355; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 87; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- 399; Total_N1_inval_RAN_SD[j]<- 101;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- 386; Total_N1_inval_UNREL_SD[j]<- 89;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 15
####
#
j=15;

ID[j]<-j
Paper[j]<- "Veldre & Andrews (2015b)"
Language[j]<- 'English'
N[j]<- 94
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 239; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<- 32; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 252; FFD_N1_inval_ORTH_SD[j]<- 31.02;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- 251; FFD_N1_inval_PSEUD_SD[j]<- 32.9;
GD_N1_val[j]<- 273; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 42.6; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 301; GD_N1_inval_ORTH_SD[j]<- 47.5;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- 295; GD_N1_inval_PSEUD_SD[j]<- 43.6;
Total_N1_val[j]<- 425; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 101.8; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- 444; Total_N1_inval_ORTH_SD[j]<- 87.2;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- 447; Total_N1_inval_PSEUD_SD[j]<- 87.2;
SFD_N1_val[j]<- 247; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 36.8; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- 260; SFD_N1_inval_ORTH_SD[j]<- 34.9; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- 262; SFD_N1_inval_PSEUD_SD[j]<- 38.7; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 16
####
#
j=16;

ID[j]<-j
Paper[j]<- "Veldre & Andrews (2015a)"
Language[j]<-'English'
N[j]<- 107
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (211 + 219)/2; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- (26.8 + 30)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- (263 + 267)/2; FFD_N1_inval_RAN_SD[j]<- (36.2 + 37.2)/2;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (229 + 236)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 37.2; GD_N1_inval_SD[j]<-NA # same SE
GD_N1_inval_RAN[j]<- (299 + 306)/2; GD_N1_inval_RAN_SD[j]<- (41.3 + 46.5)/2;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- (283 + 294)/2; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- (60 + 62.06)/2; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- (355 + 381)/2; Total_N1_inval_RAN_SD[j]<- (60 + 67.2)/2;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- (210 + 219)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- (26.8 + 32.06)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- (275 + 282)/2; SFD_N1_inval_RAN_SD[j]<- (41.3 + 40.3)/2; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 17
####
#
j=17;

ID[j]<-j
Paper[j]<- "Angele & Rayner (2011)"
Language[j]<-'English'
N[j]<- 32
  #N
  FFD_N_val[j]<- 239; FFD_N_inval[j]<-NA  # Table 3
FFD_N_val_SD[j]<- 89; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- 247; FFD_N_inval_RAN_SD[j]<- 95;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 282; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 117; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- 297; GD_N_inval_RAN_SD[j]<- 131; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<- 247; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<- 92; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- 256; SFD_N_inval_RAN_SD[j]<- 96;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 224; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<- 83; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 238; FFD_N1_inval_RAN_SD[j]<- 75;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 233; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 91; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 254; GD_N1_inval_RAN_SD[j]<- 84;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 226; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 85; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- 242; SFD_N1_inval_RAN_SD[j]<- 70; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 18
####
#
j=18;

ID[j]<-j
Paper[j]<- "Yan (2015)"
Language[j]<-'Chinese'
N[j]<- 42
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (266 + 250)/2; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- (50 + 38)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- (283 + 287)/2; FFD_N1_inval_UNREL_SD[j]<- (50 + 43)/2; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- (286 + 271)/2; FFD_N1_inval_PSEUD_SD[j]<- (55 + 41)/2;
GD_N1_val[j]<- (319 + 289)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (63 + 47)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- (358 + 362)/2; GD_N1_inval_UNREL_SD[j]<- (88 + 69)/2;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- (364 + 335)/2; GD_N1_inval_PSEUD_SD[j]<- (82 + 64)/2;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- (267 + 247)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- (52 + 38)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- (289 + 292)/2; SFD_N1_inval_UNREL_SD[j]<- (68 + 52)/2; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- (288 + 272)/2; SFD_N1_inval_PSEUD_SD[j]<- (65 + 47)/2; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 19
####
#
j=19;

ID[j]<-j
Paper[j]<- "Yen, Tsai, Tzeng, & Hung (2008)"
Language[j]<-'Chinese'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 260; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 44.5; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 303; FFD_N1_inval_UNREL_SD[j]<- 69.2; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- 307; FFD_N1_inval_PSEUD_SD[j]<- 63.9;
GD_N1_val[j]<- 286; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 61.7; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 347; GD_N1_inval_UNREL_SD[j]<- 102.6;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- 353; GD_N1_inval_PSEUD_SD[j]<- 94.4;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 20
####
#
j=20;

ID[j]<-j
Paper[j]<- "Yang, Wang, Tong, & Rayner (2012)"
Language[j]<-'Chinese'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 1
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 222; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 31; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- 231; GD_N_inval_UNREL_SD[j]<- 46; 
GD_N_inval_SEM[j]<- 225; GD_N_inval_SEM_SD[j]<- 39; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<- 219; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<- 30; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- 229; SFD_N_inval_UNREL_SD[j]<- 49;
SFD_N_inval_SEM[j]<- 223; SFD_N_inval_SEM_SD[j]<- 39;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<-NA; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 248; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 45; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 278; GD_N1_inval_UNREL_SD[j]<- 52;
GD_N1_inval_SEM[j]<- 279; GD_N1_inval_SEM_SD[j]<- 57;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 240; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 39; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- 268; SFD_N1_inval_UNREL_SD[j]<- 51; 
SFD_N1_inval_SEM[j]<- 265; SFD_N1_inval_SEM_SD[j]<- 47;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 21
####
#
j=21;

ID[j]<-j
Paper[j]<- "Yang, Wang, Tong, & Rayner (2012), Exp.2"
Language[j]<-'Chinese'
N[j]<- 48
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 3
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 232; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 46; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- (222 + 226)/2; GD_N_inval_UNREL_SD[j]<- (34 + 33)/2; 
GD_N_inval_SEM[j]<- 227; GD_N_inval_SEM_SD[j]<- 34; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<- 226; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<- 41; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- (215 + 224)/2; SFD_N_inval_UNREL_SD[j]<- (32 + 34)/2;
SFD_N_inval_SEM[j]<- 226; SFD_N_inval_SEM_SD[j]<- 35;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<-NA; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 260; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 41; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- (274 + 283)/2; GD_N1_inval_UNREL_SD[j]<- (50 + 59)/2;
GD_N1_inval_SEM[j]<- 273; GD_N1_inval_SEM_SD[j]<- 54;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 253; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 36; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- (269 + 271)/2; SFD_N1_inval_UNREL_SD[j]<- (46 + 55)/2; 
SFD_N1_inval_SEM[j]<- 260; SFD_N1_inval_SEM_SD[j]<- 45;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA



####
# 22
####
#
j=22;

ID[j]<-j
Paper[j]<- "Yang, Wang, Xu, & Rayner (2009)"
Language[j]<-'Chinese'
N[j]<- 42 # see Results section
  #N
  FFD_N_val[j]<- 213; FFD_N_inval[j]<-NA  # Table 2
FFD_N_val_SD[j]<- 94; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- 227; FFD_N_inval_UNREL_SD[j]<- 85;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 219; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 101; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- 238; GD_N_inval_UNREL_SD[j]<- 91; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 231; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 74; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 255; FFD_N1_inval_UNREL_SD[j]<- 85; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 245; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 79; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 284; GD_N1_inval_UNREL_SD[j]<- 95;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 23
####
#
j=23;

ID[j]<-j
Paper[j]<- "Yang, Wang, Xu, & Rayner (2009), Exp.2"
Language[j]<-'Chinese'
N[j]<- 66
  #N
  FFD_N_val[j]<- NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (235 + 234)/2; FFD_N1_inval[j]<-NA  # Table 4
FFD_N1_val_SD[j]<- (71 + 75)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- (269 + 254)/2; FFD_N1_inval_UNREL_SD[j]<- (96 + 88)/2; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- (275 + 259)/2; FFD_N1_inval_PSEUD_SD[j]<- (85 + 86)/2;
GD_N1_val[j]<- (311 + 276)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (148 + 117)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- (361 + 342)/2; GD_N1_inval_UNREL_SD[j]<- (174 + 148)/2;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- (379 + 373)/2; GD_N1_inval_PSEUD_SD[j]<- (179 + 155)/2;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 24
####
#
j=24;

ID[j]<-j
Paper[j]<- "Winskel & Salehuddin (2014)"
Language[j]<-'Malay'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (247 + 242)/2; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- (276 + 279)/2; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (269 + 271)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- (326 + 329)/2; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA



####
# 25
####
#
j=25;

ID[j]<-j
Paper[j]<- "Yang, Rayner, Li, & Wang (2012)"
Language[j]<-'Chinese'
N[j]<- 36
  #N
  FFD_N_val[j]<- 254; FFD_N_inval[j]<-NA  # Table 1
FFD_N_val_SD[j]<- 44; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- 267; FFD_N_inval_UNREL_SD[j]<- 39;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 269; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 48; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- 291; GD_N_inval_UNREL_SD[j]<- 54; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<- 258; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<- 44; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- 269; SFD_N_inval_UNREL_SD[j]<- 42;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 256; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<- 41; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 270; FFD_N1_inval_UNREL_SD[j]<- 39; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 307; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 78; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 382; GD_N1_inval_UNREL_SD[j]<- 113;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 261; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 58; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- 280; SFD_N1_inval_UNREL_SD[j]<- 56; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 26
####
#
j=26;

ID[j]<-j
Paper[j]<- "Angele, Slattery, Yang, Kliegl, & Rayner (2008)"
Language[j]<-'English'
N[j]<- 32
  #N
  FFD_N_val[j]<- 229; FFD_N_inval[j]<-NA  # Table 2
FFD_N_val_SD[j]<- 32; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- 228; FFD_N_inval_RAN_SD[j]<- 32;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 286; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 65; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- 283; GD_N_inval_RAN_SD[j]<- 59; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<- 234; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<- 38; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- 234; SFD_N_inval_RAN_SD[j]<- 35;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 252; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<- 41; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 275; FFD_N1_inval_RAN_SD[j]<- 45;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 290; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 60; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 338; GD_N1_inval_RAN_SD[j]<- 89;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 259; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 46; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- 296; SFD_N1_inval_RAN_SD[j]<- 50; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 27
####
#
j=27;

ID[j]<-j
Paper[j]<- "Lima (1987)"
Language[j]<-'English'
N[j]<- 18
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (223 + 234)/2; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- (222 + 243)/2; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (277 + 286)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- (293 + 306)/2; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 28
####
#
j=28;

ID[j]<-j
Paper[j]<- "Lima (1987), Exp.2"
Language[j]<-'English'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (218 + 224)/2; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- (232 + 247)/2; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (272 + 284)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- (301 + 313)/2; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA

####
# 29
####
#
j=29;

ID[j]<-j
Paper[j]<- "Belanger, Mayberry, & Rayner (2013)"
Language[j]<-'English'
N[j]<- 20
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (232 + 219)/2; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- (76 + 72)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- (255 + 238)/2; FFD_N1_inval_ORTH_SD[j]<- (94 + 88)/2;
FFD_N1_inval_UNREL[j]<- (243 + 241)/2; FFD_N1_inval_UNREL_SD[j]<- (99 + 101)/2; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- (231 + 241)/2; FFD_N1_inval_PHON_SD[j]<- (69 + 84)/2;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (263 + 246)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (114 + 92)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- (282 + 262)/2; GD_N1_inval_ORTH_SD[j]<- (118 + 107)/2;
GD_N1_inval_UNREL[j]<- (274 + 267)/2; GD_N1_inval_UNREL_SD[j]<- (125 + 128)/2;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- (266 + 266)/2; GD_N1_inval_PHON_SD[j]<- (101 + 101)/2;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- (237 + 225)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- (77 + 71)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- (257 + 246)/2; SFD_N1_inval_ORTH_SD[j]<- (94 + 87)/2; 
SFD_N1_inval_UNREL[j]<- (248 + 240)/2; SFD_N1_inval_UNREL_SD[j]<- (103 + 99)/2; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- (238 + 250)/2; SFD_N1_inval_PHON_SD[j]<- (69 + 86)/2; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 30
####
#
j=30;

ID[j]<-j
Paper[j]<- "Balota, Pollatsek, & Rayner (1985)"
Language[j]<-'English'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1               # WARNING!: Different measures are presented in different tables
FFD_N1_val[j]<- 221; FFD_N1_inval[j]<-NA  # Table 5
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 223; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 240; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 248; GD_N1_inval[j]<-NA          # Table 3
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 256; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 291; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 31
####
#
j=31;

ID[j]<-j
Paper[j]<- "Johnson, Perea, & Rayner (2007)"
Language[j]<-'English'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 264; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- (283 + 287)/2; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 278; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- (308 + 317)/2; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 270; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- (292 + 292)/2; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 32
####
#
j=32;

ID[j]<-j
Paper[j]<- "Johnson, Perea, & Rayner (2007), Exp.2"
Language[j]<-'English'
N[j]<- 35
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 282; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- (307 + 300)/2; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 307; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- (344 + 333)/2; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 287; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- (315 + 309)/2; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 33
####
#
j=33;

ID[j]<-j
Paper[j]<- "Johnson, Perea, & Rayner (2007), Exp.3"
Language[j]<-'English'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 286; FFD_N1_inval[j]<-NA  # Table 4 
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- (322 + 296 + 314)/3; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 301; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- (352 + 312 + 323)/3; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 288; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- (334 + 303 + 320)/3; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 34
####
#
j=34;

ID[j]<-j
Paper[j]<- "Briihl & Inhoff (1995), Exp.2"
Language[j]<-'English'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 236; FFD_N1_inval[j]<-NA  # Table 6 (I take the "Global" statistics)
FFD_N1_val_SD[j]<- 28.4; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- 260; FFD_N1_inval_X_SD[j]<- 36.7; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 290; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 63.6; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- 331; GD_N1_inval_X_SD[j]<- 58.7;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 35
####
#
j=35;

ID[j]<-j
Paper[j]<- "Angele, Laishley, Rayner, & Liversedge (2014)"
Language[j]<-'English'
N[j]<- 35
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (235 + 241)/2; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- (73 + 69)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- (272 + 266)/2; FFD_N1_inval_RAN_SD[j]<- (80 + 77)/2;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (250 + 270)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (95 + 107)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- (294 + 300)/2; GD_N1_inval_RAN_SD[j]<- (99 + 115)/2;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- (275 + 305)/2; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- (143 + 156)/2; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- (321 + 332)/2; Total_N1_inval_RAN_SD[j]<- (141 + 161)/2;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 36
####
#
j=36;

ID[j]<-j
Paper[j]<- "Angele & Rayner (2013)"
Language[j]<-'English'
N[j]<- 30 # because only half of participants read lowercase sentences
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 231; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<- 19.8; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 253; FFD_N1_inval_RAN_SD[j]<- 23.3;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 251; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 22.5; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 276; GD_N1_inval_RAN_SD[j]<- 27.9;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 37
####
#
j=37;

ID[j]<-j
Paper[j]<- "Pollatsek, Lesch, Morris, & Rayner (1992), Exp. 2"
Language[j]<-'English'
N[j]<- 40
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 260; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 295; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 301; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- 275; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 306; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 331; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 349; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- 317; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 38
####
#
j=38;

ID[j]<-j
Paper[j]<- "Miellet & Sparrow (2004)"
Language[j]<-'French'
N[j]<- 13 # 2 subjects were excluded
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 228; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- 239; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 272; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- 284; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA



####
# 39
####
#
j=39;

ID[j]<-j
Paper[j]<- "Binder, Pollatsek, & Rayner (1999)"
Language[j]<-'English'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (228 + 243)/2; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- (250 + 251)/2; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (237 + 260)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- (269 + 271)/2; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 40
####
#
j=40;

ID[j]<-j
Paper[j]<- "Chace, Rayner, & Well (2005)"
Language[j]<-'English'
N[j]<- 23 # low-skill subjects were removed (see paper)
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 262; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<- 29.4; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 285; FFD_N1_inval_RAN_SD[j]<- 29.4;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 276; FFD_N1_inval_ORTH_SD[j]<- 29.4;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- 269; FFD_N1_inval_PHON_SD[j]<- 29.4;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 292; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 43; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 318; GD_N1_inval_RAN_SD[j]<- 43;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 313; GD_N1_inval_ORTH_SD[j]<- 43;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- 304; GD_N1_inval_PHON_SD[j]<- 43;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 41
####
#
j=41;

ID[j]<-j
Paper[j]<- "Starr & Inhoff (2004)"
Language[j]<-'English'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 308; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 78.3; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 330; FFD_N1_inval_RAN_SD[j]<- 73.4;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 334; FFD_N1_inval_UNREL_SD[j]<- 83.2; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 361; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 88.1; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 400; GD_N1_inval_RAN_SD[j]<- 117.5;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 398; GD_N1_inval_UNREL_SD[j]<- 107.7;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 42
####
#
j=42;

ID[j]<-j
Paper[j]<- "Angele & Rayner (2013), Exp. 2"
Language[j]<-'English'
N[j]<- 40
  #N
  FFD_N_val[j]<- 207; FFD_N_inval[j]<-NA  # Table 6
FFD_N_val_SD[j]<- 72; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- 214; FFD_N_inval_RAN_SD[j]<- 72;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 248; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 117; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- 252; GD_N_inval_RAN_SD[j]<- 114; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<- 313; Total_N_inval[j]<-NA
Total_N_val_SD[j]<- 187; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- 322; Total_N_inval_RAN_SD[j]<- 184;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 236; FFD_N1_inval[j]<-NA  # Table 6
FFD_N1_val_SD[j]<- 81; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 248; FFD_N1_inval_RAN_SD[j]<- 82;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 279; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 115; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 322; GD_N1_inval_RAN_SD[j]<- 128;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 359; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 218; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- 400; Total_N1_inval_RAN_SD[j]<- 204;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 43
####
#
j=43;

ID[j]<-j
Paper[j]<- "White, Rayner, & Liversedge (2005)"
Language[j]<-'English'
N[j]<- 48
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- ((274 + 304)/2 + (262 + 273)/2)/2; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- ((85 + 114)/2 + (93 + 89)/2)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- ((321 + 295)/2 + (362 + 375)/2)/2; FFD_N1_inval_RAN_SD[j]<- ((112 + 91)/2 + (164 + 152)/2)/2;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- ((293 + 329)/2 + (269 + 286)/2)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- ((98 + 144)/2 + (104 + 103)/2)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- ((340 + 330)/2 + (398 + 416)/2)/2; GD_N1_inval_RAN_SD[j]<- ((132 + 123)/2 + (171 + 161)/2)/2;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- ((279 + 311)/2 + (262 + 275)/2)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- ((85 + 116)/2 + (94 + 90)/2)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- ((325 + 300)/2 + (379 + 390)/2)/2; SFD_N1_inval_RAN_SD[j]<- ((113 + 89)/2 + (166 + 155)/2)/2; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 44
####
#

j=44;

ID[j]<-j
Paper[j]<- "White, Warren, & Reichle (2011)"
Language[j]<-'English'
N[j]<- 32
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 268; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 130; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 273; FFD_N1_inval_RAN_SD[j]<- 96;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 291; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 141; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 298; GD_N1_inval_RAN_SD[j]<- 128;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 45
####
#
j=45;

ID[j]<-j
Paper[j]<- "White, Warren, & Reichle (2011), Exp.2"
Language[j]<-'English'
N[j]<- 36
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 250; FFD_N1_inval[j]<-NA  # Table 4
FFD_N1_val_SD[j]<- 92; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 280; FFD_N1_inval_RAN_SD[j]<- 98;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 269; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 108; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 319; GD_N1_inval_RAN_SD[j]<- 121;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 46
####
#
j=46;

ID[j]<-j
Paper[j]<- "Angele, Tran, & Rayner (2013)"
Language[j]<-'English'
N[j]<- 40
  #N
FFD_N_val[j]<- 230; FFD_N_inval[j]<-NA  # Table 3
FFD_N_val_SD[j]<- 76.1; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- 236; FFD_N_inval_RAN_SD[j]<- 77.9;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- 232; FFD_N_inval_UNREL_SD[j]<- 81.7;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 262; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 107; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- 278; GD_N_inval_RAN_SD[j]<- 119; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- 267; GD_N_inval_UNREL_SD[j]<- 116; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<- 307; Total_N_inval[j]<-NA
Total_N_val_SD[j]<- 151; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- 351; Total_N_inval_RAN_SD[j]<- 190;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- 347; Total_N_inval_UNREL_SD[j]<- 193;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<- 233; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<- 75.6; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- 241; SFD_N_inval_RAN_SD[j]<- 77.8;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- 233; SFD_N_inval_UNREL_SD[j]<- 79.2;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 250; FFD_N1_inval[j]<-NA  # Table 4
FFD_N1_val_SD[j]<- 92.3; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 290; FFD_N1_inval_RAN_SD[j]<- 115;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 283; FFD_N1_inval_UNREL_SD[j]<- 109; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 279; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 115; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 322; GD_N1_inval_RAN_SD[j]<- 125;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 318; GD_N1_inval_UNREL_SD[j]<- 126;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 314; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 152; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- 398; Total_N1_inval_RAN_SD[j]<- 205;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- 392; Total_N1_inval_UNREL_SD[j]<- 196;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 256; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 92.7; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- 302; SFD_N1_inval_RAN_SD[j]<- 108; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- 297; SFD_N1_inval_UNREL_SD[j]<- 107; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 47
####
#
j=47;

ID[j]<-j
Paper[j]<- "Angele, Tran, & Rayner (2013), Exp.2"
Language[j]<-'English'
N[j]<- 40
  #N
  FFD_N_val[j]<- 223; FFD_N_inval[j]<-NA  # Table 10
FFD_N_val_SD[j]<- 68.9; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- 222; FFD_N_inval_PSEUD_SD[j]<- 72.5;
GD_N_val[j]<- 241; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 89.7; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- 243; GD_N_inval_PSEUD_SD[j]<- 95.3;
Total_N_val[j]<- 307; Total_N_inval[j]<-NA
Total_N_val_SD[j]<- 169; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- 328; Total_N_inval_PSEUD_SD[j]<- 179;
SFD_N_val[j]<- 224; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<- 68.6; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- 225; SFD_N_inval_PSEUD_SD[j]<- 72.7;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 234; FFD_N1_inval[j]<-NA  # Table 11
FFD_N1_val_SD[j]<- 75.1; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- 248; FFD_N1_inval_PSEUD_SD[j]<- 85.9;
GD_N1_val[j]<- 260; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 97.6; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- 279; GD_N1_inval_PSEUD_SD[j]<- 113;
Total_N1_val[j]<- 330; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 177; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- 365; Total_N1_inval_PSEUD_SD[j]<- 189;
SFD_N1_val[j]<- 240; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 77; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- 254; SFD_N1_inval_PSEUD_SD[j]<- 82; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 48
####
#
j=48;

ID[j]<-j
Paper[j]<- "Henderson & Ferreira (1990)"
Language[j]<-'English'
N[j]<- 12
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 230; FFD_N1_inval[j]<-NA  # Table 
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 232; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 257; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 262; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 49
####
#
j=49;

ID[j]<-j
Paper[j]<- "Sereno & Rayner (2000)"
Language[j]<-'English'
N[j]<- 32
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- ((267 + 257)/2 + (283 + 260)/2)/2; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- ((292 + 283)/2 + (294 + 277)/2)/2; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- ((281 + 273)/2 + (306 + 278)/2)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- ((320 + 303)/2 + (326 + 296)/2)/2; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 50
####
#
j=50;

ID[j]<-j
Paper[j]<- "Inhoff, Starr, & Shindler (2000)"
Language[j]<-'English'
N[j]<- 20
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 261; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<- 40.2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 343; FFD_N1_inval_RAN_SD[j]<- 89.4;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 309; FFD_N1_inval_UNREL_SD[j]<- 58.1; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 276; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 44.7; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 367; GD_N1_inval_RAN_SD[j]<- 80.4;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 336; GD_N1_inval_UNREL_SD[j]<- 53.6;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 297; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 49.1; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- 436; Total_N1_inval_RAN_SD[j]<- 116.2;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- 378; Total_N1_inval_UNREL_SD[j]<- 67.1;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 261; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 35.7; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- 351; SFD_N1_inval_RAN_SD[j]<- 89.4; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- 311; SFD_N1_inval_UNREL_SD[j]<- 53.6; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 51
####
#
j=51;

ID[j]<-j
Paper[j]<- "Kennison & Clifton (1995)"
Language[j]<-'English'
N[j]<-48
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (275 + 290)/2; FFD_N1_inval[j]<-NA  # Table 5
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- (298 + 313)/2; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (313 + 346)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- (350 + 363)/2; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 52
####
#
j=52;

ID[j]<-j
Paper[j]<- "Johnson & Dunne (2012)"
Language[j]<-'English'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 274; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<- 20.2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 295; FFD_N1_inval_ORTH_SD[j]<- 30.6;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 284; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 23; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 308; GD_N1_inval_ORTH_SD[j]<- 39.9;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 277; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 20.2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- 300; SFD_N1_inval_ORTH_SD[j]<- 34.5; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 53
####
#
j=53;

ID[j]<-j
Paper[j]<- "Johnson & Dunne (2012), Exp.2"
Language[j]<-'English'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 276; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<- 29.5; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- (300 + 302)/2; FFD_N1_inval_ORTH_SD[j]<- (39.9 + 43.8)/2;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 290; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 33.9; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- (313 + 317)/2; GD_N1_inval_ORTH_SD[j]<- (52.5 + 47.1)/2;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 286; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 38.8; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- (303 + 308)/2; SFD_N1_inval_ORTH_SD[j]<- (41 + 45.4)/2; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 54
####
#
j=54;

ID[j]<-j
Paper[j]<- "Yang, Li, Wang, Slattery, & Rayner (2014)"
Language[j]<-'Chinese'
N[j]<- 39
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 262; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 37.4; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- (262 + 278)/2; FFD_N1_inval_UNREL_SD[j]<- (37.4 + 43.7)/2; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 321; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 74.9; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- (346 + 374)/2; GD_N1_inval_UNREL_SD[j]<- (81.1 + 81.1)/2;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 271; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 43.7; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- (267 + 292)/2; SFD_N1_inval_UNREL_SD[j]<- (49.9 + 62.4)/2; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 55
####
#
j=55;

ID[j]<-j
Paper[j]<- "Gu, Li, & Liversedge (2015)"
Language[j]<-'Chinese'
N[j]<- 48
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (287 + 292)/2; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<- (48.4 + 48.4)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- (326 + 323)/2; FFD_N1_inval_PSEUD_SD[j]<-(55.4 + 55.4)/2;
GD_N1_val[j]<- (360 + 355)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (124.7 + 97)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- (433 + 427)/2; GD_N1_inval_PSEUD_SD[j]<- (117.7 + 97)/2;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 56
####
#
j=56;

ID[j]<-j
Paper[j]<- "Schotter, Lee, Reiderman, & Rayner (2015), Exp.2"
Language[j]<-'English'
N[j]<- 72
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (228 + 223)/2; FFD_N1_inval[j]<-NA  # Table 5
FFD_N1_val_SD[j]<- (34.7 + 29.6)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- (242 + 241)/2; FFD_N1_inval_UNREL_SD[j]<- (39.8 + 38.1)/2; 
FFD_N1_inval_SEM[j]<- (238 + 232)/2; FFD_N1_inval_SEM_SD[j]<- (35.6 + 37.3)/2;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (262 + 252)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (46.6 + 42.4)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- (276 + 276)/2; GD_N1_inval_UNREL_SD[j]<- (53.4 + 50.1)/2;
GD_N1_inval_SEM[j]<- (276 + 263)/2; GD_N1_inval_SEM_SD[j]<- (56 + 47.5)/2;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- (296 + 272)/2; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- (61.1 + 52.6)/2; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- (326 + 306)/2; Total_N1_inval_UNREL_SD[j]<- (74.6 + 61.9)/2;
Total_N1_inval_SEM[j]<- (331 + 300)/2; Total_N1_inval_SEM_SD[j]<- (93.3 + 62.7)/2;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- (235 + 225)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- (38.1 + 33.9)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- (251 + 252)/2; SFD_N1_inval_UNREL_SD[j]<- (50.1 + 46.6)/2; 
SFD_N1_inval_SEM[j]<- (247 + 239)/2; SFD_N1_inval_SEM_SD[j]<- (43.2 + 44.1)/2;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 57
####
#
j=57;

ID[j]<-j
Paper[j]<- "McDonald (2006)"
Language[j]<-'English'
N[j]<- 16
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 260; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<- 28; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 282; FFD_N1_inval_RAN_SD[j]<- 39;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 286; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 35; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 321; GD_N1_inval_RAN_SD[j]<- 45;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 415; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 107; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- 449; Total_N1_inval_RAN_SD[j]<- 95;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 58
####
#
j=58;

ID[j]<-j
Paper[j]<- "Inhoff, Starr, Liu, & Wang (1998)"
Language[j]<-'English'
N[j]<- 15 # because only half of participants did the experiment on a CRT monitor
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 257; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 42.6; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 285; FFD_N1_inval_RAN_SD[j]<- 34.8;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 289; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 58.1; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 331; GD_N1_inval_RAN_SD[j]<- 42.6;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA

####
# 59
####
#
j=59;

ID[j]<-j
Paper[j]<- "Reingold, Reichle, Glaholt, & Sheridan (2012)"
Language[j]<-'English'
N[j]<- 60
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (214 + 234)/2; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- (29.4 + 35.6)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- (251 + 260)/2; FFD_N1_inval_PSEUD_SD[j]<- (45.7 + 53.4)/2;
GD_N1_val[j]<- (247 + 305)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (44.9 + 85.9)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- (309 + 356)/2; GD_N1_inval_PSEUD_SD[j]<- (67.3 + 103)/2;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- (216 + 239)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- (30.9 + 39.5)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- (267 + 276)/2; SFD_N1_inval_PSEUD_SD[j]<- (56.5 + 64.2)/2; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 60
####
#
j=60;

ID[j]<-j
Paper[j]<- "Payne & Stine–Morrow (2012)"
Language[j]<-'English'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 222; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 44.1; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 252; FFD_N1_inval_RAN_SD[j]<- 44.1;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 266; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 78.3; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 307; GD_N1_inval_RAN_SD[j]<- 78.3;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 224; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-53.8; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- 263; SFD_N1_inval_RAN_SD[j]<- 53.8; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 61
####
#
j=61;

ID[j]<-j
Paper[j]<- "Masserang & Pollatsek (2012)"
Language[j]<-'English'
N[j]<- 42-10 # 10 participants were excluded
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (253 + 240)/2; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- (270 + 259)/2; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (322 + 300)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- (334 + 350)/2; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- (465 + 405)/2; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- (435 + 420)/2; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 62
####
#
j=62;

ID[j]<-j
Paper[j]<- "Masserang & Pollatsek (2012), Exp.2"
Language[j]<-'English'
N[j]<- 58
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (239 + 244)/2; FFD_N1_inval[j]<-NA  # Table 4
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- (263 + 254)/2; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (331 + 298)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- (371 + 327)/2; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- (464 + 366)/2; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- (472 + 412)/2; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 63
####
#
j=63;

ID[j]<-j
Paper[j]<- "Yang (2013)"
Language[j]<-'Chinese'
N[j]<- 48
  #N
  FFD_N_val[j]<- 228; FFD_N_inval[j]<-NA  # Table 2
FFD_N_val_SD[j]<- 32; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- 228; FFD_N_inval_UNREL_SD[j]<- 37;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 257; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 56; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- 264; GD_N_inval_ORTH_SD[j]<- 60; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 241; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 41; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 268; FFD_N1_inval_UNREL_SD[j]<- 48; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 280; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 69; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 328; GD_N1_inval_UNREL_SD[j]<- 75;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 64
####
#
j=64;

ID[j]<-j
Paper[j]<- "Yang (2013), Exp.2"
Language[j]<-'Chinese'
N[j]<- 54
  #N
  FFD_N_val[j]<- 237; FFD_N_inval[j]<-NA  # Table 4
FFD_N_val_SD[j]<- 46; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- 233; FFD_N_inval_UNREL_SD[j]<- 40;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<- 274; GD_N_inval[j]<-NA
GD_N_val_SD[j]<- 66; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- 268; GD_N_inval_UNREL_SD[j]<- 55; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 247; FFD_N1_inval[j]<-NA  # Table 4
FFD_N1_val_SD[j]<- 37; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 277; FFD_N1_inval_UNREL_SD[j]<- 53; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 282; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 57; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 331; GD_N1_inval_UNREL_SD[j]<- 87;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 65
####
#
j=65;

ID[j]<-j
Paper[j]<- "Choi & Gordon (2014)"
Language[j]<-'English'
N[j]<- 48
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 220; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<- 36; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 228; FFD_N1_inval_ORTH_SD[j]<- 43;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- 235; FFD_N1_inval_PHON_SD[j]<- 31;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 242; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 43; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 257; GD_N1_inval_ORTH_SD[j]<- 48;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- 263; GD_N1_inval_PHON_SD[j]<- 60;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 300; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 101; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- 324; Total_N1_inval_ORTH_SD[j]<- 109;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- 337; Total_N1_inval_PHON_SD[j]<- 118;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 219; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 35; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- 224; SFD_N1_inval_ORTH_SD[j]<- 40; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- 229; SFD_N1_inval_PHON_SD[j]<- 41; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 66
####
#
j=66;

ID[j]<-j
Paper[j]<- "Yan, Luo, & Inhoff (2014)"
Language[j]<-'Chinese'
N[j]<- 36
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (262 + 258)/2; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<- (32 + 34)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- (258 + 269)/2; FFD_N1_inval_PSEUD_SD[j]<- (38 + 36)/2;
GD_N1_val[j]<- (288 + 284)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (45 + 39)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- (282 + 297)/2; GD_N1_inval_PSEUD_SD[j]<- (44 + 51)/2;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- (261 + 257)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- (32 + 34)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- (259 + 269)/2; SFD_N1_inval_PSEUD_SD[j]<- (41 + 39)/2; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 67
####
#
j=67;

ID[j]<-j
Paper[j]<- "Williams, Perea, Pollatsek, & Rayner (2006)"
Language[j]<-'English'
N[j]<- 18
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 258; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<- 45.6; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- (260 + 286)/2; FFD_N1_inval_ORTH_SD[j]<- (47.9 + 44.8)/2;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 273; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 65.07; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- (287 + 308)/2; GD_N1_inval_ORTH_SD[j]<- (64.4+64.6)/2;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 311; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 138.09; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- (351 + 359)/2; Total_N1_inval_ORTH_SD[j]<- (123.2+90.2)/2;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 266; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 55.7; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- (265 + 295)/2; SFD_N1_inval_ORTH_SD[j]<- (52.4 + 55.6)/2; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 68
####
#
j=68;

ID[j]<-j
Paper[j]<- "Williams, Perea, Pollatsek, & Rayner (2006), Exp.2"
Language[j]<- 'English'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 257; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 48.9; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- (283 + 285)/2; FFD_N1_inval_ORTH_SD[j]<- (43.6 + 59.9)/2;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 268; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 43.9; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- (291 + 293)/2; GD_N1_inval_ORTH_SD[j]<- (47+61.9)/2;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 278; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 51.7; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- (308 + 319)/2; Total_N1_inval_ORTH_SD[j]<- (57.8+78.2)/2;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 257; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 50.8; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- (287 + 285)/2; SFD_N1_inval_ORTH_SD[j]<- (50.5+62.6)/2; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 69
####
#
j=69;

ID[j]<-j
Paper[j]<- "Kambe (2004)"
Language[j]<-'English'
N[j]<- 36
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (255 + 264 + 268)/3; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- (283 + 283 + 289)/3; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (286 + 303 + 289)/3; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- (313 + 320 + 320)/3; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 70
####
#
j=70;

ID[j]<-j
Paper[j]<- "Kambe (2004), Exp.2"
Language[j]<-'English'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<-  (264 + 266 + 268)/3; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- (310 + 307 + 313)/3; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (308 + 299 + 296)/3; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- (354 + 349 + 358)/3; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 71
####
#
j=71;

ID[j]<-j
Paper[j]<- "Deutsch, Frost, Pelleg, Pollatsek, & Rayner (2003)"
Language[j]<-'Hebrew'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 225; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 23; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 238; FFD_N1_inval_ORTH_SD[j]<- 30;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 257; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 38; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 279; GD_N1_inval_ORTH_SD[j]<- 55;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- 372; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 90; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- 400; Total_N1_inval_ORTH_SD[j]<- 92;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 72
####
#
j=72;

ID[j]<-j
Paper[j]<- "Deutsch, Frost, Pollatsek, & Rayner (2005)"
Language[j]<-'Hebrew'
N[j]<- 36
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 225; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<- 14.7; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 241; FFD_N1_inval_ORTH_SD[j]<- 22.5;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 274; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 22; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 295; GD_N1_inval_ORTH_SD[j]<- 25.3;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 73
####
#
j=73;

ID[j]<-j
Paper[j]<- "Miller, Juhasz, & Rayner (2006)"
Language[j]<-'English'
N[j]<- 42
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (270 + 269)/2; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- (41 + 39)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- (291 + 292)/2; FFD_N1_inval_RAN_SD[j]<- (42 + 50)/2;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (339 + 340)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (83 + 72)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- (373 + 363)/2; GD_N1_inval_RAN_SD[j]<- (77 + 71)/2;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- (288 + 289)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- (50 + 49)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- (340 + 323)/2; SFD_N1_inval_RAN_SD[j]<- (86 + 58)/2; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 74
####
#
j=74;

ID[j]<-j
Paper[j]<- "Miller, Juhasz, & Rayner (2006),Exp.2"
Language[j]<-'English'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (303 + 288)/2; FFD_N1_inval[j]<-NA  # Table 5
FFD_N1_val_SD[j]<- (46 + 43)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- (341 + 342)/2; FFD_N1_inval_X_SD[j]<- (48 + 49)/2; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (335 + 318)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (45 + 53)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- (391 + 396)/2; GD_N1_inval_X_SD[j]<- (48 + 56)/2;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- (306 + 299)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- (49 + 46)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- (365 + 358)/2; SFD_N1_inval_X_SD[j]<- (53 + 50)/2;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 75
####
#
j=75;

ID[j]<-j
Paper[j]<- "Dare & Shillcock (2013), Exp.2"
Language[j]<-'English'
N[j]<- 30
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 236; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 29.7; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 277.8; FFD_N1_inval_UNREL_SD[j]<- 39; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 262.4; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 39; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 312.4; GD_N1_inval_UNREL_SD[j]<- 57.4;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 76
####
#
j=76;

ID[j]<-j
Paper[j]<- "Wakeford & Murray (n.d)"
Language[j]<-'English'
N[j]<- 28
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 267; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- 277; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 298; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- 299; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 77
####
#
j=77;

ID[j]<-j
Paper[j]<- "Wang & Inhoff (2010)"
Language[j]<-'English'
N[j]<- 25
#N
FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 250; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- 50; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- 283; FFD_N1_inval_PSEUD_SD[j]<- 45;
GD_N1_val[j]<- 313; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 70; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- 367; GD_N1_inval_PSEUD_SD[j]<- 60;
Total_N1_val[j]<- 333; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 75; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- 381; Total_N1_inval_PSEUD_SD[j]<- 55;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 78
####
#
j=78;

ID[j]<-j
Paper[j]<- "Wang & Inhoff (2010), Exp.2"
Language[j]<-'English'
N[j]<- 16
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 266; FFD_N1_inval[j]<-NA  # Table 4
FFD_N1_val_SD[j]<- 48; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- 298; FFD_N1_inval_PSEUD_SD[j]<- 48;
GD_N1_val[j]<- 317; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 60; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- 351; GD_N1_inval_PSEUD_SD[j]<- 44;
Total_N1_val[j]<- 331; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 64; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- 367; Total_N1_inval_PSEUD_SD[j]<- 52;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 79
####
#
j=79;

ID[j]<-j
Paper[j]<- "Wang & Inhoff (2013)"
Language[j]<-'English'
N[j]<- 29
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 257; FFD_N1_inval[j]<-NA  # No table, results reported in text
FFD_N1_val_SD[j]<- 27.3; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- 286; FFD_N1_inval_RAN_SD[j]<- 27.3;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 274; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-31.07; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- 315; GD_N1_inval_RAN_SD[j]<- 31.07;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-306; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 38.8; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- 347; Total_N1_inval_RAN_SD[j]<- 38.8;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 80
####
#
j=80;

ID[j]<-j
Paper[j]<- "Tiffin-Richards & Schroeder (2015)"
Language[j]<-'German'
N[j]<- 23 # 1 participant was excluded
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 212; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<- 33.5; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 219; FFD_N1_inval_ORTH_SD[j]<- 38.3;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- 223; FFD_N1_inval_PHON_SD[j]<- 38.3;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 221; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 47.9; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 234; GD_N1_inval_ORTH_SD[j]<- 47.9;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- 236; GD_N1_inval_PHON_SD[j]<- 47.9;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 215; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- 38.3; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- 229; SFD_N1_inval_ORTH_SD[j]<- 43.1; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- 226; SFD_N1_inval_PHON_SD[j]<- 43.1; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 81
####
#
j=81;

ID[j]<-j
Paper[j]<- "Shahid (2014), Exp.4"
Language[j]<-'English'
N[j]<- 80
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- ((204 + 210)/2 + (205 + 213)/2)/2 ; FFD_N1_inval[j]<-NA  # Table 5.5
FFD_N1_val_SD[j]<- ((28 + 28)/2 + (30 + 30)/2)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- ((244 + 251)/2 + (254 + 258)/2)/2; FFD_N1_inval_PSEUD_SD[j]<- ((42 + 46)/2 + (45 + 46)/2)/2;
GD_N1_val[j]<- ((212 + 219)/2 + (213 + 224)/2)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- ((33 + 33)/2 + (36 + 36)/2)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- ((269 + 276)/2 + (285 + 289)/2)/2; GD_N1_inval_PSEUD_SD[j]<- ((59 + 70)/2 + (62 + 68)/2)/2;
Total_N1_val[j]<- ((234 + 246)/2 + (237 + 255)/2)/2; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- ((43 + 46)/2 + (48 + 47)/2)/2; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- ((303 + 317)/2 + (320 + 339)/2)/2; Total_N1_inval_PSEUD_SD[j]<- ((77 + 77)/2 + (79 + 92)/2)/2;
SFD_N1_val[j]<-((205 + 211)/2 + (206 + 215)/2)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- ((30 + 28)/2 + (31 + 31)/2)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- ((252 + 262)/2 + (265 + 272)/2)/2; SFD_N1_inval_PSEUD_SD[j]<- ((50 + 60)/2 + (56 + 60)/2)/2; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 82
####
#
j=82;

ID[j]<-j
Paper[j]<- "Cui, Drieghe, Bai, Yan, & Liversedge (2014)"
Language[j]<-'Chinese'
N[j]<- 96 # 7 were excluded
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (233 + 224)/2; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<- (42 + 43)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- (245 + 247)/2; FFD_N1_inval_PSEUD_SD[j]<- (44 + 37)/2;
GD_N1_val[j]<- (242 + 280)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (51 + 62)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- (267 + 325)/2; GD_N1_inval_PSEUD_SD[j]<- (52 + 59)/2;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 83
####
#
j=83;

ID[j]<-j
Paper[j]<- "Winskel (2011)"
Language[j]<-'Thai'
N[j]<- 36
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 247; FFD_N1_inval[j]<-NA  # Table 2
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- 256; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- 261; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 330; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- 367; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- 384; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 84
####
#
j=84;

ID[j]<-j
Paper[j]<- "Wang, Inhoff, & Radach (2009)"
Language[j]<-'English'
N[j]<- 44
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- 271; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<- 46.4; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- 310; FFD_N1_inval_PSEUD_SD[j]<- 53.06;
GD_N1_val[j]<- 291; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 53.06; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- 341; GD_N1_inval_PSEUD_SD[j]<- 59.6;
Total_N1_val[j]<- 319; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- 66.3; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- 369; Total_N1_inval_PSEUD_SD[j]<- 59.6;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 85
####
#
j=85;

ID[j]<-j
Paper[j]<- "Plummer & Rayner (2012)"
Language[j]<-'English'
N[j]<- 36
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (249 + 249)/2; FFD_N1_inval[j]<-NA  # Table 5
FFD_N1_val_SD[j]<- (28.8 + 31.2)/2; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- ((270 + 271)/2 + (267 + 265)/2)/2; FFD_N1_inval_ORTH_SD[j]<- ((33 + 31.8)/2 + (28.8 + 28.2)/2)/2;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (282 + 303)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- (39.6 + 40.8)/2; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- ((298 + 309)/2 + (320 + 330)/2)/2; GD_N1_inval_ORTH_SD[j]<- ((38.4 + 38.4)/2 + (39.6 + 46.2)/2)/2;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<- (384 + 423)/2; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<- (67.2 + 81.6)/2; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- ((407 + 457)/2 + (402 + 496)/2)/2; Total_N1_inval_ORTH_SD[j]<- ((80.4 + 86.4)/2 + (67.2 + 100.2)/2)/2;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- (256 + 266)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<- (31.8 + 38.4)/2; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- ((281 + 285)/2 + (278 + 282)/2)/2; SFD_N1_inval_ORTH_SD[j]<- ((37.2 + 37.2)/2 + (33 + 34.8)/2)/2; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 86
####
#
j=86;

ID[j]<-j
Paper[j]<- "Drieghe, Rayner, & Pollatsek (2005)"
Language[j]<-'English'
N[j]<- 24
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- NA; FFD_N1_inval[j]<-NA  # Table 3
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- NA; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- 262; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- 305; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- 279; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- 305; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 87
####
#
j=87;

ID[j]<-j
Paper[j]<- "Liu, Inhoff, Ye, & Wu (2002), Exp.2"
Language[j]<-'Chinese'
N[j]<- 27-7 # 7 participants were excluded
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- NA; FFD_N1_inval[j]<-NA  # Table 4 ()
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- 338; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<- 58.1; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- 356; GD_N1_inval_PHON_SD[j]<- 71.5;
GD_N1_inval_PSEUD[j]<- 388; GD_N1_inval_PSEUD_SD[j]<- 76.02;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA


####
# 88
####
#
j=88;

ID[j]<-j
Paper[j]<- "Winskel & Perea (2013)"
Language[j]<-'Thai'
N[j]<- 33
  #N
  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
GD_N_val[j]<-NA; GD_N_inval[j]<-NA
GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
Total_N_val[j]<-NA; Total_N_inval[j]<-NA
Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
FFD_N1_val[j]<- (234 + 239)/2; FFD_N1_inval[j]<-NA  # Table 1
FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
FFD_N1_inval_ORTH[j]<- (247 + 254)/2; FFD_N1_inval_ORTH_SD[j]<- NA;
FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
GD_N1_val[j]<- (294 + 291)/2; GD_N1_inval[j]<-NA
GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
GD_N1_inval_ORTH[j]<- (343 + 319)/2; GD_N1_inval_ORTH_SD[j]<- NA;
GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
SFD_N1_val[j]<- (241 + 241)/2; SFD_N1_inval[j]<-NA
SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
SFD_N1_inval_ORTH[j]<- (251 + 261)/2; SFD_N1_inval_ORTH_SD[j]<- NA; 
SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA



####
# 89
####
#
#j=89;
#
#ID[j]<-j
#Paper[j]<- ""
#Language[j]<-''
#N[j]<- 
#  #N
#  FFD_N_val[j]<-NA; FFD_N_inval[j]<-NA  # Table 
#FFD_N_val_SD[j]<-NA; FFD_N_inval_SD[j]<-NA
#FFD_N_inval_RAN[j]<- NA; FFD_N_inval_RAN_SD[j]<- NA;
#FFD_N_inval_X[j]<- NA; FFD_N_inval_X_SD[j]<- NA;
#FFD_N_inval_ORTH[j]<- NA; FFD_N_inval_ORTH_SD[j]<- NA;
#FFD_N_inval_UNREL[j]<- NA; FFD_N_inval_UNREL_SD[j]<- NA;
#FFD_N_inval_SEM[j]<- NA; FFD_N_inval_SEM_SD[j]<- NA;
#FFD_N_inval_PHON[j]<- NA; FFD_N_inval_PHON_SD[j]<- NA;
#FFD_N_inval_PSEUD[j]<- NA; FFD_N_inval_PSEUD_SD[j]<- NA;
#GD_N_val[j]<-NA; GD_N_inval[j]<-NA
#GD_N_val_SD[j]<-NA; GD_N_inval_SD[j]<-NA
#GD_N_inval_RAN[j]<- NA; GD_N_inval_RAN_SD[j]<- NA; 
#GD_N_inval_X[j]<- NA; GD_N_inval_X_SD[j]<- NA; 
#GD_N_inval_ORTH[j]<- NA; GD_N_inval_ORTH_SD[j]<- NA; 
#GD_N_inval_UNREL[j]<- NA; GD_N_inval_UNREL_SD[j]<- NA; 
#GD_N_inval_SEM[j]<- NA; GD_N_inval_SEM_SD[j]<- NA; 
#GD_N_inval_PHON[j]<- NA; GD_N_inval_PHON_SD[j]<- NA;
#GD_N_inval_PSEUD[j]<- NA; GD_N_inval_PSEUD_SD[j]<- NA;
#Total_N_val[j]<-NA; Total_N_inval[j]<-NA
#Total_N_val_SD[j]<-NA; Total_N_inval_SD[j]<-NA
#Total_N_inval_RAN[j]<- NA; Total_N_inval_RAN_SD[j]<- NA;
#Total_N_inval_X[j]<- NA; Total_N_inval_X_SD[j]<- NA;
#Total_N_inval_ORTH[j]<- NA; Total_N_inval_ORTH_SD[j]<- NA;
#Total_N_inval_UNREL[j]<- NA; Total_N_inval_UNREL_SD[j]<- NA;
#Total_N_inval_SEM[j]<- NA; Total_N_inval_SEM_SD[j]<- NA;
#Total_N_inval_PHON[j]<- NA; Total_N_inval_PHON_SD[j]<- NA;
#Total_N_inval_PSEUD[j]<- NA; Total_N_inval_PSEUD_SD[j]<- NA;
#SFD_N_val[j]<-NA; SFD_N_inval[j]<-NA
#SFD_N_val_SD[j]<-NA; SFD_N_inval_SD[j]<-NA
#SFD_N_inval_RAN[j]<- NA; SFD_N_inval_RAN_SD[j]<- NA;
#SFD_N_inval_X[j]<- NA; SFD_N_inval_X_SD[j]<- NA;
#SFD_N_inval_ORTH[j]<- NA; SFD_N_inval_ORTH_SD[j]<- NA;
#SFD_N_inval_UNREL[j]<- NA; SFD_N_inval_UNREL_SD[j]<- NA;
#SFD_N_inval_SEM[j]<- NA; SFD_N_inval_SEM_SD[j]<- NA;
#SFD_N_inval_PHON[j]<- NA; SFD_N_inval_PHON_SD[j]<- NA;
#SFD_N_inval_PSEUD[j]<- NA; SFD_N_inval_PSEUD_SD[j]<- NA;
#FixProb_N_val[j]<-NA; FixProb_N_inval[j]<-NA
#FixProb_N_val_SD[j]<-NA; FixProb_N_inval_SD[j]<-NA
#LandPos_N_val[j]<- NA; LandPos_N_inval[j]<- NA
#LandPos_N_val_SD[j]<- NA; LandPos_N_inval_SD[j]<- NA

# N+1 
#FFD_N1_val[j]<- NA; FFD_N1_inval[j]<-NA  # Table 
#FFD_N1_val_SD[j]<-NA; FFD_N1_inval_SD[j]<-NA
#FFD_N1_inval_RAN[j]<- NA; FFD_N1_inval_RAN_SD[j]<- NA;
#FFD_N1_inval_X[j]<- NA; FFD_N1_inval_X_SD[j]<- NA; 
#FFD_N1_inval_ORTH[j]<- NA; FFD_N1_inval_ORTH_SD[j]<- NA;
#FFD_N1_inval_UNREL[j]<- NA; FFD_N1_inval_UNREL_SD[j]<- NA; 
#FFD_N1_inval_SEM[j]<- NA; FFD_N1_inval_SEM_SD[j]<- NA;
#FFD_N1_inval_PHON[j]<- NA; FFD_N1_inval_PHON_SD[j]<- NA;
#FFD_N1_inval_PSEUD[j]<- NA; FFD_N1_inval_PSEUD_SD[j]<- NA;
#GD_N1_val[j]<- NA; GD_N1_inval[j]<-NA
#GD_N1_val_SD[j]<-NA; GD_N1_inval_SD[j]<-NA
#GD_N1_inval_RAN[j]<- NA; GD_N1_inval_RAN_SD[j]<- NA;
#GD_N1_inval_X[j]<- NA; GD_N1_inval_X_SD[j]<- NA;
#GD_N1_inval_ORTH[j]<- NA; GD_N1_inval_ORTH_SD[j]<- NA;
#GD_N1_inval_UNREL[j]<- NA; GD_N1_inval_UNREL_SD[j]<- NA;
#GD_N1_inval_SEM[j]<- NA; GD_N1_inval_SEM_SD[j]<- NA;
#GD_N1_inval_PHON[j]<- NA; GD_N1_inval_PHON_SD[j]<- NA;
#GD_N1_inval_PSEUD[j]<- NA; GD_N1_inval_PSEUD_SD[j]<- NA;
#Total_N1_val[j]<-NA; Total_N1_inval[j]<-NA
#Total_N1_val_SD[j]<-NA; Total_N1_inval_SD[j]<-NA
#Total_N1_inval_RAN[j]<- NA; Total_N1_inval_RAN_SD[j]<- NA;
#Total_N1_inval_X[j]<- NA; Total_N1_inval_X_SD[j]<- NA;
#Total_N1_inval_ORTH[j]<- NA; Total_N1_inval_ORTH_SD[j]<- NA;
#Total_N1_inval_UNREL[j]<- NA; Total_N1_inval_UNREL_SD[j]<- NA;
#Total_N1_inval_SEM[j]<- NA; Total_N1_inval_SEM_SD[j]<- NA;
#Total_N1_inval_PHON[j]<- NA; Total_N1_inval_PHON_SD[j]<- NA;
#Total_N1_inval_PSEUD[j]<- NA; Total_N1_inval_PSEUD_SD[j]<- NA;
#SFD_N1_val[j]<-NA; SFD_N1_inval[j]<-NA
#SFD_N1_val_SD[j]<-NA; SFD_N1_inval_SD[j]<-NA
#SFD_N1_inval_RAN[j]<- NA; SFD_N1_inval_RAN_SD[j]<- NA; 
#SFD_N1_inval_X[j]<- NA; SFD_N1_inval_X_SD[j]<- NA;
#SFD_N1_inval_ORTH[j]<- NA; SFD_N1_inval_ORTH_SD[j]<- NA; 
#SFD_N1_inval_UNREL[j]<- NA; SFD_N1_inval_UNREL_SD[j]<- NA; 
#SFD_N1_inval_SEM[j]<- NA; SFD_N1_inval_SEM_SD[j]<- NA;
#SFD_N1_inval_PHON[j]<- NA; SFD_N1_inval_PHON_SD[j]<- NA; 
#SFD_N1_inval_PSEUD[j]<- NA; SFD_N1_inval_PSEUD_SD[j]<- NA; 
#FixProb_N1_val[j]<-NA; FixProb_N1_inval[j]<-NA
#FixProb_N1_val_SD[j]<-NA; FixProb_N1_inval_SD[j]<-NA
#LandPos_N1_val[j]<- NA; LandPos_N1_inval[j]<- NA
#LandPos_N1_val_SD[j]<- NA; LandPos_N1_inval_SD[j]<- NA



#############################################################################################
# Merge variables into a data matrix:
dataN1<- data.frame(ID, Paper, Language, N, FFD_N_val, FFD_N_val_SD, FFD_N_inval,
                  FFD_N_inval_SD, FFD_N_inval_RAN, FFD_N_inval_RAN_SD,FFD_N_inval_X, FFD_N_inval_X_SD,
                  FFD_N_inval_ORTH, FFD_N_inval_ORTH_SD,FFD_N_inval_UNREL, FFD_N_inval_UNREL_SD,
                  FFD_N_inval_SEM, FFD_N_inval_SEM_SD, FFD_N_inval_PHON, FFD_N_inval_PHON_SD,
                  FFD_N_inval_PSEUD, FFD_N_inval_PSEUD_SD, GD_N_val, GD_N_val_SD,GD_N_inval,
                  GD_N_inval_SD, GD_N_inval_RAN, GD_N_inval_RAN_SD,
                  GD_N_inval_X, GD_N_inval_X_SD, GD_N_inval_ORTH, GD_N_inval_ORTH_SD,
                  GD_N_inval_UNREL, GD_N_inval_UNREL_SD, GD_N_inval_SEM, GD_N_inval_SEM_SD,
                  GD_N_inval_PHON, GD_N_inval_PHON_SD, GD_N_inval_PSEUD, GD_N_inval_PSEUD_SD,
                  Total_N_val, Total_N_val_SD, Total_N_inval,Total_N_inval_SD,
                  Total_N_inval_RAN, Total_N_inval_RAN_SD, Total_N_inval_X, Total_N_inval_X_SD,
                  Total_N_inval_ORTH, Total_N_inval_ORTH_SD,Total_N_inval_UNREL,
                  Total_N_inval_UNREL_SD, Total_N_inval_SEM, Total_N_inval_SEM_SD,
                  Total_N_inval_PHON, Total_N_inval_PHON_SD, Total_N_inval_PSEUD, Total_N_inval_PSEUD_SD,
                  SFD_N_val, SFD_N_inval, SFD_N_val_SD,SFD_N_inval_SD, SFD_N_inval_RAN, SFD_N_inval_RAN_SD,
                  SFD_N_inval_X, SFD_N_inval_X_SD, SFD_N_inval_ORTH, SFD_N_inval_ORTH_SD,
                  SFD_N_inval_UNREL, SFD_N_inval_UNREL_SD, SFD_N_inval_SEM, SFD_N_inval_SEM_SD,
                  SFD_N_inval_PHON, SFD_N_inval_PHON_SD, SFD_N_inval_PSEUD, SFD_N_inval_PSEUD_SD,
                  FixProb_N_val, FixProb_N_val_SD, FixProb_N_inval,
                  FixProb_N_inval_SD,LandPos_N_val, LandPos_N_val_SD, LandPos_N_inval, LandPos_N_inval_SD, 
                  FFD_N1_val, FFD_N1_val_SD, FFD_N1_inval, FFD_N1_inval_SD, FFD_N1_inval_RAN, 
                  FFD_N1_inval_RAN_SD, FFD_N1_inval_X, FFD_N1_inval_X_SD, FFD_N1_inval_ORTH, FFD_N1_inval_ORTH_SD,
                  FFD_N1_inval_UNREL, FFD_N1_inval_UNREL_SD, FFD_N1_inval_SEM, FFD_N1_inval_SEM_SD, 
                  FFD_N1_inval_PHON, FFD_N1_inval_PHON_SD, FFD_N1_inval_PSEUD, FFD_N1_inval_PSEUD_SD,
                  GD_N1_val, GD_N1_val_SD,GD_N1_inval,GD_N1_inval_SD,
                  GD_N1_inval_RAN, GD_N1_inval_RAN_SD, GD_N1_inval_X, GD_N1_inval_X_SD,
                  GD_N1_inval_ORTH, GD_N1_inval_ORTH_SD, GD_N1_inval_UNREL, GD_N1_inval_UNREL_SD,
                  GD_N1_inval_SEM, GD_N1_inval_SEM_SD, GD_N1_inval_PHON, GD_N1_inval_PHON_SD,
                  GD_N1_inval_PSEUD, GD_N1_inval_PSEUD_SD,
                  Total_N1_val, Total_N1_val_SD, Total_N1_inval,Total_N1_inval_SD, Total_N1_inval_RAN,
                  Total_N1_inval_RAN_SD, Total_N1_inval_X,
                  Total_N1_inval_X_SD, Total_N1_inval_ORTH,
                  Total_N1_inval_ORTH_SD, Total_N1_inval_UNREL,
                  Total_N1_inval_UNREL_SD, Total_N1_inval_SEM,
                  Total_N1_inval_SEM_SD, Total_N1_inval_PHON,
                  Total_N1_inval_PHON_SD, Total_N1_inval_PSEUD,
                  Total_N1_inval_PSEUD_SD, SFD_N1_val, SFD_N1_inval, SFD_N1_val_SD,
                  SFD_N1_inval_SD,
                  SFD_N1_inval_RAN, SFD_N1_inval_RAN_SD, SFD_N1_inval_X, SFD_N1_inval_X_SD,
                  FixProb_N1_val, SFD_N1_inval_ORTH, SFD_N1_inval_ORTH_SD,
                  SFD_N1_inval_UNREL, SFD_N1_inval_UNREL_SD, SFD_N1_inval_SEM, SFD_N1_inval_SEM_SD,
                  SFD_N1_inval_PHON, SFD_N1_inval_PHON_SD, SFD_N1_inval_PSEUD, SFD_N1_inval_PSEUD_SD,
                  FixProb_N1_val_SD, FixProb_N1_inval,FixProb_N1_inval_SD, LandPos_N1_val, LandPos_N1_val_SD,
                  LandPos_N1_inval, LandPos_N1_inval_SD)

#Save in a data file:
save(dataN1,file="Data/dataN1.Rda")

write.csv(cbind(dataN1[,1:4], dataN1[,85:157]), file = "Data/N1.csv", row.names = TRUE)

rm(ID, N, FFD_N_val, FFD_N_val_SD, FFD_N_inval,
   FFD_N_inval_SD, FFD_N_inval_RAN, FFD_N_inval_RAN_SD,FFD_N_inval_X, FFD_N_inval_X_SD,
   FFD_N_inval_ORTH, FFD_N_inval_ORTH_SD,FFD_N_inval_UNREL, FFD_N_inval_UNREL_SD,
   FFD_N_inval_SEM, FFD_N_inval_SEM_SD, FFD_N_inval_PHON, FFD_N_inval_PHON_SD,
   FFD_N_inval_PSEUD, FFD_N_inval_PSEUD_SD, GD_N_val, GD_N_val_SD,GD_N_inval,
   GD_N_inval_SD, GD_N_inval_RAN, GD_N_inval_RAN_SD,
   GD_N_inval_X, GD_N_inval_X_SD, GD_N_inval_ORTH, GD_N_inval_ORTH_SD,
   GD_N_inval_UNREL, GD_N_inval_UNREL_SD, GD_N_inval_SEM, GD_N_inval_SEM_SD,
   GD_N_inval_PHON, GD_N_inval_PHON_SD, GD_N_inval_PSEUD, GD_N_inval_PSEUD_SD,
   Total_N_val, Total_N_val_SD, Total_N_inval,Total_N_inval_SD,
   Total_N_inval_RAN, Total_N_inval_RAN_SD, Total_N_inval_X, Total_N_inval_X_SD,
   Total_N_inval_ORTH, Total_N_inval_ORTH_SD,Total_N_inval_UNREL,
   Total_N_inval_UNREL_SD, Total_N_inval_SEM, Total_N_inval_SEM_SD,
   Total_N_inval_PHON, Total_N_inval_PHON_SD, Total_N_inval_PSEUD, Total_N_inval_PSEUD_SD,
   SFD_N_val, SFD_N_inval, SFD_N_val_SD,SFD_N_inval_SD, SFD_N_inval_RAN, SFD_N_inval_RAN_SD,
   SFD_N_inval_X, SFD_N_inval_X_SD, SFD_N_inval_ORTH, SFD_N_inval_ORTH_SD,
   SFD_N_inval_UNREL, SFD_N_inval_UNREL_SD, SFD_N_inval_SEM, SFD_N_inval_SEM_SD,
   SFD_N_inval_PHON, SFD_N_inval_PHON_SD, SFD_N_inval_PSEUD, SFD_N_inval_PSEUD_SD,
   FixProb_N_val, FixProb_N_val_SD, FixProb_N_inval,
   FixProb_N_inval_SD,LandPos_N_val, LandPos_N_val_SD, LandPos_N_inval, LandPos_N_inval_SD, 
   FFD_N1_val, FFD_N1_val_SD, FFD_N1_inval, FFD_N1_inval_SD, FFD_N1_inval_RAN, 
   FFD_N1_inval_RAN_SD, FFD_N1_inval_X, FFD_N1_inval_X_SD, FFD_N1_inval_ORTH, FFD_N1_inval_ORTH_SD,
   FFD_N1_inval_UNREL, FFD_N1_inval_UNREL_SD, FFD_N1_inval_SEM, FFD_N1_inval_SEM_SD, 
   FFD_N1_inval_PHON, FFD_N1_inval_PHON_SD, FFD_N1_inval_PSEUD, FFD_N1_inval_PSEUD_SD,
   GD_N1_val, GD_N1_val_SD,GD_N1_inval,GD_N1_inval_SD,
   GD_N1_inval_RAN, GD_N1_inval_RAN_SD, GD_N1_inval_X, GD_N1_inval_X_SD,
   GD_N1_inval_ORTH, GD_N1_inval_ORTH_SD, GD_N1_inval_UNREL, GD_N1_inval_UNREL_SD,
   GD_N1_inval_SEM, GD_N1_inval_SEM_SD, GD_N1_inval_PHON, GD_N1_inval_PHON_SD,
   GD_N1_inval_PSEUD, GD_N1_inval_PSEUD_SD,
   Total_N1_val, Total_N1_val_SD, Total_N1_inval,Total_N1_inval_SD, Total_N1_inval_RAN,
   Total_N1_inval_RAN_SD, Total_N1_inval_X,
   Total_N1_inval_X_SD, Total_N1_inval_ORTH,
   Total_N1_inval_ORTH_SD, Total_N1_inval_UNREL,
   Total_N1_inval_UNREL_SD, Total_N1_inval_SEM,
   Total_N1_inval_SEM_SD, Total_N1_inval_PHON,
   Total_N1_inval_PHON_SD, Total_N1_inval_PSEUD,
   Total_N1_inval_PSEUD_SD, SFD_N1_val, SFD_N1_inval, SFD_N1_val_SD,
   SFD_N1_inval_SD,
   SFD_N1_inval_RAN, SFD_N1_inval_RAN_SD, SFD_N1_inval_X, SFD_N1_inval_X_SD,
   FixProb_N1_val, SFD_N1_inval_ORTH, SFD_N1_inval_ORTH_SD,
   SFD_N1_inval_UNREL, SFD_N1_inval_UNREL_SD, SFD_N1_inval_SEM, SFD_N1_inval_SEM_SD,
   SFD_N1_inval_PHON, SFD_N1_inval_PHON_SD, SFD_N1_inval_PSEUD, SFD_N1_inval_PSEUD_SD,
   FixProb_N1_val_SD, FixProb_N1_inval,FixProb_N1_inval_SD, LandPos_N1_val, LandPos_N1_val_SD,
   LandPos_N1_inval, LandPos_N1_inval_SD)



######################################
# Effect sizes (N+1 preview effects) #
######################################
FFD_ms<-NULL
FFD_es<- NULL
FFD_RAN_es<- NULL
FFD_RAN_ms<- NULL
FFD_X_es<- NULL
FFD_X_ms<- NULL
FFD_ORTH_es<- NULL
FFD_ORTH_ms<- NULL
FFD_UNREL_es<- NULL
FFD_UNREL_ms<- NULL
FFD_SEM_es<- NULL
FFD_SEM_ms<- NULL
FFD_PHON_es<- NULL
FFD_PHON_ms<- NULL
FFD_PSEUD_es<- NULL
FFD_PSEUD_ms<- NULL
GD_ms<- NULL
GD_es<- NULL
GD_RAN_es<- NULL
GD_RAN_ms<- NULL
GD_X_es<- NULL
GD_X_ms<- NULL
GD_ORTH_es<- NULL
GD_ORTH_ms<- NULL
GD_UNREL_es<- NULL
GD_UNREL_ms<- NULL
GD_SEM_es<- NULL
GD_SEM_ms<- NULL
GD_PHON_es<- NULL
GD_PHON_ms<- NULL
GD_PSEUD_es<- NULL
GD_PSEUD_ms<- NULL
Total_ms<-NULL
Total_es<-NULL
Total_RAN_es<-NULL
Total_RAN_ms<-NULL
Total_X_es<-NULL
Total_X_ms<-NULL
Total_ORTH_es<-NULL
Total_ORTH_ms<-NULL
Total_UNREL_es<-NULL
Total_UNREL_ms<-NULL
Total_SEM_es<-NULL
Total_SEM_ms<-NULL
Total_PHON_es<-NULL
Total_PHON_ms<-NULL
Total_PSEUD_es<-NULL
Total_PSEUD_ms<-NULL
SFD_ms<-NULL
SFD_es<-NULL
SFD_RAN_es<-NULL
SFD_RAN_ms<-NULL
SFD_X_es<-NULL
SFD_X_ms<-NULL
SFD_ORTH_es<-NULL
SFD_ORTH_ms<-NULL
SFD_UNREL_es<-NULL
SFD_UNREL_ms<-NULL
SFD_SEM_es<-NULL
SFD_SEM_ms<-NULL
SFD_PHON_es<-NULL
SFD_PHON_ms<-NULL
SFD_PSEUD_es<-NULL
SFD_PSEUD_ms<-NULL
Fix_prob<- NULL
Fix_prob_es<-NULL


#ms
for(i in 1:nrow(dataN1)){
  FFD_ms[i]<- (dataN1$FFD_N1_inval[i]- dataN1$FFD_N1_val[i])
  FFD_RAN_ms[i]<- (dataN1$FFD_N1_inval_RAN[i]- dataN1$FFD_N1_val[i])
  FFD_X_ms[i]<- (dataN1$FFD_N1_inval_X[i]- dataN1$FFD_N1_val[i])
  FFD_ORTH_ms[i]<- (dataN1$FFD_N1_inval_ORTH[i]- dataN1$FFD_N1_val[i])
  FFD_UNREL_ms[i]<- (dataN1$FFD_N1_inval_UNREL[i]- dataN1$FFD_N1_val[i])
  FFD_SEM_ms[i]<- (dataN1$FFD_N1_inval_SEM[i]- dataN1$FFD_N1_val[i])
  FFD_PHON_ms[i]<- (dataN1$FFD_N1_inval_PHON[i]- dataN1$FFD_N1_val[i])
  FFD_PSEUD_ms[i]<- (dataN1$FFD_N1_inval_PSEUD[i]- dataN1$FFD_N1_val[i])
  GD_ms[i]<- (dataN1$GD_N1_inval[i]- dataN1$GD_N1_val[i])
  GD_RAN_ms[i]<- (dataN1$GD_N1_inval_RAN[i]- dataN1$GD_N1_val[i])
  GD_X_ms[i]<- (dataN1$GD_N1_inval_X[i]- dataN1$GD_N1_val[i])
  GD_ORTH_ms[i]<- (dataN1$GD_N1_inval_ORTH[i]- dataN1$GD_N1_val[i])
  GD_UNREL_ms[i]<- (dataN1$GD_N1_inval_UNREL[i]- dataN1$GD_N1_val[i])
  GD_SEM_ms[i]<- (dataN1$GD_N1_inval_SEM[i]- dataN1$GD_N1_val[i])
  GD_PHON_ms[i]<- (dataN1$GD_N1_inval_PHON[i]- dataN1$GD_N1_val[i])
  GD_PSEUD_ms[i]<- (dataN1$GD_N1_inval_PSEUD[i]- dataN1$GD_N1_val[i])
  SFD_ms[i]<- (dataN1$SFD_N1_inval[i]- dataN1$SFD_N1_val[i])
  SFD_RAN_ms[i]<- (dataN1$SFD_N1_inval_RAN[i]- dataN1$SFD_N1_val[i])
  SFD_X_ms[i]<- (dataN1$SFD_N1_inval_X[i]- dataN1$SFD_N1_val[i])
  SFD_ORTH_ms[i]<- (dataN1$SFD_N1_inval_ORTH[i]- dataN1$SFD_N1_val[i])
  SFD_UNREL_ms[i]<- (dataN1$SFD_N1_inval_UNREL[i]- dataN1$SFD_N1_val[i])
  SFD_SEM_ms[i]<- (dataN1$SFD_N1_inval_SEM[i]- dataN1$SFD_N1_val[i])
  SFD_PHON_ms[i]<- (dataN1$SFD_N1_inval_PHON[i]- dataN1$SFD_N1_val[i])
  SFD_PSEUD_ms[i]<- (dataN1$SFD_N1_inval_PSEUD[i]- dataN1$SFD_N1_val[i])
  Total_ms[i]<- (dataN1$Total_N1_inval[i]- dataN1$Total_N1_val[i])
  Total_RAN_ms[i]<- (dataN1$Total_N1_inval_RAN[i]- dataN1$Total_N1_val[i])
  Total_X_ms[i]<- (dataN1$Total_N1_inval_X[i]- dataN1$Total_N1_val[i])
  Total_ORTH_ms[i]<- (dataN1$Total_N1_inval_ORTH[i]- dataN1$Total_N1_val[i])
  Total_UNREL_ms[i]<- (dataN1$Total_N1_inval_UNREL[i]- dataN1$Total_N1_val[i])
  Total_SEM_ms[i]<- (dataN1$Total_N1_inval_SEM[i]- dataN1$Total_N1_val[i])
  Total_PHON_ms[i]<- (dataN1$Total_N1_inval_PHON[i]- dataN1$Total_N1_val[i])
  Total_PSEUD_ms[i]<- (dataN1$Total_N1_inval_PSEUD[i]- dataN1$Total_N1_val[i])
  Fix_prob[i]<- (dataN1$FixProb_N1_inval[i]- dataN1$FixProb_N1_val[i])
}

#ES (Cohen's d)
for(i in 1:nrow(dataN1)){
  FFD_es[i]<- (dataN1$FFD_N1_inval[i]- dataN1$FFD_N1_val[i])/ sqrt((dataN1$FFD_N1_inval_SD[i]^2+dataN1$FFD_N1_val_SD[i]^2)/2)
  FFD_RAN_es[i]<- (dataN1$FFD_N1_inval_RAN[i]- dataN1$FFD_N1_val[i])/ sqrt((dataN1$FFD_N1_inval_RAN_SD[i]^2+dataN1$FFD_N1_val_SD[i]^2)/2)
  FFD_X_es[i]<- (dataN1$FFD_N1_inval_X[i]- dataN1$FFD_N1_val[i])/ sqrt((dataN1$FFD_N1_inval_X_SD[i]^2+dataN1$FFD_N1_val_SD[i]^2)/2)
  FFD_ORTH_es[i]<- (dataN1$FFD_N1_inval_ORTH[i]- dataN1$FFD_N1_val[i])/ sqrt((dataN1$FFD_N1_inval_ORTH_SD[i]^2+dataN1$FFD_N1_val_SD[i]^2)/2)
  FFD_UNREL_es[i]<- (dataN1$FFD_N1_inval_UNREL[i]- dataN1$FFD_N1_val[i])/ sqrt((dataN1$FFD_N1_inval_UNREL_SD[i]^2+dataN1$FFD_N1_val_SD[i]^2)/2)
  FFD_SEM_es[i]<- (dataN1$FFD_N1_inval_SEM[i]- dataN1$FFD_N1_val[i])/ sqrt((dataN1$FFD_N1_inval_SEM_SD[i]^2+dataN1$FFD_N1_val_SD[i]^2)/2)
  FFD_PHON_es[i]<- (dataN1$FFD_N1_inval_PHON[i]- dataN1$FFD_N1_val[i])/ sqrt((dataN1$FFD_N1_inval_PHON_SD[i]^2+dataN1$FFD_N1_val_SD[i]^2)/2)
  FFD_PSEUD_es[i]<- (dataN1$FFD_N1_inval_PSEUD[i]- dataN1$FFD_N1_val[i])/ sqrt((dataN1$FFD_N1_inval_PSEUD_SD[i]^2+dataN1$FFD_N1_val_SD[i]^2)/2)
  GD_es[i]<- (dataN1$GD_N1_inval[i]- dataN1$GD_N1_val[i])/sqrt((dataN1$GD_N1_inval_SD[i]^2+dataN1$GD_N1_val_SD[i]^2)/2)
  GD_RAN_es[i]<- (dataN1$GD_N1_inval_RAN[i]- dataN1$GD_N1_val[i])/sqrt((dataN1$GD_N1_inval_RAN_SD[i]^2+dataN1$GD_N1_val_SD[i]^2)/2)
  GD_X_es[i]<- (dataN1$GD_N1_inval_X[i]- dataN1$GD_N1_val[i])/sqrt((dataN1$GD_N1_inval_X_SD[i]^2+dataN1$GD_N1_val_SD[i]^2)/2)
  GD_ORTH_es[i]<- (dataN1$GD_N1_inval_ORTH[i]- dataN1$GD_N1_val[i])/sqrt((dataN1$GD_N1_inval_ORTH_SD[i]^2+dataN1$GD_N1_val_SD[i]^2)/2)
  GD_UNREL_es[i]<- (dataN1$GD_N1_inval_UNREL[i]- dataN1$GD_N1_val[i])/sqrt((dataN1$GD_N1_inval_UNREL_SD[i]^2+dataN1$GD_N1_val_SD[i]^2)/2)
  GD_SEM_es[i]<- (dataN1$GD_N1_inval_SEM[i]- dataN1$GD_N1_val[i])/sqrt((dataN1$GD_N1_inval_SEM_SD[i]^2+dataN1$GD_N1_val_SD[i]^2)/2)
  GD_PHON_es[i]<- (dataN1$GD_N1_inval_PHON[i]- dataN1$GD_N1_val[i])/sqrt((dataN1$GD_N1_inval_PHON_SD[i]^2+dataN1$GD_N1_val_SD[i]^2)/2)
  GD_PSEUD_es[i]<- (dataN1$GD_N1_inval_PSEUD[i]- dataN1$GD_N1_val[i])/sqrt((dataN1$GD_N1_inval_PSEUD_SD[i]^2+dataN1$GD_N1_val_SD[i]^2)/2)
  SFD_es[i]<- (dataN1$SFD_N1_inval[i]- dataN1$SFD_N1_val[i])/ sqrt((dataN1$SFD_N1_inval_SD[i]^2+dataN1$SFD_N1_val_SD[i]^2)/2)
  SFD_RAN_es[i]<- (dataN1$SFD_N1_inval_RAN[i]- dataN1$SFD_N1_val[i])/ sqrt((dataN1$SFD_N1_inval_RAN_SD[i]^2+dataN1$SFD_N1_val_SD[i]^2)/2)
  SFD_X_es[i]<- (dataN1$SFD_N1_inval_X[i]- dataN1$SFD_N1_val[i])/ sqrt((dataN1$SFD_N1_inval_X_SD[i]^2+dataN1$SFD_N1_val_SD[i]^2)/2)
  SFD_ORTH_es[i]<- (dataN1$SFD_N1_inval_ORTH[i]- dataN1$SFD_N1_val[i])/ sqrt((dataN1$SFD_N1_inval_ORTH_SD[i]^2+dataN1$SFD_N1_val_SD[i]^2)/2)
  SFD_UNREL_es[i]<- (dataN1$SFD_N1_inval_UNREL[i]- dataN1$SFD_N1_val[i])/ sqrt((dataN1$SFD_N1_inval_UNREL_SD[i]^2+dataN1$SFD_N1_val_SD[i]^2)/2)
  SFD_SEM_es[i]<- (dataN1$SFD_N1_inval_SEM[i]- dataN1$SFD_N1_val[i])/ sqrt((dataN1$SFD_N1_inval_SEM_SD[i]^2+dataN1$SFD_N1_val_SD[i]^2)/2)
  SFD_PHON_es[i]<- (dataN1$SFD_N1_inval_PHON[i]- dataN1$SFD_N1_val[i])/ sqrt((dataN1$SFD_N1_inval_PHON_SD[i]^2+dataN1$SFD_N1_val_SD[i]^2)/2)
  SFD_PSEUD_es[i]<- (dataN1$SFD_N1_inval_PSEUD[i]- dataN1$SFD_N1_val[i])/ sqrt((dataN1$SFD_N1_inval_PSEUD_SD[i]^2+dataN1$SFD_N1_val_SD[i]^2)/2)
  Total_es[i]<- (dataN1$Total_N1_inval[i]- dataN1$Total_N1_val[i])/ sqrt((dataN1$Total_N1_inval_SD[i]^2+dataN1$Total_N1_val_SD[i]^2)/2)
  Total_RAN_es[i]<- (dataN1$Total_N1_inval_RAN[i]- dataN1$Total_N1_val[i])/ sqrt((dataN1$Total_N1_inval_RAN_SD[i]^2+dataN1$Total_N1_val_SD[i]^2)/2)
  Total_X_es[i]<- (dataN1$Total_N1_inval_X[i]- dataN1$Total_N1_val[i])/ sqrt((dataN1$Total_N1_inval_X_SD[i]^2+dataN1$Total_N1_val_SD[i]^2)/2)
  Total_ORTH_es[i]<- (dataN1$Total_N1_inval_ORTH[i]- dataN1$Total_N1_val[i])/ sqrt((dataN1$Total_N1_inval_ORTH_SD[i]^2+dataN1$Total_N1_val_SD[i]^2)/2)
  Total_UNREL_es[i]<- (dataN1$Total_N1_inval_UNREL[i]- dataN1$Total_N1_val[i])/ sqrt((dataN1$Total_N1_inval_UNREL_SD[i]^2+dataN1$Total_N1_val_SD[i]^2)/2)
  Total_SEM_es[i]<- (dataN1$Total_N1_inval_SEM[i]- dataN1$Total_N1_val[i])/ sqrt((dataN1$Total_N1_inval_SEM_SD[i]^2+dataN1$Total_N1_val_SD[i]^2)/2)
  Total_PHON_es[i]<- (dataN1$Total_N1_inval_PHON[i]- dataN1$Total_N1_val[i])/ sqrt((dataN1$Total_N1_inval_PHON_SD[i]^2+dataN1$Total_N1_val_SD[i]^2)/2)
  Total_PSEUD_es[i]<- (dataN1$Total_N1_inval_PSEUD[i]- dataN1$Total_N1_val[i])/ sqrt((dataN1$Total_N1_inval_PSEUD_SD[i]^2+dataN1$Total_N1_val_SD[i]^2)/2)
  Fix_prob_es[i]<- (dataN1$FixProb_N1_inval[i]- dataN1$FixProb_N1_val[i])/sqrt((dataN1$FixProb_N1_inval_SD[i]^2+dataN1$FixProb_N1_val_SD[i]^2)/2)
}

ES_N1 <- data.frame(Paper, Language, FFD_ms, FFD_RAN_ms, FFD_X_ms, FFD_ORTH_ms, FFD_UNREL_ms, FFD_SEM_ms, FFD_PHON_ms,
                    FFD_PSEUD_ms, FFD_es, FFD_RAN_es, FFD_X_es, FFD_ORTH_es, FFD_UNREL_es, FFD_SEM_es,
                    FFD_PHON_es, FFD_PSEUD_es, GD_ms, GD_RAN_ms, GD_X_ms, GD_ORTH_ms, GD_UNREL_ms, 
                    GD_SEM_ms, GD_PHON_ms, GD_PSEUD_ms, GD_es, GD_RAN_es, GD_X_es, GD_ORTH_es, GD_UNREL_es,
                    GD_SEM_es, GD_PHON_es, GD_PSEUD_es, SFD_ms, SFD_RAN_ms, SFD_X_ms, SFD_ORTH_ms, 
                    SFD_UNREL_ms, SFD_SEM_ms, SFD_PHON_ms, SFD_PSEUD_ms, SFD_es, SFD_RAN_es, SFD_X_es,
                    SFD_ORTH_es, SFD_UNREL_es, SFD_SEM_es, SFD_PHON_es, SFD_PSEUD_es,
                    Total_ms, Total_RAN_ms, Total_X_ms, Total_ORTH_ms, Total_UNREL_ms, Total_SEM_ms,
                    Total_PHON_ms, Total_PSEUD_ms, Total_es, Total_RAN_es, Total_X_es, Total_ORTH_es,
                    Total_UNREL_es, Total_SEM_es, Total_PHON_es, Total_PSEUD_es)

#Save in a data file:
save(ES_N1,file="Data/ES_N1.Rda")

rm(Paper, Language, FFD_ms, FFD_RAN_ms, FFD_X_ms, FFD_ORTH_ms, FFD_UNREL_ms, FFD_SEM_ms, FFD_PHON_ms,
   FFD_PSEUD_ms, FFD_es, FFD_RAN_es, FFD_X_es, FFD_ORTH_es, FFD_UNREL_es, FFD_SEM_es,
   FFD_PHON_es, FFD_PSEUD_es, GD_ms, GD_RAN_ms, GD_X_ms, GD_ORTH_ms, GD_UNREL_ms, 
   GD_SEM_ms, GD_PHON_ms, GD_PSEUD_ms, GD_es, GD_RAN_es, GD_X_es, GD_ORTH_es, GD_UNREL_es,
   GD_SEM_es, GD_PHON_es, GD_PSEUD_es, SFD_ms, SFD_RAN_ms, SFD_X_ms, SFD_ORTH_ms, 
   SFD_UNREL_ms, SFD_SEM_ms, SFD_PHON_ms, SFD_PSEUD_ms, SFD_es, SFD_RAN_es, SFD_X_es,
   SFD_ORTH_es, SFD_UNREL_es, SFD_SEM_es, SFD_PHON_es, SFD_PSEUD_es,
   Total_ms, Total_RAN_ms, Total_X_ms, Total_ORTH_ms, Total_UNREL_ms, Total_SEM_ms,
   Total_PHON_ms, Total_PSEUD_ms, Total_es, Total_RAN_es, Total_X_es, Total_ORTH_es,
   Total_UNREL_es, Total_SEM_es, Total_PHON_es, Total_PSEUD_es, Fix_prob, Fix_prob_es)



write.csv(ES_N1, file = "Data/ES.csv", row.names = TRUE)

#############################################################################################
# References:
#

# See "All_papers.xls"