#+++++++++++++++
# Front Matter +
#+++++++++++++++

##Activate Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(timeDate)
library(hydroGOF) # Use for rmse() function to calculate proper exponent
#library(reshape2)     ## dcast(), melt()

#___________________________________________________________________________________________________________________
#
#++++++++++++++++++
# Read in Data    +
#++++++++++++++++++

## 2017-2018 Dual Meet Results
dat<-read.csv("C:/My Documents/Research/Wrestling/Results/ResultsFormatting/Data/master2017_2018.csv",header=F, sep=",")
dat$V1 <-as.character(dat$V1)

## D1 Schools List
d1List <- read.csv("C:/My Documents/Research/Wrestling/Results/ResultsFormatting/Data/d1Schools2017_2018.csv",header=T, sep=",")
d1List$School <- as.character(d1List$School)

#____________________________________________________________________________________________________________________
#
#++++++++++++++
# Format Data +
#++++++++++++++

##Dual Meet
dualDat <- dat %>%
  filter(!grepl(paste(c("125",'133','141','149','157','165','174','184','197','285','HWT','Hwt','258',"vs.","FloArena",'NWCA','\\*'),collapse="|"),V1)) %>% # Filter for date and dual scores only
  transmute(new=gsub("([0-9]+)",",\\1 ",V1)) %>% # add commas for easier separate next
  separate(col=new,into=c("t1","t1Score","t2","t2Score"),sep=",",fill='right') %>%
  mutate(t2=gsub("\\:","",t2)) %>% # Drop colon after day;
  mutate(t1=gsub("^\\s+|\\s+$", "", t1), # Trim leading and trailing whitespace
         t1Score=gsub("^\\s+|\\s+$", "", t1Score),
         t2=gsub("^\\s+|\\s+$", "", t2),
         t2Score=gsub("^\\s+|\\s+$", "", t2Score)) %>%
  mutate(dyWk=ifelse(t1 %in% c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),t1,NA), # Create columns for day of week, month and day
         mo=ifelse(grepl(paste(c("November","December","January","February","March"),collapse="|"),t1Score),t1Score,NA), 
         da=ifelse(grepl(paste(1:31,collapse="|"),t2),t2,NA)) %>%
  fill(dyWk,mo,da) %>% # fill day of week, month and day columns down
  filter(!is.na(t2Score)) %>% # Remove rows with only dates
  mutate(t1=as.character(t1), # Define correct variable type for all columns
         t1Score=as.numeric(t1Score),
         t2=as.character(t2),
         t2Score=as.numeric(t2Score),
         dyWk=as.factor(dyWk),
         mo=as.factor(mo),
         da=as.numeric(da))
  
##Individual Dual Results
#DEV NOTE: 1)Figure out how to deal with em-,en- or regular dashes in gsub statement
#          2) Figure out how to deal with colons after weights in gsub statement
indivDMD <- dat %>%
# ####
  # mutate(date=ifelse(grepl(paste(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),collapse="|"),V1),V1,NA)) %>% # Create column for date
  # separate(col=date, into=c("dyWk","mo","da"),sep=",\\s|\\s") %>%                                 # Separate date into columns for text day of week, month and numeric day
  # mutate(da=gsub(":","",da)) %>%  # Remove colons after numeric day
  # fill(dyWk,mo,da) %>% # fill day of week, month and day columns down
  # filter(!grepl(paste(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),collapse="|"),V1)) %>% # Drop Date only lines
  # mutate(teams=ifelse(!grepl(paste(c("125",'133','141','149','157','165','174','184','197','285','HWT','Hwt','258','* Davidson deducted team point'),collapse="|"),V1),V1,NA)) %>% # Create column for team names and scores
  # mutate(teams=gsub("[0-9]","",teams)) %>% # Remove team scores
  # separate(col=teams,into=c("t1","t2"),sep=",") %>% # Separate individual teams into columns
  # mutate(t1=gsub("^\\s+|\\s+$", "", t1), # Trim leading and trailing whitespace
  #        t2=gsub("^\\s+|\\s+$", "", t2)) %>%
  # fill(t1,t2) %>%
# ####
  filter(grepl(paste(c("125",'133','141','149','157','165','174','184','197','285','HWT','Hwt','258'),collapse="|"),V1)) %>%
  #filter(!grepl(paste(c("tf",'fall','pin','wbf','fft','forfeit'),collapse="|"),V1,ignore.case=T)) %>% # Limit to decisions and major decisions
  mutate(V1=sub('^([0-9]{3})(:)','\\1 -',V1)) %>% # Trying to deal with colons after weights
  mutate(V1=sub('^([A-z]{3})(:)','\\1 -',V1,ignore.case=T)) %>% # Trying to deal with colons after weights; heavyweight denoted in text (i.e. - HWT or Hwt)
  # DEV NOTE: Still need to QaQc this to make sure it is recoding as expected
  mutate(type=ifelse(grepl(paste(c('sv-[0-9]','sv','tiebreaker-[0-9]','sudden victory-[0-9]','tb-[0-9]'),collapse='|'),V1,ignore.case=T),'SV ', # Standardize coding for win types; location not yet standardized
                     ifelse(grepl(paste(c('major','maj\\s','maj.','major dec','maj dec','maj. dec.','md\\s','md\\.\\s'),collapse='|'),V1,ignore.case=T),'MD ',
                            ifelse(grepl(paste(c('wins by decision over','decision','dec.','dec\\s'),collapse='|'),V1,ignore.case=T),'DEC ', 
                            ifelse(grepl(paste(c('tf\\s','tf;\\s','tech\\s','techs\\s','tech. fall','tf\\.\\s'),collapse='|'),V1,ignore.case=T),'TF ',
                                          ifelse(grepl(paste(c('fall\\s','by fall','\\sf\\s','\\spin\\s','\\spins\\s','\\sp.\\s','fall\\.\\s','wbf','pinned'),collapse='|'),V1,ignore.case=T),'F ',
                                                 ifelse(grepl(paste(c('wins by forfeit','won by forfeit','by forfeit','forfeit','for.','fft'),collapse='|'),V1,ignore.case=T),'FFT ',
                                                        ifelse(grepl('injury default',V1,ignore.case=T),'ID',
                                                               ifelse(grepl('disqualified',V1,ignore.case=T),'DQ',
                                                                      ifelse(grepl('def.',V1,ignore.case=T),'DF ',
                                                                             ifelse(grepl('inj.',V1,ignore.case=T),'MFF',NA))))))))))) %>%
  #mutate(V1)
  mutate(V1=gsub(paste(c('over','\\.','wb','\\@','sv-[0-9]','sv','tiebreaker-[0-9]','sudden victory-[0-9]','tb-[0-9]','major','maj\\s','maj.','major dec','maj dec','maj. dec.',
                         'md\\s','md\\.\\s','wins by decision over','decision','dec\\.','dec\\s','tf\\s','tf;\\s','tech\\s','techs\\s','tech. fall','tf\\.\\s','fall\\s','by fall',
                         '\\sf\\s','\\spin\\s','\\spins\\s','\\sp.\\s','fall\\.\\s','wbf','pinned','wins by forfeit','won by forfeit','by forfeit','forfeit','for\\.','fft',
                         'inj\\.','injury default','disqualified','def\\.'),collapse='|'),'',V1,ignore.case=T)) %>%
  mutate(V1=gsub('#[0-9]{1}|#[0-9]{2}|No. [0-9]{2}','',V1)) %>% # Remove Rankings
  mutate(V1=gsub('Campbell, NC','Campbell',V1)) %>%
  mutate(V1=gsub('\\(|\\)','~',V1)) %>%
  #mutate(V1=gsub(',,|, ,',',',V1)) %>%
  separate(col=V1,into=c("wt_w1",'w1School','w2','w2School','score'),sep='~') %>%
  mutate(wt_w1=gsub("^\\s+|\\s+$", "", wt_w1), # Trim leading and trailing whitespace
         w1School=gsub("^\\s+|\\s+$", "", w1School),
         w2=gsub("^\\s+|\\s+$", "", w2),
         w2School=gsub("^\\s+|\\s+$", "", w2School),
         score=gsub("^\\s+|\\s+$", "", score)) %>%
  mutate(wt_w1=gsub("\\s-\\s|-\\s|-|\\s|\\:",'~',wt_w1),w2=gsub('\\s','~',w2)) %>%
  separate(col=wt_w1,into=c('wt','w1First','w1Last'),sep='~') %>%
  separate(col=w2,into=c('w2First','w2Last'),sep='~')

  mutate(V1=gsub('\\s',',',V1)) %>%
  mutate(V1=gsub(',,,|,,|,-,',',',V1)) %>%
  #mutate(V1=gsub(',$','',V1))
  separate(col=V1,into=c('wt','w1First','w1Last','w1School','w2First','w2Last','w2School','w1Score','w2Score','time'),sep=',')
  
  # transmute(V1=gsub('wins by decision over|decision|dec.|dec','DEC ',V1,ignore.case=T)) %>% # Standardize coding for win types; location not yet standardized
  # transmute(V1=gsub('major|maj|maj.|major dec|maj dec|maj. dec.','MD ',V1,ignore.case=T)) %>%
  # transmute(V1=gsub('tf|tech|tech. fall','TF',V1,ignore.case=T)) %>%
  # transmute(V1=gsub('fall|by fall|f|pin','F',V1,ignore.case=T)) %>%
  # transmute(V1=gsub('wins by forfeit|won by forfeit|forfeit','FFT',V1,ignore.case=T)) %>%
  # transmute(V1=gsub('def.',"DF", V1,ignore.case=T)) %>%
  transmute(V1=gsub('over|\\.|wb|\\@','',V1,ignore.case=T)) %>% # Remove unneeded text
  
  
##Missing Dual meet results
missingDat <- dat %>%
  filter(grepl(paste(c("vs.","FloArena"),collapse="|"),V1)) 

#______________________________________________________________________________________
#
#+++++++++++++++++++++++++++++++++++++++++++++++++
# Dual Team Pythagorean Winning Expectation      +
#                                                +
# DEV NOTE: Strength of Schedule adjustment?     +
#+++++++++++++++++++++++++++++++++++++++++++++++++

### Data Format
## Filter for d1 v. d1 only
# Format list of d1 schools
d1ListAbb <- d1List %>%
  mutate(School=ifelse(School=='University of Pennsylvania','Penn',ifelse(School=='Pennsylvania State University','Penn State',ifelse(School=='California Polytechnic State University','Cal Poly',
                          ifelse(School=='California State University, Bakersfield','CSU Bakersfield',ifelse(School=='Virginia Military Institute','VMI',
                          ifelse(School=='U.S. Air Force Academy', 'Air Force',ifelse(School=='U.S. Military Academy',"Army",ifelse(School=='U.S. Naval Academy','Navy', #Also Army West Point
                          ifelse(School=='University of Pittsburgh','Pitt',ifelse(School=='North Carolina State University','NC State',
                          ifelse(School=='Southern Illinois University Edwardsville','SIUE',ifelse(School=='University of Illinois at Urbana?Champaign','Illinois',
                          ifelse(School=='The Ohio State University','Ohio State',School)))))))))))))) %>%
  mutate(School=gsub('university|university of|university at|?madison|?stillwater|Tennessee at|\\?| at UrbanaChampaign|at Chapel Hill|College|Pennsylvania|\\,|Bloomington|Lincoln|park','',School,ignore.case=T)) %>% # Remove unneeded text
  mutate(School=gsub("^\\s+|\\s+$", "", School)) # Trim leading and trailing whitespace

# Standardize d1 school names in results
dualDat <- dualDat %>%
  arrange(t1) %>%
  mutate(t1=ifelse(t1=="North Carolina State","NC State",t1),t2=ifelse(t2=="Army West Point","Army",ifelse(t2=="North Carolina State","NC State",t2)))


#Filter for d1 Schools
dualDatD1 <- dualDat %>%
  filter(t1 %in% d1ListAbb$School) %>% # Filter for d1 on winners column
  filter(t2 %in% d1ListAbb$School) # Filter for d1 on losers column

##Points For (PF)
#Points for Winners (PFW)
teamPFW <- dualDatD1 %>%
  group_by(t1) %>%
  summarize(nWin=n(),PFW=sum(t1Score)) 

#Points for Losers (PFL)
teamPFL <- dualDatD1 %>%
  group_by(t2) %>%
  summarize(nLoss=n(),PFL=sum(t2Score)) 

# Join PFW and PFL and sum for PF
teamPF <- teamPFW %>%
  full_join(teamPFL,by=c("t1"="t2")) %>%
  mutate(nWin=ifelse(is.na(nWin),0,nWin),nLoss=ifelse(is.na(nLoss),0,nLoss),PFW=ifelse(is.na(PFW),0,PFW),PFL=ifelse(is.na(PFL),0,PFL)) %>%
  mutate(n=nWin+nLoss,PF=PFW+PFL)

##Points Against (PA)
# Points Against Winners (PAW)
teamPAW <- dualDatD1 %>%
  group_by(t1) %>%
  summarize(PAW=sum(t2Score))

# Points Against Losers (PAL)
teamPAL <- dualDatD1 %>%
  group_by(t2) %>%
  summarize(PAL=sum(t1Score))

# Join PAW and PAL and sum for PA
teamPA <- teamPAW %>%
  full_join(teamPAL,by=c("t1"="t2")) %>%
  mutate(PAW=ifelse(is.na(PAW),0,PAW),PAL=ifelse(is.na(PAL),0,PAL)) %>%
  mutate(PA=PAW+PAL)

## Team Win Expectation
# d1 points per dual (PPD) and Exponent
# Total point by dual
totalPtsDual <- dualDatD1 %>%
  mutate(totalPts=t1Score+t2Score)

# Number of duals to date in current season
nDualsTeam <- dualDatD1 %>%
  count(t1)

nDualsTotal <- sum(nDualsTeam$n)

# Calculate points PPD
PPD <- sum(totalPtsDual$totalPts)/nDualsTotal

#Calculate Pythagenport (Exp1) and Pythagenpat (Exp2) for whole d1
d1Exp1 <- 1.5*log10(PPD)+0.45
d1Exp2 <- PPD^0.287

# Combine PF and PA and calcuate first-order teamPWP
teamPWPdual <- teamPF %>%
  full_join(teamPA, by="t1") %>%
  mutate(avgPF=PF/n,avgPA=PA/n,actualWP=nWin/n,teamExp1=1.5*log10((PF+PA)/n)+0.45,teamExp2=((PF+PA)/n)^0.287,d1Exp1=d1Exp1,d1Exp2=d1Exp2,teamPWP=1/(1+((PA/PF)^1.92)),teamPWPavg=1/(1+((avgPA/avgPF)^1.92)),teamPWPexp1=1/(1+((PA/PF)^teamExp1)),teamPWPexp2=1/(1+((PA/PF)^teamExp2)),
         teamPWPd1Exp1=1/(1+((PA/PF)^d1Exp1)),teamPWPd1Exp2=1/(1+((PA/PF)^d1Exp2))) %>%
  arrange(desc(teamPWP)) %>%
  left_join(d1ListAbb, by=c('t1'='School')) %>%
  select(-Nickname,-Primary.conference,-National.titles.2.) #%>%
  #arrange(desc(teamPWP))

# RMSE for iteratively determining exponent
rmse(teamPWPdual$teamPWP,teamPWPdual$actualWP) # seems that exp of 2.75 minimizes RMSE; 12/21 - 2.22; 02/06 - 1.99; 02/13 - 1.95

# Descriptive Stats for Exps 1 and 2
meanExp1 <- mean(teamPWPdual$teamExp1)
sdExp1 <- sd(teamPWPdual$teamExp1)
meanExp2 <- mean(teamPWPdual$teamExp2)
sdExp2 <- sd(teamPWPdual$teamExp2)

# PLaying with plots
# Regression Stats PWP v. WP
summary(lm(teamPWPdual$teamPWP~teamPWPdual$actualWP))
# Plot actual WP v. PE
ggplot(teamPWPdual,aes(actualWP,teamPWP))+geom_smooth(method=lm)+geom_point(aes(color=Conference))+geom_abline(aes(intercept=0, slope=1))
# Plot histogram of PE 
ggplot(teamPWPdual,aes(teamPWP))+geom_histogram(binwidth = 0.05)
# Histogram of totalPts scored per dual (poisson distribution?)
ggplot(totalPtsDual,aes(totalPts))+geom_histogram(binwidth= 1)

## Third Order Team Dual Meet Win Expectation
# Average PF scored by DI teams
avgPF.PAd1 <- teamPWPdual %>%
  summarize(nd1=sum(n),PFd1=sum(PF),PAd1=sum(PA),medianPFd1=median(avgPF),medianPAd1=median(avgPA)) %>%
  mutate(avgPFd1=PFd1/nd1,avgPAd1=PAd1/nd1)

t1t2only <- dualDatD1 %>%
  select(t1,t2)

t2t1only <- dualDatD1 %>%
  select(t1=t2,t2=t1)

avgOPA.OPF <- t1t2only %>%
  bind_rows(t2t1only) %>%
  arrange(t1) %>%
  left_join(teamPWPdual,by=c("t2"="t1")) %>%
  select(t1,t2,avgPF,avgPA) %>%
  group_by(t1) %>%
  summarize(avgOPF=mean(avgPF),avgOPA=mean(avgPA))

##############################################################################
# Calculate points expected PPD
PPD <- sum(totalPtsDual$totalPts)/nDualsTotal

#Calculate Pythagenport (Exp1) and Pythagenpat (Exp2) for whole d1
d1Exp1 <- 1.5*log10(PPD)+0.45
d1Exp2 <- PPD^0.287
############################################################################# 

# RMSE for iteratively determining exponent
rmse(team3rdWPEdual$team3rdPWP,team3rdWPEdual$actualWP) # 12/21 - 2.14 minimizes RMSE

# RMSE for iteratively determining exponent - 3rd Order
rmse(team3rdWPEdual$ePA,team3rdWPEdual$PA) # 12/21 - <1 minimizes RMSE
rmse(team3rdWPEdual$ePF,team3rdWPEdual$PF) # 12/21 - <1 minimizes RMSE

ggplot(team3rdWPEdual,aes(PA,ePA))+geom_smooth(method=lm)+geom_point(aes(color=Conference))+geom_abline(aes(intercept=0, slope=1))
ggplot(team3rdWPEdual,aes(PF,ePF))+geom_smooth(method=lm)+geom_point(aes(color=Conference))+geom_abline(aes(intercept=0, slope=1))
ggplot(team3rdWPEdual,aes(actualWP,ePA))+geom_smooth(method=lm)+geom_point(aes(color=Conference))+geom_abline(aes(intercept=0, slope=1))
ggplot(team3rdWPEdual,aes(actualWP,ePF))+geom_smooth(method=lm)+geom_point(aes(color=Conference))+geom_abline(aes(intercept=0, slope=1))

team3rdWPEdual <- teamPWPdual %>%
  full_join(avgOPA.OPF, by="t1") %>%
  mutate(avgPFd1=avgPF.PAd1$avgPFd1[1],avgPAd1=avgPF.PAd1$avgPAd1[1],ePF=PF*((avgPAd1/avgOPA)^1.5),ePA=PA*((avgPFd1/avgOPF)^1.5)) %>%
  mutate(team3rdExp1=1.5*log10((ePF+ePA)/n)+0.45,team3rdExp2=((ePF+ePA)/n)^0.287,#d1eExp1=d1eExp1,d1eExp2=d1eExp2,
         team3rdPWP=1/(1+((ePA/ePF)^1.59)),team3rdPWPexp1=1/(1+((ePA/ePF)^team3rdExp1)),team3rdPWPexp2=1/(1+((PA/PF)^team3rdExp2))) %>% #,
         #teamPWPd1Exp1=1/(1+((PA/PF)^d1Exp1)),teamPWPd1Exp2=1/(1+((PA/PF)^d1Exp2)))
  arrange(desc(team3rdPWP)) 

# RMSE for iteratively determining exponent
rmse(team3rdWPEdual$team3rdPWP,team3rdWPEdual$actualWP) # 12/21 - 2.14 minimizes RMSE, 02/06 - 1.59

# Regression Stats PWP v. WP
summary(lm(team3rdWPEdual$team3rdPWP~team3rdWPEdual$actualWP))
# Plot actual WP v. PE
ggplot(team3rdWPEdual,aes(actualWP,team3rdPWP))+geom_smooth(method=lm)+geom_point(aes(color=Conference))+geom_abline(aes(intercept=0, slope=1))
# Plot histogram of PE 
ggplot(team3rdWPEdual,aes(team3rdPWP))+geom_histogram(binwidth = 0.05)

team3rdWPEdual %>%
  arrange(desc(team3rdPWPexp1))

# Summarize only PWP values fro comparison
blah <- team3rdWPEdual %>%
  select(t1,n,actualWP,teamPWP,teamPWPavg,team3rdPWP) %>%
  arrange(desc(team3rdPWP))

#__________________________________________________________________________________________________________________________________________________
#
#+++++++++++++++++++++++++++++++++++++++++++++++++
# Simple Rating System Calulation                +
#+++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate MOV for winners and losers; summs/averages to zero
t1MOV <- dualDatD1 %>%
  mutate(tMOV=t1Score-t2Score) %>%
  select(t=t1,tMOV)

t2MOV <- dualDatD1 %>%
  mutate(tMOV=t2Score-t1Score) %>%
  select(t=t2,tMOV)

mov <- t1MOV %>%
  bind_rows(t2MOV) %>%
  group_by(t) %>%
  summarize(tavgMOV=mean(tMOV)) %>%
  arrange(desc(tavgMOV))

# Create opponents list
t1OppoList <- dualDatD1 %>%
  select(t1,t2)

t2OppoList <- dualDatD1 %>%
  select(t1=t2,t2=t1)

oppoList <- t1OppoList %>%
  bind_rows(t2OppoList) %>%
  arrange(t1)

# Merge oppoList with mov 
oppoMOV <- oppoList %>%
  left_join(mov,by=c('t2'='t')) %>%
  group_by(t1) %>%
  summarize(oppoMOV=mean(tavgMOV)) %>%
  arrange(desc(oppoMOV))

#Merge mov with oppoMOV
srsTemp <- mov %>%
  left_join(oppoMOV,by=c('t'='t1')) %>%
  mutate(srs=tavgMOV+oppoMOV) %>%
  arrange(desc(srs))

#Re-run w/ new srs
oppoSRS <- oppoList %>%
  left_join(srsTemp,by=c('t2'='t')) %>%
  group_by(t1) %>%
  summarize(oppoSRS=mean(srs))

#Merge srsTemp with oppoSRS
srsTemp2 <- srsTemp %>%
  left_join(oppoSRS,by=c('t'='t1')) %>%
  mutate()
  
  
  
