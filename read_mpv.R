#### a script to parse and clean mpv data
#### authors: FE RB
#### created 4/2/24
#### last update: 4/2/24
#### notes: passed off to RB for codebook review and EDA

### To do:
### basic EDA
### set up for name / race / place imputation
### ID and remove all non use-of-force deaths


library(tidyverse)

mpv<-read_csv("./data/mpv-4-2-24.csv")

### magnitudes of missing data on core demographics (age, gender, race, date)

mpv<-mpv %>% 
  mutate(year = year(mdy(date)))

# table(mpv$year)

### gender
table(mpv$gender)
### to do: collapse to F/M/T/NA

### race
table(mpv$race)


# use of force eda --------------------------------------------------------

table(mpv$cause_of_death)


# list used in PNAS we include cases in which the cause of death was coded as: asphyxiated /249
#restrained; beaten / bludgeoned with instrument; chemical agent / pepper spray; gunshot; medical emergency; and tasered
### audit this list, ID use of force and non-force. 
### convert into exhaustive binary list
# physical force (incl all hand-to-hand and baton use)
# taser
# guns
# vehicles
# dogs
# accidental (inclusive of all non-force deaths from accidents)

### evaluate PNAS (edwards et al 2019) exclusion criteria case-by-case against MPV

