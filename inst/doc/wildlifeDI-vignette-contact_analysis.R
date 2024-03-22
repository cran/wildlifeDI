## ----setup, include=FALSE-----------------------------------------------------
### Cacheing data for speed
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = TRUE)

## ----message=FALSE,warning=FALSE,results='hide'-------------------------------
library(wildlifeDI)
library(move2)
library(adehabitatLT)
library(ggplot2)
library(sf)
library(igraph)
library(dplyr)

## -----------------------------------------------------------------------------
data(does)
does

## -----------------------------------------------------------------------------
ggplot(does) + 
  geom_sf(aes(color=mt_track_id(does)))

## -----------------------------------------------------------------------------
dcPlot(does,tc=15*60,dmax=1000)

## ----warning=F,message=F------------------------------------------------------
doecons <- conProcess(does,dc=50,tc=15*60)
table(doecons$contact)

## -----------------------------------------------------------------------------
doephas <- conPhase(doecons, pc=60*60)

consum <- doephas |>
    st_drop_geometry() |>
    filter(!is.na(contact_pha)) |>
    dplyr::group_by(contact_pha) |>
    dplyr::summarise(
      nfix = n(),
      t1 = min(date),
      t2 = max(date),
      duration = max(date)-min(date),
      avg_d = mean(contact_d,na.rm=T),
      min_d = min(contact_d,na.rm=T),
      max_d = max(contact_d,na.rm=T)
    ) 
consum

## -----------------------------------------------------------------------------
#contact phase initiaition tod
consum$tod <- as.numeric(as.POSIXlt(consum$t1)$hour + as.POSIXlt(consum$t1)$min / 60)  #convert POSIX to hours

conall <- doecons |>
  subset(contact == 1)
conall$tod <- as.numeric(as.POSIXlt(conall$date)$hour + as.POSIXlt(conall$date)$min / 60)  #convert POSIX to hours


h1 <- ggplot(consum,aes(tod)) + 
  geom_histogram(binwidth=1)
h2 <- ggplot(conall,aes(tod)) + 
  geom_histogram(binwidth=1)
h1
h2

## ----message=FALSE------------------------------------------------------------
ggplot() + 
  geom_sf(data=does,aes(color=mt_track_id(does))) +
  geom_sf(data=conall)

## -----------------------------------------------------------------------------
pha_fir <- doephas |>
  filter(!is.na(contact_pha)) |>
  group_by(contact_pha) |>
  filter(row_number()==1)


ggplot() + 
  geom_sf(data=does,aes(color=mt_track_id(does))) +
  geom_sf(data=pha_fir)

## ----message=FALSE, warning=FALSE---------------------------------------------
pha_lin <- doephas |>
  filter(!is.na(contact_pha)) |>
  group_by(contact_pha) |>
  summarise(n = dplyr::n(),do_union=FALSE) |>
  filter(n > 1) |>
  st_cast("LINESTRING")

ggplot() + 
  geom_sf(data=does,aes(color=mt_track_id(does))) +
  geom_sf(data=pha_lin)


## -----------------------------------------------------------------------------
cons <- conProcess(does,dc=50,tc=15*60,return='contacts')


tab_cnt <- cons |>
  count(id1,id2)
gr <- graph_from_data_frame(tab_cnt,directed=FALSE)
E(gr)$weight <- tab_cnt$n

plot(gr)

## -----------------------------------------------------------------------------
doephas$stepLength <- as.numeric(mt_distance(doephas))

# Calculate time to any contact fix
doephas <- conTimelag(doephas,def='all')

#categorize time to contact as immediately before, contact, after, or non contact (NA) 
#Should be tailored to individual dataset
doephas$dt_lev <-  cut(doephas$contact_timelag, breaks = c(-Inf,-45*60,-15*60,15*60,45*60,Inf), labels = c("Other","Before","Contact","After","Other"))
table(doephas$dt_lev)


ggplot(doephas, aes(x=dt_lev, y=pForest)) + 
  geom_boxplot() +
  labs(x='',y='Forest Cover (%)') 

## -----------------------------------------------------------------------------
ggplot(doephas, aes(x=dt_lev, y=stepLength)) + 
  geom_boxplot() +
  labs(x='',y='Step-Length (m)') + 
  scale_y_continuous(trans='log10')

## -----------------------------------------------------------------------------
sessionInfo()

