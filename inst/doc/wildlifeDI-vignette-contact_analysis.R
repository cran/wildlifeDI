## ----setup, include=FALSE-----------------------------------------------------
### Cacheing data for speed
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = TRUE)

## ----message=FALSE,warning=FALSE,results='hide'-------------------------------
library(wildlifeDI)
library(adehabitatLT)
library(ggplot2)
library(sf)
library(igraph)
library(nlme)

## -----------------------------------------------------------------------------
data(does)
does
plot(does)

## -----------------------------------------------------------------------------
plt <- dcPlot(does,tc=15*60,dmax=1000)
plt

## ----warning=F,message=F------------------------------------------------------
doecons <- conProcess(does,dc=50,tc=15*60)  

## -----------------------------------------------------------------------------
doephas <- conPhase(doecons, pc=60*60)
conSummary(doephas)

## -----------------------------------------------------------------------------
doepair <- conPairs(doephas)
doetemp <- conTemporal(doephas,units='mins')

doepair$hod <- as.POSIXlt(doepair$date)$hour + as.POSIXlt(doepair$date)$min / 60  #convert POSIX to hours
doetemp$hod <- as.POSIXlt(doetemp$start_time)$hour + as.POSIXlt(doetemp$start_time)$min / 60  #convert POSIX to hours
doepair$dom <- as.POSIXlt(doepair$date)$mday
hist(doepair$dom,breaks=0:31)

## -----------------------------------------------------------------------------
hist(doepair$hod,breaks=0:24) #Figure 2b

## -----------------------------------------------------------------------------
hist(doetemp$hod,breaks=0:24) #Figure 2c

## -----------------------------------------------------------------------------
hist(as.numeric(doetemp$duration)) #figure 2d

## ----message=FALSE------------------------------------------------------------
con_sf <- conSpatial(doephas,type='point')             # Get points of all contacts

#Figure 3a
sf_pt <- ltraj2sf(does)  # Turn all fixes into sf points
plot(st_geometry(sf_pt),col='grey',pch=20)
plot(st_geometry(con_sf),col='black',pch=20,add=T)

## -----------------------------------------------------------------------------
#Figure 3b
con_sf_first <- conSpatial(doephas,type='point',def='first')

plot(st_geometry(sf_pt),col='grey',pch=20)
plot(st_geometry(con_sf),col='black',pch=20,add=T)
plot(st_geometry(con_sf_first),col='red',pch=20,add=T)

## ----message=FALSE, warning=FALSE---------------------------------------------
#Figure 3c
con_sf_ln <- conSpatial(doephas,type='line')

sf_ln <- ltraj2sf(does,type='line')  # Turn all fixes into sf points

plot(st_geometry(sf_ln),col='grey')
plot(st_geometry(con_sf_ln),col='red',add=T)

## -----------------------------------------------------------------------------
mat_cnt <- conMatrix(doecons)
#mat_rat <- conMatrix(doecons,output='rate')
mat_cnt

## -----------------------------------------------------------------------------
#shorten ID names
row.names(mat_cnt) <- substr(row.names(mat_cnt),5,6)
colnames(mat_cnt) <- substr(colnames(mat_cnt),5,6)

gr <- graph_from_adjacency_matrix(mat_cnt,mode='undirected',weighted=TRUE)
plot(gr)
# ggnet(gr, 
#       mode = "fruchtermanreingold",
#       label = TRUE, 
#       alpha = 1, 
#       color = "white", 
#       segment.color = "black",
#       segment.size = log(E(gr)$weight))

## -----------------------------------------------------------------------------
#Use ConContext for randomization Analysis 
doe_rand <- conContext(doephas,var=c('pForest','dist'),nrand=1000)

g1 = ggplot(doe_rand, aes(x=dt_lev, y=pForest)) + 
  geom_boxplot() +
  labs(x='',y='Forest Cover (%)') 

g2 = ggplot(doe_rand, aes(x=dt_lev, y=dist)) + 
  geom_boxplot() +
  labs(x='',y='Step-Length (m)')

g1
g2


## -----------------------------------------------------------------------------
tapply(doe_rand$dist, doe_rand$dt_lev, mean)
tapply(doe_rand$dist, doe_rand$dt_lev, sd) 
tapply(doe_rand$pForest, doe_rand$dt_lev, mean)
tapply(doe_rand$pForest, doe_rand$dt_lev, sd)

## -----------------------------------------------------------------------------
m1 = lme(pForest ~ dt_lev,random = ~1|ID, data = doe_rand)
summary(m1)

## -----------------------------------------------------------------------------
m2 = lme(dist ~ dt_lev, random= ~1|ID, data = doe_rand ,na.action=na.exclude)
summary(m2)

## ----eval=FALSE---------------------------------------------------------------
#  # NOT RUN
#  mca <- conProcess(deer,hunters,dc=150,tc=4*60) # process contacts, tc=4 min, dc=150m
#  mcp <- conPhase(mca,pc=16*60)                  # group into phases pc=16 min
#  
#  mcp <- conDisplacement(mcp,def='first')    # calculate displacement
#  
#  #Context Analysis
#  mockhunt <- conContext(mcp,var=c('dist','displacement','Forest_Perc'),def='first',nlag=12,lag=8*60,gap=4*60,idcol='burst',nrand=NA)

## -----------------------------------------------------------------------------
data(mockhunt)
head(mockhunt)

## -----------------------------------------------------------------------------
ggplot(data=mockhunt, aes(x=dt_lev, y=dist, group=phaid)) +
  geom_line(col='grey32') + 
  labs(x='Time to contact (min)',y='Step-length (m)') + 
  scale_x_discrete(labels=c(as.character(seq(-96,96,by=8))))

## -----------------------------------------------------------------------------
ggplot(data=mockhunt, aes(x=dt_lev, y=displacement, group=phaid)) +
  geom_line(col='grey32') + 
  labs(x='Time to contact (min)',y='Displacement (m)') + 
  scale_x_discrete(labels=c(as.character(seq(-96,96,by=8))))

## -----------------------------------------------------------------------------
ggplot(data=mockhunt, aes(x=dt_lev, y=Forest_Perc, group=phaid)) +
  geom_line(col='grey32') + 
  labs(x='Time to contact (min)',y='Forest Cover (%)') + 
  scale_x_discrete(labels=c(as.character(seq(-96,96,by=8))))

## -----------------------------------------------------------------------------
ggplot(mockhunt, aes(x=dt_lev, y=dist)) + 
  geom_boxplot() +
  coord_cartesian(ylim=c(0,1000)) +
  labs(x='Time to contact (min)',y='Step length (m)') +
  scale_x_discrete(labels=c(as.character(seq(-96,96,by=8))))

## -----------------------------------------------------------------------------
ggplot(mockhunt, aes(x=dt_lev, y=displacement)) + 
  geom_boxplot() +
  coord_cartesian(ylim=c(0,2000)) +
  labs(x='Time to contact (min)',y='Distance to contact (m)') +
  scale_x_discrete(labels=c(as.character(seq(-96,96,by=8))))

## -----------------------------------------------------------------------------
ggplot(mockhunt, aes(x=dt_lev, y=Forest_Perc)) + 
  geom_boxplot() +
  labs(x='Time to contact (min)',y='Forest Cover (%)') +
  scale_x_discrete(labels=c(as.character(seq(-96,96,by=8))))

## -----------------------------------------------------------------------------
mockhunt$t = as.integer(mockhunt$dt_lev)
mockhunt$con = 0
mockhunt$con[which(mockhunt$t > 12)] = 1
mh = mockhunt[!is.na(mockhunt$dist),]

## -----------------------------------------------------------------------------
distlme <- lme(dist ~ t*con , random = ~ 1|ID, correlation=corAR1(),data = mh)
summary(distlme)

## -----------------------------------------------------------------------------
displme <- lme(displacement ~ t*con , random = ~ 1|ID, correlation=corAR1(),data = mh)
summary(displme)

## -----------------------------------------------------------------------------
pForlme <- lme(Forest_Perc ~ t*con , random = ~ 1|ID, correlation=corAR1(),data = mh)
summary(pForlme)

## -----------------------------------------------------------------------------
sessionInfo()

