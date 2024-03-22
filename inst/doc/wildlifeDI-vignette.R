## ----warning=FALSE------------------------------------------------------------
library(wildlifeDI)
library(move2)
library(sf)
library(dplyr)
library(adehabitatLT)

data(deer)
deer

## -----------------------------------------------------------------------------
checkTO(deer)

## ----message=FALSE------------------------------------------------------------
library(sf)

## ----fig.width=5,fig.align='center'-------------------------------------------
idcol <- mt_track_id_column(deer)
mcphr <- deer |>
  group_by_at(idcol) |>
  summarise() |>
  st_convex_hull()

plot(st_geometry(mcphr),border=c("red","black"))

## -----------------------------------------------------------------------------
#Compute SI index
AIB <- mcphr |> 
  st_intersection() |>
  filter(n.overlaps == 2)

st_area(AIB)/st_area(st_union(mcphr))

## -----------------------------------------------------------------------------
deer37 <- deer[mt_track_id(deer) == '37',]
deer38 <- deer[mt_track_id(deer) == '38',]
deer_sim <- GetSimultaneous(deer37, deer38, tc = 7.5*60)
table(deer$id)
table(deer_sim$id)

## -----------------------------------------------------------------------------
Prox(deer, tc=7.5*60, dc=50)

## -----------------------------------------------------------------------------
Ca(deer, tc=7.5*60, dc=50)

## ----fig.align='center',fig.width=7-------------------------------------------
Don(deer, tc=7.5*60, dc=50)

## -----------------------------------------------------------------------------
Lixn(deer, method='spatial', tc=7.5*60)

## -----------------------------------------------------------------------------
Cs(deer, tc=7.5*60)

## -----------------------------------------------------------------------------
HAI(deer, tc=7.5*60, dc=50)

## -----------------------------------------------------------------------------
IAB(deer, dc=50, tc=7.5*60)

## -----------------------------------------------------------------------------
Cr(deer, tc=7.5*60)

## -----------------------------------------------------------------------------
DI(deer, tc=7.5*60)

## ----fig.align='center',fig.width=7-------------------------------------------
deer_prox <- Prox(deer, tc=7.5*60, dc=50, local=TRUE)
plot(mt_time(deer_prox),deer_prox$prox,type="l")

## ----fig.align='center',fig.width=7-------------------------------------------
df <- IAB(deer, dc=50, tc=7.5*60, local=TRUE)
plot(df$date, df$Iab,type='l')

## -----------------------------------------------------------------------------
#obtain the local di analysis data-frame
di.df <- DI(deer, tc=7.5*60, local=TRUE)

## ----fig.align='center',fig.width=7-------------------------------------------
#Examine the temporal dynamics of local di
plot(di.df$date, di.df$di,type="l")

## ----fig.align='center',fig.width=7-------------------------------------------
#Smoothed version of local di
di.df$smooth <- 0
#4 fixes/hour x 6 hours on either side of 12 hour centered window
w <- 4*6 
n <- dim(di.df)[1]   #no. of fixes

for (i in (w+1):(n-1-w)){
  di.temp <- di.df$di[(i-w):(i+w)]
  di.df$smooth[i] <- mean(di.temp,na.rm=T)
  }

plot(di.df$date, di.df$smooth,type="l")

