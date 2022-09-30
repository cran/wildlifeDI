## ----warning=FALSE------------------------------------------------------------
library(wildlifeDI)
data(deer)
deer

## -----------------------------------------------------------------------------
deer37 <- deer[1]
deer37
deer38 <- deer[2]
deer38

## -----------------------------------------------------------------------------
checkTO(deer37,deer38)

## ----message=FALSE------------------------------------------------------------
library(sf)

## ----fig.width=5,fig.align='center'-------------------------------------------
#convert ltraj to sf points to compute mcp polygon
pts37 <- ltraj2sf(deer37)
pts38 <- ltraj2sf(deer38)

#compute mcp polygons
hr37 <- st_convex_hull(st_union(pts37))
hr38 <- st_convex_hull(st_union(pts38)) 
#plot
plot(hr38)
plot(hr37,border="red",add=T)

## -----------------------------------------------------------------------------
#Compute SI index
st_area(st_intersection(hr37,hr38))/st_area(st_union(hr37,hr38))

## -----------------------------------------------------------------------------
deers <- GetSimultaneous(deer37,deer38,tc=7.5*60)
deer37.sim <- deers[1]
deer38.sim <- deers[2]
deer37.sim
deer38.sim

## -----------------------------------------------------------------------------
Prox(deer37, deer38, tc=7.5*60, dc=50)

## -----------------------------------------------------------------------------
Ca(deer37, deer38, tc=7.5*60, dc=50)

## ----fig.align='center',fig.width=7-------------------------------------------
Don(deer37,deer38, tc=7.5*60, dc=50)

## -----------------------------------------------------------------------------
Lixn(deer37, deer38, method='spatial', tc=7.5*60, 
     hr1=hr37, hr2=hr38)

## -----------------------------------------------------------------------------
Cs(deer37, deer38, tc=7.5*60)

## -----------------------------------------------------------------------------
#compute overlap zone
oz <- st_intersection(hr37, hr38)

HAI(deer37, deer38, oz, tc=7.5*60, dc=50)

## -----------------------------------------------------------------------------
IAB(deer37, deer38, dc=50, tc=7.5*60)

## -----------------------------------------------------------------------------
Cr(deer37, deer38, tc=7.5*60)

## -----------------------------------------------------------------------------
DI(deer37, deer38, tc=7.5*60)

## ----fig.align='center',fig.width=7-------------------------------------------
prox.df <- Prox(deer37, deer38, tc=7.5*60, dc=50, local=TRUE)
plot(prox.df$date1,prox.df$prox,type="l")

## ----fig.align='center',fig.width=7-------------------------------------------
df <- IAB(deer37, deer38, dc=50, tc=7.5*60, local=TRUE)
plot(df$date, df$Iab,type='l')

## -----------------------------------------------------------------------------
#obtain the local di analysis data-frame
di.df <- DI(deer37, deer38, tc=7.5*60, local=TRUE)

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

