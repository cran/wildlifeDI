# ---- roxygen documentation ----
#
#' @title Calculate net displacement from contacts
#' @description
#' Calculate the net-displacement (distance) of fixes before and after a contact from that contact point.
#'
#' @details
#' This function is used to compute the net displacement away from contacts by an animal before and after a contact phase. Net displacement represents an important contextual variable, related to the mobility of the individual.  

#' @param ltraj an object of the class \code{ltraj} which should be output from the function \code{conPhase}.
#' @param def how to define the point-of-contact. The default is to define it as all fixes in a phase \code{type = 'all'}, alternatively contacts can be defined as a single point along the phase defined as one of: \code{'first','last','minDist','minTime'}, which corresponds to the first fix int he contact phase, the last fix in the contact phase, the fix with the minimum time difference and the fix with the closest contact distance.
#' @param idcol column id associated with IDs of individuals, default is the 'burst'
#'
#' @return
#' An ltraj object with a new 'displacement' column in infolocs.
#'
# @references
#'
#' @keywords contacts
#' @seealso conPhase, conContext
#' 
#' @examples 
#' \dontrun{
#' data(does)
#' doecons <- conProcess(does,tc=15*60,dc=50)
#' doephas <- conPhase(doecons,pc=60*60)
#' disp_f <- conDisplacement(doephas,def='first')
#' disp_l <- conDisplacement(doephas,def='last')
#' }
#' 
#' @export
#
# ---- End of roxygen documentation ----

#ASSUMES PROJECTED COORDINATES
conDisplacement <- function(ltraj,def='all',idcol='burst'){
  
  #Get the Fix ID of every Contact based on 'DEF'
  cpdf <- conPairs(ltraj)
  cid <- NULL
  for (p in 1:max(cpdf$contact_pha)){
    cpdf_sub <- cpdf[cpdf$contact_pha == p,]
    if (def == 'first'){
      cid <- c(cid, cpdf_sub$contact_orig_rowid[which.min(cpdf_sub$date)])
    } else if (def == 'last'){
      cid <- c(cid, cpdf_sub$contact_orig_rowid[which.max(cpdf_sub$date)])
    } else if (def == 'minTime'){
      cid <- c(cid, cpdf_sub$contact_orig_rowid[which.min(cpdf_sub$contact_dt)])
    } else if (def == 'minDist') {
      cid <- c(cid, cpdf_sub$contact_orig_rowid[which.min(cpdf_sub$contact_d)])
    } else {
      cid <- c(cid,unique(cpdf_sub$contact_orig_rowid))
    }
  }
  
  # Set up displacement analysis
  dfr <- ld(ltraj)
  dfr$displacement <- 0
  n <- dim(dfr)[1]
  
  #Peform Displacement individually for every Animal.
  anid <- unique(dfr[,idcol])
  for (ani in anid){
    ind <- which(dfr[,idcol]==ani)
    cid_ani <- cid[which(cid %in% ind)]
    if (length(cid_ani) == 0) {
      #animal has no contacts so displacement is NA
      dfr$displacement[ind] <- NA
    } else {
      for (i in ind){
        j <- cid_ani[which.min(abs(dfr$date[i]-dfr$date[cid_ani]))]
        df$displacement[i] <-sqrt((dfr$x[i]-dfr$x[j])^2+(dfr$y[i]-dfr$y[j])^2)
      }
    }
  }

  outtraj <- dl(dfr,proj4string=attr(ltraj,'proj4string'))
  return(outtraj)
}