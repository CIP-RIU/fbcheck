#'Function for trait type
#' @param trait trait
#' @param datadict data dictionary
#' @author omar benites
#' @export
#' 
trait_type <- function(trait,datadict)
{
  tp <- as.character(datadict[datadict$ABBR==trait,c("TYPE")]) 
  stringr::str_trim(tp,side="both")
  
  if(is.na(tp)){
    tp <- "none"
  }
  return(tp)
}

#'Function for scale trait conditions 
#'
#'@param trait trait
#'@param datadict data dictionary
#'@author omar benites
#'@description Function to get the scale of differents trait, 
#' @export
#' 
scale_trait <- function(trait,datadict){
  
  tp <- trait_type(trait = trait,datadict = datadict)
  
  if(tp=="Continuous"||tp=="Discrete"){
    
    ll <- as.numeric(datadict[datadict$ABBR==trait,c("LOWER")])
    ul <- as.numeric(datadict[datadict$ABBR==trait,c("UPPER")])
    output <- list(ll=ll,ul=ul)
  }
  
  if(tp=="Categorical"){
    cat_scale <- datadict[datadict$ABBR == trait, c("CLASS1","CLASS2","CLASS3","CLASS4","CLASS5","CLASS6","CLASS7","CLASS8","CLASS9","CLASS10")]
    pattern <- "= .*$"
    cat_scale <- gsub(pattern=pattern,replacement = "",x = cat_scale)
    cat_scale <- suppressWarnings(as.numeric(cat_scale))
    cat_scale <- as.numeric(stringr::str_trim(cat_scale[!is.na(cat_scale)],side="both"))
    output <- list(cat_scale=cat_scale)
  }
  
  if(tp=="none"){output <- print("none")
  }
  
  invisible(output)
  
}



#' Function to obtain parameter from fieldbook 
#' @description This function gets parameters or values from fieldbook excel file. Do an excel scrapping.
#' @param fp fieldbook path
#' @param sheet fieldbook's sheet
#' @param param Parameters
#' @export 
#' 
#' 
get.fb.param <-function(fp,sheet,param){
  params <- readxl::read_excel(path = fp, sheet = sheet)
  params <- as.data.frame(params)
  lapply(x <- 1:ncol(params), function(x)  params[,x]<-as.character(params[,x]))
  #for(i in 1:ncol(params)) params[,i]<-as.character(params[,i])
  params[params$Factor==param,2]
}


#' Function to obtain parameter from Participatory Varietal Selection's fieldbooks (PVS).
#' @description This function gets parameters or values from PVS fieldbook excel file. Do an excel scrapping.
#' @param pvs_data fieldbook path
#' @param col_param column parameter. By default is "Mother".
#' @param row_param row parameter. By default is ""Plot_size_(m2)"
#' @export 
#' 
#' 

get_pvs_param <- function(pvs_data, col_param = "Mother", row_param = "Plot_size_(m2)"){
  
  #param <- readxl::read_excel(path=hot_file , sheet = "Installation")
  params <- as.data.frame(pvs_data)
  out  <-  as.numeric(params[params$Factor==row_param, col_param]) #dejar un peque?o espacio despues de (m2) 
  #plant.denmb = as.numeric(instmb[instmb$Factor=="Planting density (plants/Ha)","Mother"])
  
}


