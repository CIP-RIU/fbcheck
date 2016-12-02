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

# #'Function for render quantitative traits in javascript conditions
# #'
# #'@param ll lower limit
# #'@param ul upper limmit
# #'@author omar benites
# #'@description Function to get the scale of differents trait, 
# #' @export
# 
# render_quantitative <- function(ll,ul){
#   out <- paste("function (instance, td, row, col, prop, value, cellProperties) {
#                Handsontable.renderers.TextRenderer.apply(this, arguments); 
#                if (value <", ll ,"  ) {
#                td.style.background = 'pink';
#                } else if (value >", ul,") {
#                td.style.background = 'pink';
#                } 
# }", sep="")
#     #cat(out,"\n"," ")
#   return(out)
#   }
# 
# #'Function for scale condition
# #'
# #' @param scale_condition The scale of the trait.
# #' @author omar benites
# #' @description In plant traits, we generally find quantitative and cualitative scales. Then, the scale condition
# #' creates javascript renders to higlight values which are out of range.  
# #' @export
# 
# render_categorical <- function(scale_condition){ 
#   out <- paste("function (instance, td, row, col, prop, value, cellProperties) {
#                Handsontable.renderers.TextRenderer.apply(this, arguments);
#                if (",scale_condition,"  ) {
#                td.style.background = 'pink';
#                } 
# }",sep="")
#     #cat(out,"\n"," ")
#   return(out)
#   } 

# #'R function for Javascript Render Trait 
# #'
# #'@param trait trait
# #'@param datadict data dictionary
# #'@author omar benites
# #'@description Function to get the scale of differents trait, 
# #'@export 
# 
# render_trait<- function(trait,datadict){
#   
#   tp <- trait_type(trait,datadict)  
#   scale_trait_values <- scale_trait(trait = trait,datadict = datadict)
#   
#   if(tp == "Continuous"|| tp == "Discrete"){
#     
#     ul <- scale_trait_values$ul  
#     ll <- scale_trait_values$ll 
#     
#     #assign_trait <- paste("renderer_",trait," <- ",sep="") 
#     render_trait <- render_quantitative(ll=ll,ul=ul)
#     #render_trait_rule <- paste(assign_trait,"\"",render_trait,"\"")
#     #cat("\"",render_trait,"\"","\n"," ")
#     #out <- paste("\"",render_trait,"\"","\n"," ")
#     out <- paste(render_trait)
#     
#   }
#   
#   if(tp =="Categorical"){
#     
#     categorical_scale <- scale_trait_values$cat_scale
#     n <- length(categorical_scale)
#     scale_rule_first <- paste("value!=",categorical_scale[1], sep = "") #1st class of categorical trait
#     scale_rule_global <- paste(scale_rule_first,"&&","value!=",categorical_scale[2:n]) %>%  paste(. ,collapse = " && ")
#     
#     #assign_trait <- paste("renderer_",trait," <- ",sep="") 
#     render_trait <- render_categorical(scale_condition = scale_rule_global)
#     #render_trait_rule <- paste(assign_trait,"\"",render_trait,"\"")
#     #cat("\"",render_trait,"\"","\n"," ")
#     out <- paste(render_trait)
#   }
#   
#   if(tp=="none"){
#     print("")
#   }      
#   
#   return(out)
# }


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

