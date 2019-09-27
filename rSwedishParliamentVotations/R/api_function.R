#' GET_votation
#'
#' \code{GET_votation} makes a GET request for data from the 
#' Swedish Parlament API, specifically for votations.
#' 
#' 
#' @param period an object of class "\code{data.frame}" containing three columns: "v1", "v2" (the nodes of the graph) and "w" (the distance from "v1" to "v2").
#' @param span numeric scalar object.
#' @param party nume
#' @param vote_result nume
#' @param rows nume
#'
#' @return \code{dijkstra} returns a vector with the shortest path distances from the starting node to every other node.
#'
#' @examples
#' GET_votation(period=2016, vote_result='Ja', rows=1)
#' GET_votation(party='C', rows=1)
#' 
#' @references \url{http://data.riksdagen.se/}
#'
#' @export
#' 


GET_votation <- function(period=NULL, span=FALSE, party=NULL, vote_result=NULL, rows=5){
  stopifnot(length(c(period, party, vote_result)) > 0, 
            is.numeric(rows) && rows > 0,
            is.numeric(period) || is.null(period),
            is.character(party) || is.null(party),
            is.character(vote_result) || is.null(vote_result),
            span == TRUE || span ==FALSE)
  
  path <- "http://data.riksdagen.se/voteringlista/?"
  
  if(length(period) > 0){
    period_formatted <- vector()
    
    if(span){
      period <- period[1]:period[2]
    }
    
    for(p in period){
      period_formatted <- 
        append(period_formatted, paste0(p, '/', as.integer(substr(p, nchar(p)-2+1, nchar(p))) + 1))
    }
    period_query <- paste0("rm=", paste(period_formatted, sep='&rm=', collapse = ''), collapse = '')
  }else{
    period_query <- ''
  }
  
  if(length(party) > 0){
    party_formatted <- vector()
    for(par in party){
      party_formatted <- append(party_formatted, par)
    }
    party_query <- 
      paste0("parti=", paste(party_formatted, sep='&parti=', collapse = ''), collapse = '')
    
    if(length(period) > 0){
      party_query <- paste0('&', party_query)
    }
  }else{
    party_query <- ''
  }
  
  if(length(vote_result) > 0){
    vote_formatted <- vector()
    for(vote in vote_result){
      vote_formatted <- append(vote_formatted, vote)
    }
    vote_query <- 
      paste0("rost=", paste(vote_formatted, sep='&rost=', collapse = ''), collapse = '')
    
    if(length(period) > 0 || length(party) > 0){
      vote_query <- paste0('&', vote_query)
    }
  }else{
    vote_query <- ''
  }
  
  if(!length(period) > 0 && !length(party) > 0 && !length(vote_result) > 0){
    rows_query <- paste0("sz=", rows)
  }else{
    rows_query <- paste0("&sz=", rows)
  }
  
  request <- paste0(path, period_query, party_query, vote_query, rows_query, "&utformat=xml", collapse = '')
  
  print(request)
  
  response <- read_xml(request)
  
  df <- data.frame()
  i <- 1
  for(child in xml_children(response)){
    values <- vector()
    for(subchild in xml_children(child)){
      values <- append(values, xml_text(subchild))
    }
    df[i,1:length(values)] <- values
    i <- i+1
  }
  return(df)
}