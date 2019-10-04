## ----include=FALSE-------------------------------------------------------
library(knitr)

## ---- results='asis', echo=FALSE-----------------------------------------
parties.df <- data.frame("Party"= c("Centerpartiet", 
                                   "Folkpartiet", 
                                   "Liberalerna", 
                                   "Kristdemokraterna",
                                   "Miljöpartiet",
                                   "Moderata Samlingspartiet",
                                   "Socialdemokraterna",
                                   "Sverigedemokraterna",
                                   "Vänsterpartiet",
                                   "Others"), 
                        "Code"= c("C", "FP", "L", "KD", "MP", "M", "S", "SD", "V", "-" ))
kable(parties.df, align = "c")

## ------------------------------------------------------------------------
# Loading requiered packages.
library(rSwedishParliamentVotations)

# Setting the function arguments.
df <- GET_votation(period=c(2016,2018), span= TRUE, party= "C", vote_result='Ja', rows=30)

## ------------------------------------------------------------------------
head(df)

