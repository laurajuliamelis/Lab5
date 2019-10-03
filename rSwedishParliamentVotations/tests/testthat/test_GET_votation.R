context("API Test")
library(rSwedishParliamentVotations)

test_that("GET_votation rejects errounous input", {
  expect_error(df <- GET_votation(rows="3"))
  expect_error(df <- GET_votation(vote_result=3))
  expect_error(df <- GET_votation(party= C))
  expect_error(df <- GET_votation(period="c(2016,2018)"))
  expect_error(df <- GET_votation(period=2016, span= TRUUE))
})

test_that("GET_votation rejects input out of bounds",{
  expect_error(df <- GET_votation(rows=0))
  expect_error(df <- GET_votation(rows=90001))
  expect_error(df <- GET_votation(vote_result=c("M", "L")))
})

test_that("GET_votation period and span works as intended", {
  # NOTE: Will fail if additional data would be induced in the database, making results exceed 500.
  df1 <- GET_votation(period=c(2004, 2007), span= TRUE, party="-", vote_result = "No", rows = 500)
  df2 <- GET_votation(period=c(2007, 2004), span= TRUE, party="-", vote_result = "No", rows = 500)
  df3 <- GET_votation(period=c(2004, 2005, 2006, 2007), span= FALSE, party="-", vote_result = "No", rows = 500)
  
  expect_true(identical(df1, df2) && identical(df1, df3))
})

test_that("Class is correct", {
  df <- GET_votation(period=c(2016,2018), span= TRUE, party= "C", vote_result='Yes', rows=1)
  
  expect_true(class(df) == "data.frame")
})


test_that("GET_votation query size is equal or smaller to rows and allows for no result", {
  # NOTE: Will fail if additional data would be induced in the database, making results exceed 500.
  expect_true(nrow(GET_votation(period=c(2004, 2007), span= TRUE, party="-", vote_result = "No", rows = 500)) < 500)
  expect_true(nrow(GET_votation(rows = 500)) == 500)
  expect_true(nrow(GET_votation(period=2002, party="-", vote_result = "No", rows = 500)) == 0)
})

