context("API Test")
library(rSwedishParliamentVotations)

test_that("GET_votation rejects errounous input", {
  expect_error(df <- GET_votation(period=c(2016,2018), span= TRUE, party= "C", vote_result='Yes', rows="3"))
  expect_error(df <- GET_votation(period=c(2016,2018), span= TRUE, party= "C", vote_result=3, rows=1))
  expect_error(df <- GET_votation(period=c(2016,2018), span= TRUE, party= C, vote_result='Yes', rows=1))
  expect_error(df <- GET_votation(period="c(2016,2018)", span= TRUE, party= C, vote_result='Yes', rows=1))
  expect_error(df <- GET_votation(period=2016, span= TRUUE, party= "C", vote_result='Yes', rows=1))
  
})

test_that("Class is correct", {
  df <- GET_votation(period=c(2016,2018), span= TRUE, party= "C", vote_result='Yes', rows=1)
  
  expect_true(class(df) == "data.frame")
})
