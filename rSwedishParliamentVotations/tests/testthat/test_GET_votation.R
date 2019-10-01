context("API Test")
library(rSwedishParliamentVotations)

test_that("GET_votation rejects errounous input", {
  expect_error(df <- GET_votation(period=c(2016,2018), span= TRUE, party= "C", vote_result='Ja', rows="3"))
  expect_error(df <- GET_votation(period=c(2016,2018), span= TRUE, party= "C", vote_result=3, rows=1))
  expect_error(df <- GET_votation(period=c(2016,2018), span= TRUE, party= C, vote_result='Ja', rows=1))
  expect_error(df <- GET_votation(period="c(2016,2018)", span= TRUE, party= C, vote_result='Ja', rows=1))
  expect_error(df <- GET_votation(period=2016, span= TRUUE, party= "C", vote_result='Ja', rows=1))
  
})

test_that("Class is correct", {
  df <- GET_votation(period=c(2016,2018), span= TRUE, party= "C", vote_result='Ja', rows=1)
  
  expect_true(class(df) == "data.frame")
})

test_that("Testing outputs for some given values",{
  df <- GET_votation(period=c(2016,2018), span= TRUE, party= "C", vote_result='Ja', rows=1)
  
  expect_equal(df$id_number, "5008634")
  expect_equal(df$fiscal_year, "2018/19")
  expect_equal(df$designation, "AU1")
  expect_equal(df$point, "1")
  expect_equal(df$votation_id, "F06B69C1-265A-4916-86FD-C03C1C3BB334")
  expect_equal(df$stakeholder_id, "084624777218")
  expect_equal(df$name, "Helena Lindahl")
  expect_equal(df$forename, "Helena")
  expect_equal(df$surname, "Lindahl")
  expect_equal(df$constituency, "Västerbottens län")
  expect_equal(df$city, "")
  expect_equal(df$party, "C")
  expect_equal(df$seat_number, "168")
  expect_equal(df$sex, "kvinna")
  expect_equal(df$birth_year, "1972")
  expect_equal(df$vote, "Ja")
  expect_equal(df$refers, "sakfrågan")
  expect_equal(df$votation, "huvud")
  expect_equal(df$votation_url, "http://data.riksdagen.se/votering/F06B69C1-265A-4916-86FD-C03C1C3BB334")
  expect_equal(df$dokument_id, "H601AU1")
  expect_equal(df$system_date, "2018-12-20 16:03:28")
})
