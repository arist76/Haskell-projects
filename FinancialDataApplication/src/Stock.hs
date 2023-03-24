{-# LANGUAGE OverloadedStrings #-}

module Stock where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as Map
import Data.String (IsString)
import qualified Network.HTTP.Simple as HttpSimple

-- newtype Stock = Stock String

newtype Stock = Stock String

companyOverview :: Stock -> IO (Map.Map String String)
companyOverview stock = do
  request <- getRequest stock "OVERVIEW"
  response <- HttpSimple.httpJSON request
  return (HttpSimple.getResponseBody response :: Map.Map String String)

incomeStatement :: Stock -> IO Aeson.Value
incomeStatement stock = do
  request <- getRequest stock "INCOME_STATEMENT"
  response <- HttpSimple.httpJSON request
  return (HttpSimple.getResponseBody response :: Aeson.Value)

balanceSheet :: Stock -> IO Aeson.Value
balanceSheet stock = do
  request <- getRequest stock "BALANCE_SHEET"
  response <- HttpSimple.httpJSON request
  return (HttpSimple.getResponseBody response :: Aeson.Value)

cashFlow :: Stock -> IO Aeson.Value
cashFlow stock = do
  request <- getRequest stock "CASH_FLOW"
  response <- HttpSimple.httpJSON request
  return (HttpSimple.getResponseBody response :: Aeson.Value)

news :: Stock -> IO Aeson.Value
news forex = do
  request <- getRequest forex "NEWS_SENTIMENT"
  response <- HttpSimple.httpJSON request
  return (HttpSimple.getResponseBody response :: Aeson.Value)

getRequest stock func = do
  HttpSimple.parseRequest $
    case stock of
      Stock f -> "https://www.alphavantage.co/query?function=" ++ func ++ "&tickers=" ++ f ++ "&sort=LATEST&apikey=TU2CJ3HWRZJEVJYG"
