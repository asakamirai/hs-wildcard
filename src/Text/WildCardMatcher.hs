{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.WildCardMatcher where

import Control.Applicative ((<$>), (<|>), (*>))

import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Attoparsec.ByteString.Char8 as ABC

class AppendParseResult a where
    appendPR :: a -> a -> a
    concatPR :: [a] -> a
    emptyPR :: a

instance AppendParseResult BSL.ByteString where
    appendPR = BSL.append
    concatPR = BSL.concat
    emptyPR = BSL.empty

instance AppendParseResult [a] where
    appendPR = (++)
    concatPR = L.concat
    emptyPR = []

instance AppendParseResult a => AppendParseResult (ABC.Parser a) where
    appendPR pa qa = do
        a1 <- pa
        a2 <- qa
        return $ appendPR a1 a2
    concatPR [] = emptyPR
    concatPR (pa:pas) = do
        a <- pa
        appendPR a <$> concatPR pas
    emptyPR = return emptyPR

instance AppendParseResult a => AppendParseResult (ABC.Parser a -> ABC.Parser a) where
    appendPR npa nqa = npa . nqa
    concatPR [] = emptyPR
    concatPR (npa:npas) = do
        appendPR npa $ concatPR npas
    emptyPR = id

(+>) :: AppendParseResult a => ABC.Parser a -> ABC.Parser a -> ABC.Parser a
p +> q = p >>= \ a -> appendPR a <$> (p **> q)

(**>) :: AppendParseResult a => ABC.Parser a -> ABC.Parser a -> ABC.Parser a
p **> q = (p +> q) <|> q

{-
(+>>) :: AppendParseResult a => ABC.Parser b -> ABC.Parser a -> ABC.Parser a
p +>> q = p *> (p **>> q)

(**>>) :: AppendParseResult a => ABC.Parser b -> ABC.Parser a -> ABC.Parser a
p **>> q = (p +>> q) <|> q

(+<<) :: AppendParseResult a => ABC.Parser a -> ABC.Parser b -> ABC.Parser a
p +<< q = p >>= \ a -> appendPR a <$> (p **<< q)

(**<<) :: AppendParseResult a => ABC.Parser a -> ABC.Parser b -> ABC.Parser a
p **<< q = (p +<< q) <|> (q *> return emptyPR)
---}
-------------------------------

pLbs :: BS.ByteString -> ABC.Parser BSL.ByteString
pLbs lbs = BSL.fromStrict <$> ABC.string lbs

pAnyChar :: ABC.Parser BSL.ByteString
pAnyChar = BSLC.singleton <$> ABC.anyChar

asteriskParserN :: ABC.Parser (ABC.Parser BSL.ByteString -> ABC.Parser BSL.ByteString)
asteriskParserN = ABC.takeWhile1 ('*' ==) *> (return $ \ r -> (pAnyChar **> r))

asteriskParser :: ABC.Parser (ABC.Parser BSL.ByteString)
asteriskParser = ($ emptyPR) <$> asteriskParserN

stringParserN :: ABC.Parser (ABC.Parser BSL.ByteString -> ABC.Parser BSL.ByteString)
stringParserN = appendPR <$> stringParser

stringParser :: ABC.Parser (ABC.Parser BSL.ByteString)
stringParser = pLbs <$> ABC.takeWhile1 ('*' /=)

wildCardParserN :: ABC.Parser (ABC.Parser BSL.ByteString -> ABC.Parser BSL.ByteString)
wildCardParserN = concatPR <$> (ABC.many' $ asteriskParserN <|> stringParserN)

wildCardParserGenerater :: ABC.Parser (ABC.Parser BSL.ByteString)
wildCardParserGenerater = ($ emptyPR) <$> wildCardParserN

generateParser :: ABC.Parser (ABC.Parser BSL.ByteString) -> BS.ByteString -> ABC.Parser BSL.ByteString
generateParser pg bs = case ABC.parse pg bs of
    ABC.Done _ r -> r
    ABC.Partial f -> case f "" of
        ABC.Done _ r -> r
        _ -> emptyPR
    _ -> emptyPR

type Matcher = BS.ByteString -> Bool

matchWildCardString :: BS.ByteString -> Matcher
matchWildCardString "" "" = True
matchWildCardString _  "" = False
matchWildCardString wildCardString targetString = case ABC.parse parser targetString of
    ABC.Done "" _ -> True
    ABC.Partial f -> case f "" of
        ABC.Done "" _ -> True
        _ -> False
    _ -> False
    where
        parser = generateParser wildCardParserGenerater wildCardString




