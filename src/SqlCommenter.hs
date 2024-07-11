{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | This module implements the SQL Commenter protocol as specified at
https://google.github.io/sqlcommenter/spec/
It provides functionality to add comments to SQL queries containing
key-value pairs of attributes, as well as parsing such comments.
-}
module SqlCommenter (
    -- * Core functionality
    sqlCommenter,
    parseFirstSqlComment,
    renderComment,

    -- * OpenTelemetry integration
    lookupSqlCommenterAttributes,
    getSqlCommenterAttributes,
    addTraceDataToAttributes,
    getSqlCommenterAttributesWithTraceData,

    -- * Testing support
    urlEncode,
) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char
import Data.Foldable
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (StrictBuilder)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Internal.StrictBuilder as B
import Data.Word
import Network.HTTP.Types (urlDecode)
import qualified OpenTelemetry.Context as Ctxt
import qualified OpenTelemetry.Context.ThreadLocal as TL
import OpenTelemetry.Propagator.W3CTraceContext (encodeSpanContext)
import qualified OpenTelemetry.Trace.Core as OTel
import System.IO.Unsafe
import Prelude hiding (span, take, takeWhile)

-- | A key used to store SQL Commenter attributes in an OpenTelemetry context.
sqlCommenterKey :: Ctxt.Key (Map Text Text)
sqlCommenterKey = unsafePerformIO $ Ctxt.newKey "sqlcommenter-attributes"
{-# NOINLINE sqlCommenterKey #-}

{- | Lookup SQL Commenter attributes from an OpenTelemetry context.

This function retrieves the SQL Commenter attributes associated with the given
OpenTelemetry context. If no attributes are found, it returns an empty map.

@since 0.1.0.0
-}
lookupSqlCommenterAttributes :: Ctxt.Context -> Map Text Text
lookupSqlCommenterAttributes ctxt = case Ctxt.lookup sqlCommenterKey ctxt of
    Nothing -> mempty
    Just attrs -> attrs

{- | Get the current SQL Commenter attributes from the thread-local OpenTelemetry context.

This function retrieves the SQL Commenter attributes from the current thread-local
OpenTelemetry context.

@since 0.1.0.0
-}
getSqlCommenterAttributes :: IO (Map Text Text)
getSqlCommenterAttributes = lookupSqlCommenterAttributes <$> TL.getContext

{- | Add OpenTelemetry trace data to a map of SQL Commenter attributes.

This function takes an OpenTelemetry span and a map of attributes, and adds
'traceparent' and 'tracestate' entries to the map based on the span context.

@since 0.1.0.0
-}
addTraceDataToAttributes :: OTel.Span -> Map Text Text -> IO (Map Text Text)
addTraceDataToAttributes span attrs = do
    (traceparent, tracestate) <- encodeSpanContext span
    pure $
        attrs
            & M.insert "traceparent" (unsafeConvert traceparent)
            & M.insert "tracestate" (unsafeConvert tracestate)
  where
    unsafeConvert = B.toText . B.unsafeFromByteString

{- | Get SQL Commenter attributes with OpenTelemetry trace data from the current context.

This function retrieves the SQL Commenter attributes from the current thread-local
OpenTelemetry context and adds trace data if a span is present in the context.

@since 0.1.0.0
-}
getSqlCommenterAttributesWithTraceData :: IO (Map Text Text)
getSqlCommenterAttributesWithTraceData = do
    ctxt <- TL.getContext
    let attrs = lookupSqlCommenterAttributes ctxt
    case Ctxt.lookupSpan ctxt of
        Nothing -> pure attrs
        Just span -> addTraceDataToAttributes span attrs

-- Represents a set of Word8 values using four Word64 values for 256 bits.
data Word8Set = Word8Set !Word64 !Word64 !Word64 !Word64 deriving (Eq, Show)

{- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
silently truncates to 8 bits Chars > '\255'.
-}
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- Creates an empty Word8Set.
empty :: Word8Set
empty = Word8Set 0 0 0 0

-- Helper function to determine which Word64 to use and the bit position.
selectWord64 :: Word8 -> (Word8, Word8)
selectWord64 val = val `divMod` 64

-- Adds a Word8 value to the set.
insert :: Word8 -> Word8Set -> Word8Set
insert val (Word8Set w0 w1 w2 w3) = case selectWord64 val of
    (0, pos) -> Word8Set (setBit w0 $ fromIntegral pos) w1 w2 w3
    (1, pos) -> Word8Set w0 (setBit w1 $ fromIntegral pos) w2 w3
    (2, pos) -> Word8Set w0 w1 (setBit w2 $ fromIntegral pos) w3
    (3, pos) -> Word8Set w0 w1 w2 (setBit w3 $ fromIntegral pos)
    _ -> error "Word8 value out of bounds"

-- Removes a Word8 value from the set.
delete :: Word8 -> Word8Set -> Word8Set
delete val (Word8Set w0 w1 w2 w3) = case selectWord64 val of
    (0, pos) -> Word8Set (clearBit w0 $ fromIntegral pos) w1 w2 w3
    (1, pos) -> Word8Set w0 (clearBit w1 $ fromIntegral pos) w2 w3
    (2, pos) -> Word8Set w0 w1 (clearBit w2 $ fromIntegral pos) w3
    (3, pos) -> Word8Set w0 w1 w2 (clearBit w3 $ fromIntegral pos)
    _ -> error "Word8 value out of bounds"

-- Checks if a Word8 value is in the set.
member :: Word8 -> Word8Set -> Bool
member val (Word8Set w0 w1 w2 w3) = case selectWord64 val of
    (0, pos) -> testBit w0 $ fromIntegral pos
    (1, pos) -> testBit w1 $ fromIntegral pos
    (2, pos) -> testBit w2 $ fromIntegral pos
    (3, pos) -> testBit w3 $ fromIntegral pos
    _ -> error "Word8 value out of bounds"

-- SQL Commenter spec wants them escaped with a slash, but this should
-- probably solve the same issue
unreservedQS :: Word8Set
unreservedQS = foldr insert SqlCommenter.empty $ map c2w "-_.~'"

-- Monoidal intersperse function.
intersperse :: (Foldable f) => a -> f a -> [a]
intersperse sep a = case toList a of
    [] -> []
    (x : xs) -> x : prependToAll sep xs
      where
        prependToAll separator = \case
            [] -> []
            (x' : xs') -> separator : x' : prependToAll separator xs'
{-# INLINE intersperse #-}

-- Monoidal intercalate function.
intercalate :: (Monoid a, Foldable f) => a -> f a -> a
intercalate delim l = mconcat (intersperse delim l)
{-# INLINE intercalate #-}
{-# SPECIALIZE intercalate :: StrictBuilder -> [StrictBuilder] -> StrictBuilder #-}

{- | Percent-encoding for URLs.

This will substitute every byte with its percent-encoded equivalent unless:

* The byte is alphanumeric. (i.e. one of @/[A-Za-z0-9]/@)

* The byte is one of the 'Word8' listed in the first argument.
-}
urlEncodeBuilder' :: Word8Set -> Text -> StrictBuilder
urlEncodeBuilder' extraUnreserved =
    BS.foldl' (\acc c -> acc <> encodeChar c) mempty . T.encodeUtf8
  where
    encodeChar ch
        | unreserved ch = B.unsafeFromWord8 ch
        | otherwise = h2 ch

    unreserved ch
        | ch >= 65 && ch <= 90 = True -- A-Z
        | ch >= 97 && ch <= 122 = True -- a-z
        | ch >= 48 && ch <= 57 = True -- 0-9
    unreserved c = c `member` extraUnreserved

    -- must be upper-case
    h2 v = B.unsafeFromWord8 37 `mappend` B.unsafeFromWord8 (h a) `mappend` B.unsafeFromWord8 (h b) -- 37 = %
      where
        (a, b) = v `divMod` 16
    h i
        | i < 10 = 48 + i -- zero (0)
        | otherwise = 65 + i - 10 -- 65: A

urlEncodeBuilder :: Text -> StrictBuilder
urlEncodeBuilder = urlEncodeBuilder' unreservedQS

-- Parser for single-line comments
singleLineComment :: Parser Text
singleLineComment = "--" *> takeTill isEndOfLine <* (endOfLine <|> endOfInput)

-- Parser for multi-line comments
multiLineComment :: Parser Text
multiLineComment = do
    _ <- char '/'
    _ <- char '*'
    commentInner mempty
  where
    commentInner :: B.StrictBuilder -> Parser Text
    commentInner !builder = do
        txt <- takeWhile (/= '*')
        c <- char '*'
        mSlash <- peekChar
        case mSlash of
            Just '/' -> char '/' *> pure (B.toText (builder <> B.fromText txt))
            _ -> commentInner (builder <> B.fromText txt <> B.fromChar c)

-- Parser for SQL strings (ignores content inside strings)
sqlString :: Parser Text
sqlString = char '\'' *> takeTill (== '\'') <* char '\''

-- Parser for quoted identifiers (table or column names)
quotedIdentifier :: Parser Text
quotedIdentifier =
    (char '"' *> takeTill (== '"') <* char '"')
        <|> (char '`' *> takeTill (== '`') <* char '`')

-- Parser that ignores SQL strings and quoted identifiers
ignoreNonComments :: Parser (Maybe a)
ignoreNonComments = Nothing <$ (sqlString <|> quotedIdentifier <|> take 1)

-- Combined parser for comments, ignoring strings and identifiers
sqlComment :: Parser (Maybe Text)
sqlComment = Just <$> (singleLineComment <|> multiLineComment)

-- Function to check for comments, ignoring strings and identifiers
hasSqlComment :: Text -> Bool
hasSqlComment input =
    case parseOnly (many' (sqlComment <|> ignoreNonComments)) input of
        Left _ -> False
        Right parts -> any isJust parts

{- | URL-encode a Text string.

This function performs percent-encoding on the input text, following the rules
specified in the SQL Commenter protocol.

@since 0.1.0.0
-}
urlEncode :: Text -> Text
urlEncode = B.toText . urlEncodeBuilder

{- | Add SQL Commenter attributes to a SQL query.

This function takes a SQL query and a map of attributes, and returns a new SQL query
with the attributes added as a comment. If the input query already contains a comment,
or if the attributes map is empty, the original query is returned unchanged.

=== Example

> let query = "SELECT * FROM users"
> let attrs = M.fromList [("app", "myapp"), ("version", "1.0")]
> sqlCommenter query attrs
"SELECT * FROM users /* app='myapp',version='1.0' */"

@since 0.1.0.0
-}
sqlCommenter :: Text -> Map Text Text -> Text
sqlCommenter query attributes =
    if B.sbLength concatenatedAttributes == 0 || hasSqlComment query
        then query
        else
            B.toText
                ( B.fromText query
                    <> B.fromText " /* "
                    <> concatenatedAttributes
                    <> B.fromText " */"
                )
  where
    concatenatedAttributes =
        intercalate (B.fromChar ',') $
            M.foldrWithKey
                (\key value acc -> urlEncodeBuilder key <> B.fromText "='" <> B.fromText (T.replace "'" "\\'" (B.toText $ urlEncodeBuilder value)) <> B.fromChar '\'' : acc)
                []
                attributes
{-# INLINE sqlCommenter #-}

{- | Render SQL Commenter attributes as a comment string.

This function takes a map of attributes and returns a 'Maybe' 'Text' containing
the rendered comment. If the attributes map is empty, it returns 'Nothing'.

This is slightly lower-level than'sqlCommenter', and is intended for use cases
where you don't want to add the comment directly to the query.

=== Example

> let attrs = M.fromList [("app", "myapp"), ("version", "1.0")]
> renderComment attrs
Just " /* app='myapp',version='1.0' */"

@since 0.1.0.0
-}
renderComment :: Map Text Text -> Maybe Text
renderComment attributes =
    if B.sbLength concatenatedAttributes == 0
        then Nothing
        else
            Just $
                B.toText
                    ( B.fromText " /* "
                        <> concatenatedAttributes
                        <> B.fromText " */"
                    )
  where
    concatenatedAttributes =
        intercalate (B.fromChar ',') $
            M.foldrWithKey
                (\key value acc -> urlEncodeBuilder key <> B.fromText "='" <> B.fromText (T.replace "'" "\\'" (B.toText $ urlEncodeBuilder value)) <> B.fromChar '\'' : acc)
                []
                attributes
{-# INLINE renderComment #-}

parseAttribute :: Parser (Text, Text)
parseAttribute = do
    key <- takeWhile1 (/= '=')
    _ <- string "='"
    val <- valueInner mempty
    pure (T.decodeUtf8 $ urlDecode True $ T.encodeUtf8 key, T.decodeUtf8 $ urlDecode True $ T.encodeUtf8 val)
  where
    valueInner !builder = do
        txt <- takeWhile (\c -> c /= '\\' && c /= '\'')
        mNext <- peekChar
        case mNext of
            Just '\\' -> do
                _ <- string "\\'"
                valueInner (builder <> B.fromText txt <> B.fromChar '\'')
            Just '\'' -> do
                _ <- char '\''
                pure $ B.toText (builder <> B.fromText txt)
            _ -> fail "Unterminated quoted attribute value"

parseSqlCommentAsAttributes :: Parser (Maybe (Map Text Text))
parseSqlCommentAsAttributes = do
    c <- sqlComment
    let innerParser = do
            skipSpace
            attrPairs <- parseAttribute `sepBy` char ','
            skipSpace
            endOfInput
            pure $ M.fromList $ attrPairs
    case c of
        Nothing -> pure Nothing
        Just txt -> pure $ Just $ case parseOnly innerParser txt of
            Left _ -> mempty
            Right vals -> vals

{- | Parse the first SQL comment in a query and extract its attributes.

This function takes a SQL query as input and attempts to parse the first comment
it encounters, interpreting it as SQL Commenter attributes. If no valid comment
is found or if the comment cannot be parsed as attributes, an empty map is returned.

=== Example

> let query = "SELECT * FROM users /* app='myapp',version='1.0' */"
> parseFirstSqlComment query
fromList [("app", "myapp"), ("version", "1.0")]

@since 0.1.0.0
-}
parseFirstSqlComment :: Text -> Map Text Text
parseFirstSqlComment input = case parseOnly (many' (parseSqlCommentAsAttributes <|> ignoreNonComments)) input of
    Left _ -> M.empty
    Right parts -> case catMaybes parts of
        [] -> M.empty
        (comment : _) -> comment
