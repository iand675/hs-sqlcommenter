# SqlCommenter for Haskell

[![Hackage](https://img.shields.io/hackage/v/sqlcommenter.svg)](https://hackage.haskell.org/package/sqlcommenter)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

SqlCommenter is a Haskell library that implements the [SQL Commenter](https://google.github.io/sqlcommenter/) protocol. It provides functionality to add key-value pair attributes as comments to SQL queries, enhancing database observability and making it easier to correlate slow queries with application code.

## Features

- Add SQL Commenter attributes to SQL queries
- Parse SQL Commenter attributes from existing SQL queries
- Integration with OpenTelemetry for distributed tracing
- URL-encoding of attribute keys and values
- Support for both single-line and multi-line SQL comments
- Robust parsing that ignores SQL strings and quoted identifiers

## Installation

You can install SqlCommenter using Cabal or Stack. Add the following to your project's `.cabal` file:

```yaml
dependencies:
  sqlcommenter == 0.1.*
```

Or if you're using Stack, add this to your `stack.yaml`:

```yaml
extra-deps:
  - sqlcommenter-0.1.0.0
```

Then run `cabal install` or `stack build` respectively.

## Quick Start

Here's a simple example of how to use SqlCommenter:

```haskell
import SqlCommenter
import qualified Data.Map.Strict as M
import Data.Text (Text)

main :: IO ()
main = do
  let query = "SELECT * FROM users"
  let attributes = M.fromList [("app", "myapp"), ("version", "1.0")]
  let commentedQuery = sqlCommenter query attributes
  putStrLn $ "Original query: " ++ show query
  putStrLn $ "Commented query: " ++ show commentedQuery
```

This will output:

```txt
Original query: "SELECT * FROM users"
Commented query: "SELECT * FROM users /* app='myapp',version='1.0' */"
```

## Usage

### Adding comments to SQL queries

Use the `sqlCommenter` function to add attributes to your SQL queries:

```haskell
sqlCommenter :: Text -> Map Text Text -> Text
```

Example:

```haskell
import SqlCommenter
import qualified Data.Map.Strict as M
import Data.Text (Text)

query :: Text
query = "SELECT * FROM users WHERE id = ?"

attributes :: Map Text Text
attributes = M.fromList
  [ ("app", "myapp")
  , ("version", "1.0")
  , ("db_driver", "postgresql")
  ]

commentedQuery :: Text
commentedQuery = sqlCommenter query attributes
```

### Parsing comments from SQL queries

Use the `parseFirstSqlComment` function to extract attributes from an existing SQL query:

```haskell
parseFirstSqlComment :: Text -> Map Text Text
```

Example:

```haskell
import SqlCommenter
import Data.Text (Text)

query :: Text
query = "SELECT * FROM users /* app='myapp',version='1.0' */"

parsedAttributes :: Map Text Text
parsedAttributes = parseFirstSqlComment query
```

### OpenTelemetry integration

SqlCommenter provides functions to integrate with OpenTelemetry for distributed tracing:

```haskell
import SqlCommenter
import qualified OpenTelemetry.Trace.Core as OTel

main :: IO ()
main = do
  -- Get current SQL Commenter attributes
  attrs <- getSqlCommenterAttributes

  -- Get attributes with trace data
  attrsWithTrace <- getSqlCommenterAttributesWithTraceData

  -- Add trace data to existing attributes
  span <- OTel.getCurrentSpan
  updatedAttrs <- addTraceDataToAttributes span attrs

  -- Use the attributes in your SQL query
  let query = "SELECT * FROM users"
  let commentedQuery = sqlCommenter query updatedAttrs
  -- Execute commentedQuery...
```

## Contributing

Contributions to `sqlcommenter` are welcome! Please feel free to submit a Pull Request.

Before submitting a PR, please make sure that:

1. Your code adheres to the Haskell Style Guide.
2. You've added appropriate documentation and updated the README & CHANGELOG as appropriate.
3. You've added tests for new functionality.
4. All tests pass when you run `cabal test`.

## License

This project is licensed under the BSD 3-Clause License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- This project is based on the [SQL Commenter specification](https://google.github.io/sqlcommenter/spec/).
- Thanks to the OpenTelemetry community for their work on distributed tracing standards.
