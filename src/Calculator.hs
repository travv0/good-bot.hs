module Calculator
    ( CalcExpr
    , eval
    , calcExpr
    ) where

import           Commands                       ( num )
import           Control.Applicative            ( (<|>) )
import           Data.Fixed                     ( mod' )
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )
import           Math.Gamma                     ( gamma )
import           System.Random                  ( randomRIO )
import qualified Text.Parsec                   as P
import           Text.Parsec                    ( (<?>) )
import           Text.Parsec.Text               ( Parser )

data BinaryOp
    = Plus
    | Minus
    | Times
    | Divide
    | Exponent
    | Mod
    deriving (Show, Eq)

data PrefixOp
    = Sqrt
    | Cbrt
    | Log
    | Ln
    | Sin
    | Cos
    | Tan
    | Sinh
    | Cosh
    | Tanh
    | Abs
    | Round
    | Floor
    | Ceil
    | Degrees
    | Radians
    | Neg
    | Fact
    | RandFloat
    | RandInt
    | Rand
    deriving (Show, Eq)

data SuffixOp
    = Percent
    | Factorial
    | DoubleFactorial
    deriving (Show, Eq)

data CalcExpr
    = CalcBinary CalcExpr BinaryOp CalcExpr
    | CalcPrefix PrefixOp CalcExpr
    | CalcSuffix CalcExpr SuffixOp
    | CalcVal Double
    deriving (Show, Eq)

precedence :: BinaryOp -> Int
precedence Plus     = 2
precedence Minus    = 2
precedence Times    = 3
precedence Divide   = 3
precedence Mod      = 3
precedence Exponent = 8

calcExpr :: Parser CalcExpr
calcExpr = P.spaces *> calcExpr' Nothing <* P.eof

calcExpr' :: Maybe CalcExpr -> Parser CalcExpr

calcExpr' Nothing = do
    lhs <- single
    (calcExpr' (Just lhs) <|> pure lhs) <* P.spaces

calcExpr' (Just lhs) = (binaryExpr lhs <|> pure lhs) <* P.spaces

valExpr :: Parser CalcExpr
valExpr = do
    v      <- value <|> parenExpr
    suffix <- P.optionMaybe (P.try suffixOp)
    (case suffix of
            Just op -> calcExpr' (Just (CalcSuffix v op))
            Nothing -> pure v
        )
        <* P.spaces

prefixExpr :: Parser CalcExpr
prefixExpr =
    CalcPrefix <$> prefixOp <*> (P.try valExpr <|> parenExpr) <* P.spaces

single :: Parser CalcExpr
single = (P.try valExpr <|> P.try prefixExpr <|> parenExpr) <* P.spaces

constant :: Parser Double
constant =
    (P.choice [P.char 'e' $> exp 1, P.try $ P.string "pi" $> pi] <?> "constant")
        <* P.spaces

value :: Parser CalcExpr
value = CalcVal <$> (constant <|> num) <* P.spaces

parenExpr :: Parser CalcExpr
parenExpr =
    (   P.between (P.char '(') (P.char ')') (P.spaces *> calcExpr' Nothing)
        <?> "parenthesized expression"
        )
        <* P.spaces

binaryExpr :: CalcExpr -> Parser CalcExpr
binaryExpr lhs = do
    op <- binaryOp
    let p = precedence op
    rhs    <- single
    nextOp <- P.lookAhead (P.optionMaybe binaryOp)
    let nextPrecIsHigher = maybe False (\nop -> precedence nop > p) nextOp
    (if nextPrecIsHigher
            then CalcBinary lhs op <$> calcExpr' (Just rhs)
            else calcExpr' (Just (CalcBinary lhs op rhs))
        )
        <* P.spaces

binaryOp :: Parser BinaryOp
binaryOp =
    (   P.choice
                (map
                    P.try
                    [ P.char '+' $> Plus
                    , P.char '-' $> Minus
                    , P.char '*' $> Times
                    , P.char '/' $> Divide
                    , P.string "mod" $> Mod
                    , P.char '^' $> Exponent
                    ]
                )
        <?> "operator"
        )
        <* P.spaces

prefixOp :: Parser PrefixOp
prefixOp =
    (   P.choice
                (map
                    P.try
                    [ P.string "sqrt" $> Sqrt
                    , P.string "cbrt" $> Cbrt
                    , P.string "log" $> Log
                    , P.string "ln" $> Ln
                    , P.string "sinh" $> Sinh
                    , P.string "cosh" $> Cosh
                    , P.string "tanh" $> Tanh
                    , P.string "sin" $> Sin
                    , P.string "cos" $> Cos
                    , P.string "tan" $> Tan
                    , P.string "abs" $> Abs
                    , P.string "round" $> Round
                    , P.string "floor" $> Floor
                    , P.string "ceil" $> Ceil
                    , P.string "degrees" $> Degrees
                    , P.string "radians" $> Radians
                    , P.char '-' $> Neg
                    , P.string "fact" $> Fact
                    , P.string "randf" $> RandFloat
                    , P.string "randi" $> RandInt
                    , P.string "rand" $> Rand
                    ]
                )
        <?> "function"
        )
        <* P.spaces

suffixOp :: Parser SuffixOp
suffixOp =
    (   P.choice
                (map
                    P.try
                    [ P.char '%' $> Percent
                    , P.char '!' $> Factorial
                    , P.string "!!" $> Factorial
                    ]
                )
        <?> "suffix"
        )
        <* P.spaces

reduceExpr :: CalcExpr -> IO Double
reduceExpr (CalcVal v              ) = pure v

reduceExpr (CalcBinary e1 Plus   e2) = (+) <$> reduceExpr e1 <*> reduceExpr e2
reduceExpr (CalcBinary e1 Minus  e2) = (-) <$> reduceExpr e1 <*> reduceExpr e2
reduceExpr (CalcBinary e1 Times  e2) = (*) <$> reduceExpr e1 <*> reduceExpr e2
reduceExpr (CalcBinary e1 Divide e2) = (/) <$> reduceExpr e1 <*> reduceExpr e2
reduceExpr (CalcBinary e1 Exponent e2) =
    (**) <$> reduceExpr e1 <*> reduceExpr e2
reduceExpr (CalcBinary e1 Mod e2) = mod' <$> reduceExpr e1 <*> reduceExpr e2

reduceExpr (CalcPrefix Sqrt e   ) = sqrt <$> reduceExpr e
reduceExpr (CalcPrefix Cbrt e   ) = (**) <$> reduceExpr e <*> pure (1 / 3)
reduceExpr (CalcPrefix Log  e   ) = logBase 10 <$> reduceExpr e
reduceExpr (CalcPrefix Ln   e   ) = log <$> reduceExpr e
reduceExpr (CalcPrefix Sin  e   ) = sin <$> reduceExpr e
reduceExpr (CalcPrefix Cos  e   ) = cos <$> reduceExpr e
reduceExpr (CalcPrefix Tan  e   ) = tan <$> reduceExpr e
reduceExpr (CalcPrefix Sinh e   ) = sinh <$> reduceExpr e
reduceExpr (CalcPrefix Cosh e   ) = cosh <$> reduceExpr e
reduceExpr (CalcPrefix Tanh e   ) = tanh <$> reduceExpr e
reduceExpr (CalcPrefix Abs  e   ) = abs <$> reduceExpr e
reduceExpr (CalcPrefix Round e) =
    fromIntegral . (round :: Double -> Integer) <$> reduceExpr e
reduceExpr (CalcPrefix Floor e) =
    fromIntegral . (floor :: Double -> Integer) <$> reduceExpr e
reduceExpr (CalcPrefix Ceil e) =
    fromIntegral . (ceiling :: Double -> Integer) <$> reduceExpr e
reduceExpr (CalcPrefix Degrees e) = do
    v <- reduceExpr e
    pure $ v * 180 / pi
reduceExpr (CalcPrefix Radians e) = do
    v <- reduceExpr e
    pure $ v * pi / 180
reduceExpr (CalcPrefix Neg       e) = negate <$> reduceExpr e
reduceExpr (CalcPrefix Fact      e) = factorial <$> reduceExpr e
reduceExpr (CalcPrefix RandFloat e) = randf =<< reduceExpr e
reduceExpr (CalcPrefix RandInt   e) = randi =<< reduceExpr e
reduceExpr (CalcPrefix Rand      e) = do
    n <- reduceExpr e
    if n == fromIntegral (round n :: Integer) then randi n else randf n

reduceExpr (CalcSuffix e Percent        ) = reduceExpr e <&> (* 0.01)
reduceExpr (CalcSuffix e Factorial      ) = factorial <$> reduceExpr e
reduceExpr (CalcSuffix e DoubleFactorial) = doubleFactorial <$> reduceExpr e

factorial :: Double -> Double
factorial n = gamma $ n + 1

randf :: Double -> IO Double
randf n = randomRIO (0, n)

randi :: Double -> IO Double
randi n = fromIntegral <$> randomRIO (0, round n :: Integer)

doubleFactorial :: Double -> Double
doubleFactorial n =
    let k = n / 2
    in  factorial k * 2 ** k * (pi / 2) ** (1 / 4 * (-1 + cos (n * pi)))

eval :: CalcExpr -> IO Double
eval = reduceExpr
