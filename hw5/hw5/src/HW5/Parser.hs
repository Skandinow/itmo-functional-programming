module HW5.Parser ( parse ) where
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Scientific                ()
import           Data.Text                      as T
import           Data.Void                      (Void)
import           HW5.Base                       (HiExpr (..), HiFun (..),
                                                 HiValue (..))
import           Text.Megaparsec                as MP hiding (parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (pExpression <* eof) ""

pFun :: Parser HiFun
pFun = choice [
  HiFunDiv <$ string "div",
  HiFunMul <$ string "mul",
  HiFunAdd <$ string "add",
  HiFunSub <$ string "sub",
  HiFunNotEquals <$ string "not-equals",
  HiFunNotLessThan <$ string "not-less-than",
  HiFunNotGreaterThan <$ string "not-greater-than",
  HiFunNot <$ string "not",
  HiFunAnd <$ string "and",
  HiFunOr <$ string "or",
  HiFunEquals <$ string "equals",
  HiFunLessThan <$ string "less-than",
  HiFunGreaterThan <$ string "greater-than",
  HiFunIf <$ string "if",
  HiFunLength <$ string "length",
  HiFunToUpper <$ string "to-upper",
  HiFunToLower <$ string "to-lower",
  HiFunReverse <$ string "reverse",
  HiFunTrim <$ string "trim",
  HiFunList <$ string "list",
  HiFunFold <$ string "fold",
  HiFunRange <$ string "range"]

pExprValue :: Parser HiExpr
pExprValue = fmap HiExprValue $ lexeme $ choice
    [ HiValueNumber <$> toRational <$> (L.signed (pure ()) L.scientific)
    , HiValueFunction <$> pFun
    , HiValueBool <$> (True  <$ string "true" <|> False <$ string "false")
    , HiValueString . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))
    , HiValueNull <$ string "null"
    ]

pList :: Parser HiExpr
pList = do
  getArgs <- pBetweenLexeme "[" "]" (pExpression `sepBy` char ',')
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) getArgs

pE :: HiExpr -> Parser HiExpr
pE func = do
  args <- lexeme $ pBetweenLexeme "(" ")" (pExpression `sepBy` char ',')
  let applyFunction = HiExprApply func args
  continueExpression applyFunction
  where
    continueExpression expressionGet = pE' expressionGet <|> return expressionGet
    pE' func' = pE func'

pExpression :: Parser HiExpr
pExpression = makeExprParser pBaseExpression operatorTable
  where
    pBaseExpression = space *> choice [pValue, pParanthesis] <* space
    pParanthesis = do
      pTerm <- pBetweenLexeme "(" ")" pExpression
      pE pTerm <|> return pTerm
    pValue = do
      pTerm <- pExprValue <|> pList
      pE pTerm <|> return pTerm

---------------------------------------------------------------------------------------

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

pBetweenLexeme :: String -> String -> Parser a -> Parser a
pBetweenLexeme left right parser = between (string left) (string right) (lexeme parser)

---------------------------------------------------------------------------------------

operatorTable :: [[Operator Parser HiExpr]]
operatorTable = [
    [ binfixL HiFunMul "*" ,
     InfixL (getFunctionExpression HiFunDiv <$ try (lexeme (string "/" <* notFollowedBy (string "="))))],
    [ binfixL HiFunAdd "+" ,
      binfixL HiFunSub "-" ],
    [ binfixN HiFunNotGreaterThan "<=" ],
    [  binfixN HiFunNotLessThan ">=" ],
    [  binfixN HiFunEquals "==" ],
    [  binfixN HiFunNotEquals "/=" ],
    [ binfixN HiFunLessThan "<" ],
    [  binfixN HiFunGreaterThan ">" ],
    [ binfixR HiFunAnd "&&" ],
    [ binfixR HiFunOr "||"  ]
    ]

binfixL, binfixN, binfixR :: HiFun -> String -> Operator Parser HiExpr
binfixL fun str = InfixL (getFunctionExpression fun <$ string str)
binfixN fun str = InfixN (getFunctionExpression fun <$ string str)
binfixR fun str = InfixR (getFunctionExpression fun <$ string str)

getFunctionExpression :: HiFun -> HiExpr -> HiExpr -> HiExpr
getFunctionExpression fun left right = HiExprApply (HiExprValue (HiValueFunction fun)) [left, right]
