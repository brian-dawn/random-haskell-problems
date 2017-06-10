module TinyThreePassCompiler where

import Control.Applicative
import Data.Char
import Text.ParserCombinators.ReadP
import Data.List
import Debug.Trace(trace)


data AST = Imm Int -- immediate value (literals)
         | Arg Int -- argument nth (variables)
         | Add AST AST -- these are just prefix notation operations
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

compile :: String -> [String]
compile = pass3 . pass2 . pass1

argumentDef = do
  variable <- many1 $ choice $ map char ['a'..'z']
  return $ variable

unboxVariable Nothing = -1 -- really this should never happen so this is kinda a hack.
unboxVariable (Just n) = n

mathExp  argList = (mathExp' argList) <* skipSpaces <* eof
mathExp' argList = (term argList) `chainl1` addop
term argList = (expr argList) `chainl1` mulop
expr argList = number <|> var argList <|> parens (mathExp' argList)

var argList = skipSpaces *> (Arg <$> (fmap
                                      (\ a -> unboxVariable $ elemIndex a argList)
                                      (munch1 isAlpha)))
addop  = skipSpaces *> (Add <$ char '+' <|> Sub <$ char '-')
mulop  = skipSpaces *> (Mul <$ char '*' <|> Div <$ char '/')
number = skipSpaces *> (Imm . read <$> munch1 isDigit)
parens = between (skipSpaces *> char '(') (skipSpaces *> char ')')

argList = do
  args <- sepBy argumentDef skipSpaces
  return args

function = do
  skipSpaces
  char '['
  skipSpaces
  args <- argList
  skipSpaces
  char ']'
  skipSpaces
  expr <- mathExp args
  return $ expr
  
pass1 :: String -> AST
pass1 prog = fst $ last $ readP_to_S function prog

------------------------------- PASS 2


-- we can simplify anything where there isn't a variable.
-- https://en.wikipedia.org/wiki/Constant_folding

-- naive simplify
simplify' (Add (Imm a) (Imm b)) = Imm $ a + b
simplify' (Sub (Imm a) (Imm b)) = Imm $ a - b
simplify' (Mul (Imm a) (Imm b)) = Imm $ a * b
simplify' (Div (Imm a) (Imm b)) = Imm $ a `div` b

-- move constants left
-- todo idk if needed
simplify' (Add (Imm a) (Arg b)) = Add (Arg b) (Imm a)

-- walk
simplify' (Add a b) = Add (simplify' a) (simplify' b)
simplify' (Sub a b) = Sub (simplify' a) (simplify' b)
simplify' (Mul a b) = Mul (simplify' a) (simplify' b)
simplify' (Div a b) = Div (simplify' a) (simplify' b)
simplify' a = a

simplify :: AST -> AST
simplify a = last $ take 15 $ iterate simplify' a




pass2 :: AST -> AST
pass2 = undefined

pass3 :: AST -> [String]
pass3 = undefined




main = print $ simplify $ pass1 "[a] 1 + 3 + a + ((a + 2) + a)"


