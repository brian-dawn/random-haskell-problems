module MoleculeToAtoms where

import Text.ParserCombinators.ReadP
import Data.List


parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = case parsed of
  Nothing -> Left "Invalid input"
  Just result -> Right result
  where
    parsed = parseFormula formula


--parseMolecule "Mg(OH)2" -- magnesium hydroxide
--Right [("Mg",1),("O",2),("H",2)]


data Formula = Element String
             | Multiplier Int [Formula]
             deriving Show
                
element = do
  first <- choice $ map char ['A'..'Z']
  rest <- many $ choice $ map char ['a'..'z']
  return $ Element $ [first] ++ rest

number = do
  nums <- many1 $ choice $ map char ['0'..'9']
  return (read nums :: Int)
  
elementWithCount = do
  elem <- element
  quantity <- option 1 number
  return $ Multiplier quantity [elem]

molecule = do
  elementsWithCount <- many1 elementWithCount
  return elementsWithCount


-- todo if this fails we want to propagate back an error.
-- we could return an Either with everything to back propagate... UHG We want this to return an Either tree...
brackets open close p = do
  char open
  r <- p
  char close
  n <- option 1 number
  return [Multiplier n r]

formula = do
  a <- molecule
    +++ (brackets '(' ')' formulas)
    +++ (brackets '{' '}' formulas)
    +++ (brackets '[' ']' formulas)
  return a

formulas = do
  a <- many1 formula
  return $ foldr1 (++) a

readElement = readP_to_S element "Fe"

readElementWithCount = readP_to_S elementWithCount "Fe"
readMolecule = readP_to_S molecule "H2O"
readFormula = readP_to_S formula "(H2O)"
readFormulas = readP_to_S formulas "(H2O)Fe"


walkFormula multiplier (Element element) = [(element, multiplier)]
walkFormula multiplier (Multiplier m children) = foldr1 (++) $ map (walkFormula (m * multiplier)) children

startWalkFormula formulas = foldr1 (++) $ map (walkFormula 1) formulas

simplifyResult results = map sumTuples $ groupByFst sorted
  where
    sumTuples tuples = foldr1 (\acc a -> (fst acc, (snd a) + (snd acc))) tuples
    groupByFst = groupBy (\a b -> fst a == fst b)
    sorted = sortByFirstOccurance results


parseFormula xs = parsedFormula  
  where
    parsedFormula = fmap (simplifyResult . startWalkFormula . fst) $ find (null . snd) (readP_to_S formulas xs)


sortByFirstOccurance xs = sortBy (\a b -> compare (elemIndex (fst a) fsts)
                                                  (elemIndex (fst b) fsts))
                          xs
                          where
                            fsts = map fst xs
