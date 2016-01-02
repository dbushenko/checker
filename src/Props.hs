{-# LANGUAGE OverloadedStrings #-}

module Props where

import Control.Monad.State
import Data.Maybe

import qualified Json.Class as C
import qualified Json.Field as F

data ClassWrapper = ClassWrapper {
      klass :: C.Class
    , props :: [Props]
    , parent :: Maybe ClassWrapper
    }
    deriving (Show, Eq)
    
data Props = Pure
           | AllFieldsFinal
           | AllFieldsPure
           | AllAncestorsFinal
           | AllAncestorsPure
           | AllMethodParamsPure
           | ClassFinal
    deriving (Show, Eq)           


containsObj :: Eq t => t -> [t] -> Bool
containsObj _ [] = False
containsObj s (str : strs) =
    if s == str
        then True
        else containsObj s strs

---------------------------------------------------
-- Properties
propsAllFieldsFinal :: [Props] -> Bool
propsAllFieldsFinal = containsObj AllFieldsFinal

propsAllFieldsPure :: [Props] -> Bool
propsAllFieldsPure = containsObj AllFieldsPure

propsAllAncestorsPure :: [Props] -> Bool
propsAllAncestorsPure = containsObj AllAncestorsPure

propsAllAncestorsFinal :: [Props] -> Bool
propsAllAncestorsFinal = containsObj AllAncestorsFinal

propsAllMethodParamsPure :: [Props] -> Bool
propsAllMethodParamsPure = containsObj AllMethodParamsPure

propsClassFinal :: [Props] -> Bool
propsClassFinal = containsObj ClassFinal

---------------------------------------------------
-- Strings

containsFinal :: [String] -> Bool
containsFinal = containsObj "final"

containsStatic :: [String] -> Bool
containsStatic = containsObj "static"

allFieldsFinal :: [F.Field] -> Bool
allFieldsFinal flds =
    case flds of
        [] -> True
        _ -> all (containsFinal . F.accessFlags) flds

-- allFieldStatic :: [F.Field] -> Bool
-- allFieldStatic flds =
--     case flds of
--         [] -> False
--         _ -> all (containsStatic . F.accessFlags) flds

allAncestorsFinal :: [ClassWrapper] -> ClassWrapper -> Bool
allAncestorsFinal wrappers w = (allFieldsFinal $ C.fields $ klass w) && (checkAncestors $ findParent wrappers w)
    where checkAncestors Nothing = True
          checkAncestors (Just x) = allAncestorsFinal wrappers x
          findParent [] w = Nothing
          findParent (p:ps) w =
              let pname = C.className $ klass p
                  cname = C.superClassName $ klass w
              in if pname == cname
                      then Just p
                      else findParent ps w
          

---------------------------------------------------------

-- -- State the-state return-value
-- checkClasses' :: [ClassWrapper] -> State (Bool, [ClassWrapper]) (Bool, [ClassWrapper])
-- checkClasses' wrappers = return (False, wrappers)

-- checkClasses :: [ClassWrapper] -> [ClassWrapper]
-- checkClasses wrappers =
--     let ((continue, newWrappers), _) = runState (checkClasses' wrappers) (False, wrappers)
--     in if continue
--            then checkClasses newWrappers
--            else newWrappers


verifyAllFieldsFinal :: ClassWrapper -> (Bool, Bool)
verifyAllFieldsFinal w = ( propsAllFieldsFinal $ props w        -- already
                         , allFieldsFinal $ C.fields $ klass w  -- really
                         )

-- verifyAllAncestorsFinal :: ClassWrapper -> (Bool, Bool)
-- verifyAllAncestorsFinal w = ( propsAllAncestorsFinal $ props w
--                             , allAncestorsFinal w
--                             )

addProperty :: ClassWrapper -> Props -> ClassWrapper
addProperty w p =
    let ps = props w
        k = klass w
        par = parent w
    in ClassWrapper k (p : ps) par

---------------------------------------------------------

checkOneClass :: [ClassWrapper] -> ClassWrapper -> (Bool, ClassWrapper)
checkOneClass wrappers w =
    case verifyAllFieldsFinal w of
        (False, True) -> (True, addProperty w AllFieldsFinal)
        _ -> case allAncestorsFinal wrappers w of
                True -> (False, addProperty w AllAncestorsFinal)
                _ -> (False, w)
        -- _ -> case verifyAllAncestorsFinal w of
        --          (False, True) -> (True, addProperty w AllAncestorsFinal)
        --          _ -> (False, w)

checkClasses' :: [ClassWrapper] -> (Bool, [ClassWrapper])
checkClasses' wrappers =
    let processed = map (checkOneClass wrappers) wrappers
        res = not $ null $ filter (\(r,_) -> r) processed
        newWrappers = map (\(_, w) -> w) processed
    in (res, newWrappers)

checkClasses :: [ClassWrapper] -> [ClassWrapper]
checkClasses wrappers =
    let (continue, newWrappers) = checkClasses' wrappers
    in if continue
           then checkClasses newWrappers
           else newWrappers


-- collectParents :: [ClassWrapper] -> [ClassWrapper]
-- collectParents wrappers = map (findParent wrappers) wrappers
--     where findParent [] w = w
--           findParent (p:ps) w =
--               let pname = C.className $ klass p
--                   cname = C.superClassName $ klass w
--               in if pname == cname
--                       then w { parent = Just p}
--                       else findParent ps w

processClasses :: [Maybe C.Class] -> [ClassWrapper]
processClasses klasses = checkClasses $ map (\(Just kl) -> ClassWrapper kl [] Nothing)
                                            $ filter isJust klasses

-- Class name, Superclass name, Found superclass name, AllFieldsFinal
printClassWrapperInfo :: ClassWrapper -> IO ()
printClassWrapperInfo wrapper = do
    let k = klass wrapper
        p = props wrapper
        -- printParent Nothing = "Nothing"
        -- printParent (Just par) = C.className $ klass par
    putStr $ C.className k
    putStr ","
    putStr $ C.superClassName k
    putStr ","
    putStr $ show $ propsAllFieldsFinal p
    putStr ","
    putStr $ show $ propsAllAncestorsFinal p
    putStr "\n"
