{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.Constant where

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B
import Data.Scientific

data Constant = ConstNameAndType {
      name :: Constant
    , constantType :: Constant
    }
    |
    ConstString {
      stringValue :: String
    }
    |
    ConstUtf8 {
      utf8Value :: String
    }
    |
    ConstInt {
      intValue :: Scientific
    }
    |
    ConstLong {
      longValue :: Scientific
    }
    |
    ConstFloat {
      floatValue :: Scientific
    }
    |
    ConstDouble {
      doubleValue :: Scientific
    }
    |
    ConstInterfaceMethod {
      methodName :: Constant
    , className :: Constant
    }
    |
    ConstEmpty
    |
    ConstUnknown
    deriving (Show, Eq)

instance FromJSON Constant where
  parseJSON (Object v) = do
    let _name = HM.lookup "name" v
        _constantType = HM.lookup "constantType" v
        _stringValue = HM.lookup "stringValue" v
        _utf8Value = HM.lookup "utf8Value" v
        _intValue = HM.lookup "intValue" v
        _longValue = HM.lookup "longValue" v
        _floatValue = HM.lookup "floatValue" v
        _doubleValue = HM.lookup "doubleValue" v
        _methodName = HM.lookup "methodName" v
        _className = HM.lookup "className" v
    return $ tryCreateConstant _name _constantType _stringValue _utf8Value _intValue _longValue _floatValue _doubleValue _methodName _className

    where tryCreateConstant (Just (String __name)) (Just (String __constantType)) _ _ _ _ _ _ _ _ =
            let name' = T.encodeUtf8 __name
                constantType' = T.encodeUtf8 __constantType
                constName = eitherDecode (B.fromStrict name') :: Either String Constant
                constConstantType = eitherDecode (B.fromStrict constantType') :: Either String Constant
            in tryCreateNameAndType constName constConstantType
            where tryCreateNameAndType (Right cname) (Right ctype) = ConstNameAndType cname ctype
                  tryCreateNameAndType _ _ = ConstUnknown
                
          tryCreateConstant _ _ (Just (String __stringValue)) _ _ _ _ _ _ _ = ConstString $ T.unpack __stringValue
          tryCreateConstant _ _ _ (Just (String __utf8Value))  _ _ _ _ _ _ = ConstUtf8 $ T.unpack __utf8Value
          tryCreateConstant _ _ _ _ (Just (Number  __intValue)) _ _ _ _ _ = ConstInt __intValue
          tryCreateConstant _ _ _ _ _ (Just (Number __longValue)) _ _ _ _ = ConstLong __longValue
          tryCreateConstant _ _ _ _ _ _ (Just (Number __floatValue)) _ _ _ = ConstFloat __floatValue
          tryCreateConstant _ _ _ _ _ _ _ (Just (Number __doubleValue)) _ _ = ConstDouble __doubleValue
          tryCreateConstant _ _ _ _ _ _ _ _ (Just (String __methodName)) (Just (String __className)) =
            let methodName' = T.encodeUtf8 __methodName
                className' = T.encodeUtf8 __className
                constmethodName = eitherDecode (B.fromStrict methodName') :: Either String Constant
                constclassName = eitherDecode (B.fromStrict className') :: Either String Constant
            in tryCreateNameAndType constmethodName constclassName
            where tryCreateNameAndType (Right cname) (Right cclass) = ConstInterfaceMethod cname cclass
                  tryCreateNameAndType _ _ = ConstUnknown
          tryCreateConstant _ _ _ _ _ _ _ _ _ _ = ConstEmpty
      
          -- tryCreateConstant _ _ _ _ (Just (Number __intValue)) _ _ _ _ _ =
          --     case toBoundedInteger __intValue :: Maybe Int of
          --         Just n -> ConstInt n
          --         Nothing -> ConstUnknown
          -- tryCreateConstant _ _ _ _ _ (Just (Number __longValue)) _ _ _ _ = 
          --     case toBoundedInteger __longValue :: Maybe Int of
          --         Just n -> ConstLong n
          --         Nothing -> ConstUnknown

-- data Constant = Constant {
--       name :: Maybe Constant
--     , constantType :: Maybe Constant
--     , stringValue :: Maybe String
--     , utf8Value :: Maybe String
--     , intValue :: Maybe Int
--     , longValue :: Maybe Integer
--     , floatValue :: Maybe Double
--     , doubleValue :: Maybe Double
--     , methodName :: Maybe Constant
--     , className :: Maybe Constant
--     }
--     deriving (Show, Eq, Generic)
-- instance FromJSON Constant
