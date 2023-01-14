{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Strict                     #-}
module Sound.SuperCollider.SynthDef (
  SynthDef(..), GraphDef(..), UGen(..), defaultSynthDef
) where

import           Control.Applicative        (Applicative (liftA2))
import           Control.DeepSeq            (NFData)
import           Control.Monad              (replicateM)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Binary                (Binary (get, put))
import           Data.Binary.Get            (Get, getByteString, getFloatbe,
                                             getInt16be, getInt32be,
                                             getWord16be, getWord32be, getWord8)
import           Data.Binary.Put            (Put, putByteString, putFloatbe,
                                             putInt32be, putWord16be,
                                             putWord32be, putWord8)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as ByteString (length)
import           Data.Foldable              (for_)
import           Data.Int                   (Int32)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeLatin1, decodeUtf8,
                                             encodeUtf8)
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector (length, replicateM)
import           Data.Vector.Unboxed        (Unbox)
import qualified Data.Vector.Unboxed        as Unboxed (Vector)
import qualified Data.Vector.Unboxed        as Unboxed.Vector (forM_, length,
                                                               replicateM)
import           Data.Word                  (Word16, Word32)
import           GHC.Generics               (Generic)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

newtype SynthDef = SynthDef [GraphDef]
                 deriving (Eq, Generic, Lift, NFData, Read, Semigroup, Show)

instance FromJSON SynthDef
instance ToJSON SynthDef

instance Binary SynthDef where
  get = getByteString 4 >>= \case
    "SCgf" -> getInt32be >>= \case
      2 -> SynthDef <$> getList getInt16be get
      _ -> fail "Unknown SynthDef version"
    _ -> fail "Does not look like SuperCollider SynthDef data"
  put (SynthDef gs) = do
    putByteString "SCgf"
    putWord32be 2
    putList gs putWord16be put

data GraphDef = GraphDef Text
                         (Unboxed.Vector Float)
                         (Unboxed.Vector Float)
                         [(Text, Int32)]
                         (Vector UGen)
                         [(Text, Unboxed.Vector Float)]
              deriving (Eq, Generic, Read, Show, Lift)

instance NFData GraphDef
instance FromJSON GraphDef
instance ToJSON GraphDef

instance Binary GraphDef where
  get = do
    graphDef <- GraphDef <$> fmap decodeUtf8 getPascalString
                         <*> getUnboxedVector getWord32be getFloatbe
    ctrlDefaults <- getUnboxedVector getWord32be getFloatbe
    graphDef ctrlDefaults <$> getList getWord32be getControlName
                          <*> getVector getWord32be get
                          <*> getList getWord16be (getVariant (Unboxed.Vector.length ctrlDefaults))
   where
    getControlName = liftA2 (,) (decodeLatin1 <$> getPascalString) getInt32be
    getVariant n = liftA2 (,) (decodeLatin1 <$> getPascalString) (Unboxed.Vector.replicateM n getFloatbe)
  put (GraphDef n c cd cn u v) = do
    putPascalString (encodeUtf8 n)
    putUnboxedVector c putWord32be putFloatbe
    putUnboxedVector cd putWord32be putFloatbe
    putList cn putWord32be $ \(a, b) -> do
      putPascalString (encodeUtf8 a)
      putInt32be b
    putWord32be . fromIntegral . Vector.length $ u
    for_ u put
    putList v putWord16be $ \(vn, vcd) -> do
      putPascalString (encodeUtf8 vn)
      Unboxed.Vector.forM_ vcd putFloatbe

data Rate = Scalar | Control | Audio | Demand
          deriving (Bounded, Enum, Eq, Generic, Read, Show, Lift)

instance NFData Rate
instance FromJSON Rate
instance ToJSON Rate

instance Binary Rate where
  get = getWord8 >>= \case
    0 -> pure Scalar
    1 -> pure Control
    2 -> pure Audio
    3 -> pure Demand
    n -> fail $ "Unknown rate value: " ++ show n
  put = putWord8 . fromIntegral . fromEnum

data UGen = UGen Text
                 Rate
                 Word16
                 (Unboxed.Vector (Int32, Word32))
                 (Vector Rate)
          deriving (Eq, Generic, Read, Show, Lift)

instance NFData UGen
instance FromJSON UGen
instance ToJSON UGen

instance Binary UGen where
  get = do
    ugen <- UGen <$> fmap decodeLatin1 getPascalString <*> get
    nIn <- fromIntegral <$> getWord32be
    nOut <- fromIntegral <$> getWord32be
    ugen <$> getWord16be
         <*> Unboxed.Vector.replicateM nIn (liftA2 (,) getInt32be getWord32be)
         <*> Vector.replicateM nOut get
  put (UGen n r s i o) = do
    putPascalString (encodeUtf8 n)
    put r
    putWord32be . fromIntegral . Unboxed.Vector.length $ i
    putWord32be . fromIntegral . Vector.length $ o
    putWord16be s
    Unboxed.Vector.forM_ i $ \(a, b) -> do
      putInt32be a
      putWord32be b
    for_ o put

defaultSynthDef :: SynthDef
defaultSynthDef = SynthDef
  [ GraphDef
    "default"
    [-0.4,0.0,0.4,0.3,4000.0,5000.0,2500.0,3200.0,1.0,1.0e-2,0.7,2.0]
    [0.0,440.0,0.1,0.0,1.0]
    [("out",0),("freq",1),("amp",2),("pan",3),("gate",4)]
    [ UGen "Control" Scalar 0 [] [Scalar]
    , UGen "Control" Control 1 [] [Control,Control,Control,Control]
    , UGen "VarSaw" Audio 0 [(1,0),(-1,1),(-1,3)] [Audio]
    , UGen "BinaryOpUGen" Audio 2 [(2,0),(-1,3)] [Audio]
    , UGen "Linen" Control 0 [(1,3),(-1,9),(-1,10),(-1,3),(-1,11)] [Control]
    , UGen "Rand" Scalar 0 [(-1,0),(-1,1)] [Scalar]
    , UGen "BinaryOpUGen" Control 0 [(1,0),(5,0)] [Control]
    , UGen "VarSaw" Audio 0 [(6,0),(-1,1),(-1,3)] [Audio]
    , UGen "BinaryOpUGen" Audio 2 [(7,0),(-1,3)] [Audio]
    , UGen "Rand" Scalar 0 [(-1,1),(-1,2)] [Scalar]
    , UGen "BinaryOpUGen" Control 0 [(1,0),(9,0)] [Control]
    , UGen "VarSaw" Audio 0 [(10,0),(-1,1),(-1,3)] [Audio]
    , UGen "BinaryOpUGen" Audio 2 [(11,0),(-1,3)] [Audio]
    , UGen "Sum3" Audio 0 [(12,0),(8,0),(3,0)] [Audio]
    , UGen "Rand" Scalar 0 [(-1,4),(-1,5)] [Scalar]
    , UGen "Rand" Scalar 0 [(-1,6),(-1,7)] [Scalar]
    , UGen "XLine" Control 0 [(14,0),(15,0),(-1,8),(-1,1)] [Control]
    , UGen "LPF" Audio 0 [(13,0),(16,0)] [Audio]
    , UGen "BinaryOpUGen" Audio 2 [(17,0),(4,0)] [Audio]
    , UGen "Pan2" Audio 0 [(18,0),(1,2),(1,1)] [Audio,Audio]
    , UGen "OffsetOut" Audio 0 [(0,0),(19,0),(19,1)] []
    ]
    []
  ]

-- -----------------------------------------------------------------------------

getPascalString :: Get ByteString
getPascalString = getByteString . fromIntegral =<< getWord8

putPascalString :: ByteString -> Put
putPascalString s = do
  putWord8 . fromIntegral . ByteString.length $ s
  putByteString s

getList :: Integral n => Get n -> Get a -> Get [a]
getList f g = (`replicateM` g) . fromIntegral =<< f

putList :: Integral n => [a] -> (n -> Put) -> (a -> Put) -> Put
putList xs f g = do
  f . fromIntegral . length $ xs
  for_ xs g

getUnboxedVector :: (Unbox a, Integral n)
                 => Get n -> Get a -> Get (Unboxed.Vector a)
getUnboxedVector f g = (`Unboxed.Vector.replicateM` g) . fromIntegral =<< f

putUnboxedVector :: (Integral n, Unbox a)
                 => Unboxed.Vector a -> (n -> Put) -> (a -> Put) -> Put
putUnboxedVector v f g = do
  f . fromIntegral . Unboxed.Vector.length $ v
  Unboxed.Vector.forM_ v g

getVector :: Integral n => Get n -> Get a -> Get (Vector a)
getVector f g = (`Vector.replicateM` g) . fromIntegral =<< f
