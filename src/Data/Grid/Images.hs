{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Data.Grid.Images where

import Codec.Picture
import Codec.Picture.Types
import Data.Grid
import GHC.TypeNats
import Data.Proxy
import qualified Data.Vector.Storable as V
import Data.Coerce
import Control.Applicative
import Data.Word
import System.IO.Unsafe
import Data.Functor.Compose

withImageAsGrid
  :: forall a r.
  Pixel a
  => Image a
  -> (forall w h. (Dimensions [w, h], KnownNat w, KnownNat h) => Grid [w, h] a -> r)
  -> r
withImageAsGrid img f =
  withKnownNat widthNat $ \w -> withKnownNat heightNat $ \h -> f $ imageToGrid w h img
  where
    widthNat = someNatVal . fromIntegral $ imageWidth img

    heightNat = someNatVal . fromIntegral $ imageHeight img

imageToGrid :: (Pixel a, KnownNat w, KnownNat h) => Proxy w -> Proxy h -> Image a -> Grid [w, h] a
imageToGrid _ _ img = tabulate (\(Coord [x, y]) -> pixelAt img x y)

gridToImage :: forall w h a. (Pixel a, KnownNat w, KnownNat h) => Grid [w, h] a -> Image a
gridToImage g = generateImage (\x y -> index g (Coord [x, y])) w h
  where
    w = fromIntegral . natVal $ Proxy @w

    h = fromIntegral . natVal $ Proxy @h

withKnownNat :: SomeNat -> (forall n. KnownNat n => Proxy n -> r) -> r
withKnownNat (SomeNat p) v = v p

imageCreator :: Image PixelRGB8
imageCreator = generateImage pixelRenderer 250 250
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

pixelGrid :: Grid [w, h] PixelRGB8
pixelGrid = withImageAsGrid imageCreator coerce

withGrid :: (Pixel a, Pixel b)
              => (forall x y. Dimensions [x, y]
                  => Grid [x, y] a -> Grid [x, y] b)
              -> Image a
              -> Image b
withGrid f img = withImageAsGrid img (gridToImage . f)

withGridD :: (forall x y a. (Pixel a, Dimensions [x, y])
                  => Grid [x, y] a -> Grid [x, y] a)
              -> DynamicImage
              -> DynamicImage
withGridD f = dynamicPixelMap (withGrid f)

runGauss :: Image Word8 -> Image Word8
runGauss = withGrid (fmap round . gauss . fmap fromIntegral)

gauss :: (Dimensions dims) => Grid dims Double -> Grid dims Double
gauss = autoConvolute omitBounds gauss'
 where
  gauss' :: Compose (Grid '[10, 10]) Maybe Double -> Double
  gauss' g = (sum g) / fromIntegral (length g)

bw :: Image Word8
bw = extractLumaPlane . convertRGB8 $ unsafePerformIO $ either (error "bad read") id <$> readImage "src-images/girl-in-hat.png"
