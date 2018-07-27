{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

-- Implementation of IEEE 754 floating point addition
-- TODO: not working yet :)

module FloatAdder where

import Clash.Prelude

addition_normaliser3 :: (BitVector 25, BitVector 8) -> (BitVector 25, BitVector 8)
addition_normaliser3 (man, ex) = (man', ex') where
    (man', ex') = addition_normaliser2 (20, man, ex)

addition_normaliser2 :: (Int, BitVector 25, BitVector 8) -> (BitVector 25, BitVector 8)
addition_normaliser2 (lvl, man, ex) = if lvl > 1
    then if slice d23 d3 man == (1 :: BitVector 21)
        then (man `shiftL` lvl, ex - (fromInteger (toInteger lvl)))
        else addition_normaliser2 (lvl - 1, man, ex)
    else (man, ex)

--f32_is_zero :: (BitVector 23, BitVector 8, Bit) -> Bool
--f32_is_zero (m, e, s) = m == (0 :: BitVector 23) && e == (0 :: BitVector 8)

f32add1 :: (BitVector 32, BitVector 32) -> BitVector 32
f32add1 (a, b) = c where
    c = let a_sign = a ! 31
            b_sign = b ! 31
            a_exponent = slice d30 d23 a
            a_mantissa = 1 ++# slice d22 d0 a
            b_exponent = slice d30 d23 b
            b_mantissa = 1 ++# slice d22 d0 b
        in if ((a_exponent == 255 && a_mantissa == (0 :: BitVector 23)) || (b_exponent == 0) && (b_mantissa == (0 :: BitVector 23)))
            then a
            else if ((b_exponent == 255 && b_mantissa /= 0) || (a_exponent == 0) && (a_mantissa == 0))
                then b
                else if ((a_exponent == 255) || (b_exponent == 255))
                    then ((fromInteger (toInteger a_sign)) `xor` (fromInteger (toInteger b_sign))) ++# (255 :: BitVector 8) ++# (0 :: BitVector 23)
                    else f32add (a, b)

f32add :: (BitVector 32, BitVector 32) -> BitVector 32
f32add (a, b) = c where
    c = let a_sign = a ! 31
            b_sign = b ! 31
            a_exponent = if slice d30 d23 a == 0 then 1 else slice d30 d23 a
            a_mantissa = (if slice d30 d23 a == 0 then 0 else 1) ++# slice d22 d0 a
            b_exponent = if slice d30 d23 b == 0 then 1 else slice d30 d23 b
            b_mantissa = (if slice d30 d23 b == 0 then 0 else 1) ++# slice d22 d0 b
            (cm, ce, cs) = if a_exponent == b_exponent
                then f32addequalexponents (a_mantissa, a_exponent, a_sign, b_mantissa, b_exponent, b_sign)
                else f32addunequalexponents (a_mantissa, a_exponent, a_sign, b_mantissa, b_exponent, b_sign)
            (cm2, ce2) = if cm ! 24 == 1
                then (cm `shiftR` 1, ce + 1)
                else if cm ! 23 /= 1 && ce /= 0
                    then addition_normaliser3 (cm, ce)
                    else (cm, ce)
        in (pack cs) ++# (ce2 :: BitVector 8) ++# ((slice d22 d0 cm2) :: BitVector 23)

f32addequalexponents :: (BitVector 25, BitVector 8, Bit, BitVector 25, BitVector 8, Bit) -> (BitVector 25, BitVector 8, Bit)
f32addequalexponents (a_mantissa, a_exponent, a_sign, b_mantissa, b_exponent, b_sign) = (c_mantissa, c_exponent, c_sign) where
    c_exponent = a_exponent
    c_mantissa = if a_sign == b_sign
        then 1 ++# slice d24 d1 (a_mantissa + b_mantissa) -- Signify to shift?
        else if a_mantissa > b_mantissa
            then a_mantissa - b_mantissa
            else b_mantissa - a_mantissa
    c_sign = if a_sign == b_sign
        then a_sign
        else if a_mantissa > b_mantissa
            then a_sign
            else b_sign

f32addunequalexponents :: (BitVector 25, BitVector 8, Bit, BitVector 25, BitVector 8, Bit) -> (BitVector 25, BitVector 8, Bit)
f32addunequalexponents (a_mantissa, a_exponent, a_sign, b_mantissa, b_exponent, b_sign) = (c_mantissa, c_exponent, c_sign) where
    c_exponent = if a_exponent > b_exponent
        then a_exponent
        else b_exponent
    c_sign = if a_exponent > b_exponent
        then a_sign
        else b_sign
    c_mantissa =
        let tmp_mantissa = if a_exponent > b_exponent
            then b_mantissa `shiftR` (fromInteger (toInteger (a_exponent - b_exponent)))
            else a_mantissa `shiftR` (fromInteger (toInteger (b_exponent - a_exponent)))
        in if a_sign == b_sign
            then if a_exponent > b_exponent
                then a_mantissa + tmp_mantissa
                else b_mantissa + tmp_mantissa
            else if a_exponent > b_exponent
                then a_mantissa - tmp_mantissa
                else b_mantissa - tmp_mantissa
