module Murmur3 exposing (hashString)

{-| Murmur 3 hash function for hashing strings

@docs hashString

-}

import Bitwise exposing (..)
import String
import Char


{-| Takes a seed and a string. Produces a hash (integer).
Given the same seed and string, it will always produce the same hash.

    hashString 1234 "Turn me into a hash" == 4138100590
-}
hashString : Int -> String -> Int
hashString seed str =
    str
        |> String.foldl hashFold ( 0, seed, 0 )
        |> finalize (String.length str)


hashFold : Char -> ( Int, Int, Int ) -> ( Int, Int, Int )
hashFold c ( shift, seed, hash ) =
    let
        res =
            0xFF
                |> and (Char.toCode c)
                |> shiftLeftBy shift
                |> (flip or) hash
    in
        if shift >= 24 then
            let
                newHash =
                    res |> mix seed |> step
            in
                ( 0, newHash, 0 )
        else
            ( shift + 8, seed, res )


finalize : Int -> ( Int, Int, Int ) -> Int
finalize strLength ( _, seed, hash ) =
    let
        acc =
            if hash /= 0 then
                mix seed hash
            else
                seed

        h1 =
            Bitwise.xor acc strLength

        h2 =
            Bitwise.xor h1 (shiftRightZfBy 16 h1)
                |> mur 0x85EBCA6B

        h3 =
            Bitwise.xor h2 (shiftRightZfBy 13 h2)
                |> mur 0xC2B2AE35

        h4 =
            Bitwise.xor h3 (shiftRightZfBy 16 h3)
    in
        shiftRightZfBy 0 h4


mix : Int -> Int -> Int
mix h1 h2 =
    let
        k1 =
            mur 0xCC9E2D51 h2

        k2 =
            or (shiftLeftBy 15 k1) (shiftRightZfBy 17 k1)
                |> mur 0x1B873593
    in
        Bitwise.xor h1 k2


mur : Int -> Int -> Int
mur c h =
    0xFFFF
        |> and h
        |> (*) c
        |> (+)
            (h
                |> shiftRightZfBy 16
                |> (*) c
                |> and 0xFFFF
                |> shiftLeftBy 16
            )
        |> and 0xFFFFFFFF


step : Int -> Int
step acc =
    let
        h1 =
            or (shiftLeftBy 13 acc) (shiftRightZfBy 19 acc)
                |> mur 5
    in
        h1
            |> shiftRightZfBy 16
            |> (+) 0xE654
            |> and 0xFFFF
            |> shiftLeftBy 16
            |> (+) ((and h1 0xFFFF) + 0x6B64)
