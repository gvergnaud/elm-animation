module Utils.List exposing (..)

range : Int -> Int -> List Int
range start end =
  if start == end
    then [end]
    else [start] ++ range (start + 1) end
