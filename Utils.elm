module Utils exposing (transplant, exospersed)

import List.Extra


{-| Move an item from one index to another.

    transplant 1 4 [ 1, 2, 3, 4, 5 ] == [ 1, 3, 4, 5, 2 ]
    transplant 4 0 [ 1, 2, 3, 4, 5 ] == [ 5, 1, 2, 3, 4 ]
-}
transplant : Int -> Int -> List a -> List a
transplant oldIdx newIdx xs =
    case List.Extra.getAt oldIdx xs of
        Just x ->
            if oldIdx <= newIdx then
                let
                    ( hx, tx ) =
                        xs
                            |> List.Extra.splitAt (newIdx + 1)
                in
                    (List.Extra.removeAt oldIdx hx ++ x :: tx)
            else
                let
                    ( hx, tx ) =
                        xs
                            |> List.Extra.splitAt newIdx
                in
                    (hx ++ x :: List.Extra.removeAt (oldIdx - newIdx) tx)

        Nothing ->
            xs


{-| Like interspersed, but add `a` also to the beginning and end of a list.

    exospersed 0 [ 1, 2, 3 ] == [ 0, 1, 0, 2, 0, 3, 0 ]
-}
exospersed : a -> List a -> List a
exospersed x list =
    case list of
        [] ->
            []

        list ->
            x :: (List.foldr (\y xs -> y :: x :: xs) [] list)
