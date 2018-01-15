module Utils exposing (sliceInto, intersperseInverted)

import List.Extra


sliceInto : Int -> Int -> List a -> List a
sliceInto oldIdx newIdx xs =
    case List.Extra.getAt oldIdx xs of
        Just x ->
            let
                ( hx, tx ) =
                    xs
                        |> List.Extra.splitAt newIdx

                ( hx_, tx_ ) =
                    if oldIdx <= newIdx then
                        ( hx |> List.Extra.removeAt oldIdx, tx )
                    else
                        ( hx, tx |> List.Extra.removeAt (oldIdx - newIdx) )
            in
                hx_ ++ (x :: tx_)

        Nothing ->
            xs


intersperseInverted : a -> List a -> List a
intersperseInverted x list =
    case list of
        [] ->
            []

        list ->
            x :: (List.foldr (\y xs -> y :: x :: xs) [] list)
