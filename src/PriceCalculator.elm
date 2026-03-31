module PriceCalculator exposing (LineItem, applyDiscount, freeShippingThreshold, shippingCost, subtotal, total)

{-| A simple e-commerce price calculator.
-}


type alias LineItem =
    { name : String
    , price : Float
    , quantity : Int
    }


subtotal : List LineItem -> Float
subtotal items =
    items
        |> List.map (\item -> item.price * toFloat item.quantity)
        |> List.sum


applyDiscount : Float -> Float -> Float
applyDiscount discountPercent price =
    if discountPercent > 0 then
        price * (1 - discountPercent / 100)

    else
        price


freeShippingThreshold : Float
freeShippingThreshold =
    50.0


shippingCost : Float -> Float
shippingCost orderSubtotal =
    if orderSubtotal >= freeShippingThreshold then
        0.0

    else
        5.99


total : Float -> List LineItem -> Float
total discountPercent items =
    let
        sub =
            subtotal items

        discounted =
            applyDiscount discountPercent sub

        shipping =
            shippingCost discounted
    in
    discounted + shipping
