module PriceCalculatorTests exposing (results, suite)

import Expect
import PriceCalculator exposing (LineItem)
import SimpleTestRunner
import Test exposing (Test, describe, test)


results : String
results =
    SimpleTestRunner.runToString suite


suite : Test
suite =
    describe "PriceCalculator"
        [ describe "subtotal"
            [ test "sums line items" <|
                \_ ->
                    PriceCalculator.subtotal
                        [ { name = "Widget", price = 10.0, quantity = 2 }
                        , { name = "Gadget", price = 15.0, quantity = 1 }
                        ]
                        |> Expect.within (Expect.Absolute 0.01) 35.0
            , test "empty cart is zero" <|
                \_ ->
                    PriceCalculator.subtotal []
                        |> Expect.within (Expect.Absolute 0.01) 0.0
            ]
        , describe "applyDiscount"
            [ test "10% discount" <|
                \_ ->
                    PriceCalculator.applyDiscount 10 100.0
                        |> Expect.within (Expect.Absolute 0.01) 90.0
            , test "no discount when zero" <|
                \_ ->
                    PriceCalculator.applyDiscount 0 100.0
                        |> Expect.within (Expect.Absolute 0.01) 100.0
            ]
        , describe "shippingCost"
            [ test "free shipping for large orders" <|
                \_ ->
                    PriceCalculator.shippingCost 75.0
                        |> Expect.within (Expect.Absolute 0.01) 0.0
            , test "charges shipping for small orders" <|
                \_ ->
                    PriceCalculator.shippingCost 30.0
                        |> Expect.within (Expect.Absolute 0.01) 5.99
            ]
        , describe "total"
            [ test "large order with discount gets free shipping" <|
                \_ ->
                    PriceCalculator.total 10
                        [ { name = "Item", price = 25.0, quantity = 4 } ]
                        |> Expect.within (Expect.Absolute 0.01) 90.0
            , test "small order pays shipping" <|
                \_ ->
                    PriceCalculator.total 0
                        [ { name = "Item", price = 10.0, quantity = 1 } ]
                        |> Expect.within (Expect.Absolute 0.01) 15.99
            ]
        ]
