module Validator exposing (PasswordStrength(..), passwordStrength, validateAge, validateEmail, validateUsername)

{-| Form validation logic — the kind of code that every app has
    and where off-by-one errors actually cause bugs in production.
-}


validateUsername : String -> Result String String
validateUsername username =
    if String.length username < 3 then
        Err "Username must be at least 3 characters"

    else if String.length username > 20 then
        Err "Username must be at most 20 characters"

    else
        Ok username


validateEmail : String -> Result String String
validateEmail email =
    if String.contains "@" email then
        Ok email

    else
        Err "Invalid email address"


validateAge : Int -> Result String Int
validateAge age =
    if age < 13 then
        Err "Must be at least 13 years old"

    else if age > 120 then
        Err "Invalid age"

    else
        Ok age


type PasswordStrength
    = Weak
    | Fair
    | Strong


passwordStrength : String -> PasswordStrength
passwordStrength password =
    let
        len =
            String.length password
    in
    if len < 8 then
        Weak

    else if len < 12 then
        Fair

    else
        Strong
