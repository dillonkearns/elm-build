module UserAccess exposing (Role(..), ageGroup, canDelete, canEdit, canView, roleLevel)

{-| User access control logic.
-}


type Role
    = Guest
    | Member
    | Moderator
    | Admin


roleLevel : Role -> Int
roleLevel role =
    case role of
        Guest ->
            0

        Member ->
            1

        Moderator ->
            2

        Admin ->
            3


canView : Role -> Bool
canView role =
    roleLevel role >= 0


canEdit : Role -> Bool
canEdit role =
    roleLevel role >= 1


canDelete : Role -> Bool
canDelete role =
    roleLevel role >= 2


ageGroup : Int -> String
ageGroup age =
    if age < 13 then
        "child"

    else if age < 18 then
        "teen"

    else if age < 65 then
        "adult"

    else
        "senior"
