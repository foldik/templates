module Dummy.Users exposing (User, list)


type alias User =
    { id : Int
    , firstName : String
    , lastName : String
    , userName : String
    , email : String
    }


list : List User
list =
    [ { id = 1, firstName = "Kristóf", lastName = "Somogyi", userName = "kristof", email = "kristof@nope.com" }
    , { id = 2, firstName = "Adrienn", lastName = "Gyarmati", userName = "adrienn", email = "adrienn@nope.com" }
    , { id = 3, firstName = "Barbara", lastName = "Horti", userName = "barbara", email = "barbara@nope.com" }
    , { id = 4, firstName = "Beáta", lastName = "Hatvani", userName = "beata", email = "beata@nope.com" }
    , { id = 5, firstName = "Dorina", lastName = "Pesti", userName = "dorina", email = "dorina@nope.com" }
    , { id = 6, firstName = "Ágnes", lastName = "Győri", userName = "agnes", email = "agnes@nope.com" }
    , { id = 7, firstName = "Nikolett", lastName = "Pécsi", userName = "nikolett", email = "nikolett@nope.com" }
    , { id = 8, firstName = "Noémi", lastName = "Mecseki", userName = "noemi", email = "noemi@nope.com" }
    , { id = 9, firstName = "Lilla", lastName = "Debreceni", userName = "lilla", email = "lilla@nope.com" }
    , { id = 10, firstName = "Katalin", lastName = "Miskolci", userName = "katalin", email = "katalin@nope.com" }
    ]
