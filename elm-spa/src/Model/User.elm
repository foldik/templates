module Model.User exposing (User(..), getRole)

import Model.Role as Role


type User
    = User String Role.Role


getRole : User -> Role.Role
getRole (User name role) =
    role
