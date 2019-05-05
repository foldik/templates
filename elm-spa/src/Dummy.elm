module Dummy exposing (adminUser, simpleUser)

import Model.Role as Role
import Model.User as User


adminUser : User.User
adminUser =
    User.User "Adrienn" Role.Admin


simpleUser : User.User
simpleUser =
    User.User "Kristóf" Role.SimpleUser
