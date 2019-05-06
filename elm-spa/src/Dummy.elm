module Dummy exposing (adminUser, simpleUser, user)

import Model.Role as Role
import Model.User as User


user : Maybe User.User
user =
    adminUser


nope : Maybe User.User
nope =
    Nothing


adminUser : Maybe User.User
adminUser =
    Just (User.User "Adrienn" Role.Admin)


simpleUser : Maybe User.User
simpleUser =
    Just (User.User "Kristóf" Role.SimpleUser)
