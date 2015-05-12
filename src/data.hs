module Rikka.Data where

import Data.Map.Strict 

data User = User 
    { nickname :: String
    , password :: String }

data World = World
    { users :: Map String User}

voidWorld :: World
voidWorld = World empty

addUser :: User -> World -> World
addUser user (World users) =
    World (insert (nickname user) user users)
