module Components.Notification exposing (Notification(..))


type Notification
    = Success String Bool
    | Error String Bool
    | Empty
