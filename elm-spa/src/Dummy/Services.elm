module Dummy.Services exposing (Service, ServiceState(..), list, stateToString)


type alias Service =
    { name : String
    , version : String
    , state : ServiceState
    , instances : Int
    , url : String
    }


type ServiceState
    = Starting
    | FailedToStart
    | Running
    | Stopped
    | Error


stateToString : ServiceState -> String
stateToString state =
    case state of
        Starting ->
            "Starting"

        FailedToStart ->
            "Failed to start"

        Running ->
            "Running"

        Stopped ->
            "Stopped"

        Error ->
            "Error"


list : List Service
list =
    [ { name = "new-project", version = "0.1.0", state = Starting, instances = 1, url = "http://localhost:9000/projects/new" }
    , { name = "new-user", version = "0.4.1", state = Running, instances = 2, url = "http://localhost:9000/users/new" }
    , { name = "new-meeting", version = "0.7.1", state = Running, instances = 2, url = "http://localhost:9000/meetings/new" }
    ]
