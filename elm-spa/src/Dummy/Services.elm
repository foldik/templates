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
    [ { name = "hello-world"
      , version = "0.1.0"
      , state = Starting
      , instances = 2
      , url = "http://localhost:9000/hello-world"
      }
    , { name = "users"
      , version = "0.2.1"
      , state = FailedToStart
      , instances = 1
      , url = "http://localhost:9000/users"
      }
    , { name = "tokens"
      , version = "0.9.1"
      , state = Running
      , instances = 2
      , url = "http://localhost:9000/tokens"
      }
    , { name = "git"
      , version = "1.0.2"
      , state = Running
      , instances = 10
      , url = "http://localhost:9000/git"
      }
    , { name = "drone"
      , version = "0.8.2"
      , state = Running
      , instances = 11
      , url = "http://localhost:9000/drone"
      }
    , { name = "teamcity"
      , version = "2.1.6"
      , state = Running
      , instances = 7
      , url = "http://localhost:9000/teamcity"
      }
    , { name = "jenkins"
      , version = "2.1.1"
      , state = Error
      , instances = 1
      , url = "http://localhost:9000/jenkins"
      }
    ]
