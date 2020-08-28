module ApiClientInfo exposing (ApiClientInfo, defaultApiClientInfo)


type alias ApiClientInfo =
    { id : String, secret : String }


defaultApiClientInfo =
    { id = "", secret = "" }
