module BetterFetch
    open Thoth.Json
    open Fable.Import
    open Fable.PowerPack
    open Fable.Core.JsInterop
    open Fable.PowerPack.Fetch

    /// Retrieves data from the specified resource. Returns Ok<Response> on success and Error<Response> on failure.
    let fetch (url: string) (init: RequestProperties list) : JS.Promise<Result<Response, Response>> =
        GlobalFetch.fetch(RequestInfo.Url url, requestProps init)
        |> Promise.map (fun response ->
            if response.Ok
            then Ok response
            else Error response)

    let private sendRecord (url: string) (record:'T) (properties: RequestProperties list) httpMethod : JS.Promise<Result<Response, Response>> =
        let defaultProps =
            [ RequestProperties.Method httpMethod
              requestHeaders [ContentType "application/json"]
              RequestProperties.Body !^(Encode.Auto.toString(0, record))]
        // Append properties after defaultProps to make sure user-defined values
        // override the default ones if necessary
        List.append defaultProps properties
        |> fetch url

    /// Sends a HTTP post with the record serialized as JSON.
    /// This function already sets the HTTP Method to POST sets the json into the body.
    let postRecord<'T> (url: string) (record:'T) (properties: RequestProperties list) : JS.Promise<Result<Response, Response>> =
        sendRecord url record properties HttpMethod.POST