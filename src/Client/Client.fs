module Client

open Fulma.Color
open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.FontAwesome
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Fable.PowerPack
open Fulma
open Shared
open Thoth.Json
open Fable.FontAwesome
open Shared

module Cmd =
    /// Takes a promise that returns a result and will take the corresponding path
    let ofPromiseResult (task : 'a -> JS.Promise<Result<'b, 'c>>) (arg : 'a)
        (ofSuccess : 'b -> 'msg) (ofError : 'c -> 'msg) : Cmd<'msg> =
        let bind dispatch =
            task arg
            |> Promise.mapResult (ofSuccess >> dispatch)
            |> Promise.mapResultError (ofError >> dispatch)
            |> ignore
        [ bind ]

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model =
    { IsLoading : bool
      CurrentMsg : (string * IColor) option
      Comment : Comment
      Comments : Comment list option }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | ErrorMsg of exn
    | StrErrorMsg of string
    | UpdateAuthorText of string
    | UpdateCommentText of string
    | SubmitForm
    | SubmissionComplete of string
    | GetComments
    | GotComments of Result<Comment list, string>
    | SetMood of Mood
    | ClearNotification

let moodColor mood =
    match mood with
    | Good -> IsSuccess
    | SoSo -> IsWarning
    | Poor -> IsDanger

let errorMsg msg = (msg, IsWarning) |> Some
let infoMsg msg = (msg, IsInfo) |> Some

// defines the initial state and initial command (= side-effect) of the application
let init() : Model * Cmd<Msg> =
    { IsLoading = false
      CurrentMsg = None
      Comment =
          { Author = ""
            CommentText = ""
            Mood = None }
      Comments = None }, (Cmd.ofMsg GetComments)

let postComment (comment : Comment) =
    promise {
        let! response = BetterFetch.postRecord "/api/comment" comment []
        match response with
        | Ok response ->
            let! text = response.text()
            printfn "%s" text
            return Ok text
        | Error response ->
            let! text = response.text()
            printfn "%s" text
            return Error text
    }

let getComments() =
    promise {
        let! response = BetterFetch.fetch "/api/comments" []
        match response with
        | Ok response ->
            let! json = response.text()
            return Decode.Auto.fromString<Comment list> (json)
        | Error response ->
            let! errText = response.text()
            return Error errText
    }

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | GetComments ->
        { currentModel with Comments = None },
        Cmd.ofPromise getComments () GotComments ErrorMsg
    | GotComments result ->
        match result with
        | Ok comments ->
            { currentModel with Comments = Some comments }, Cmd.none
        | Error e -> { currentModel with CurrentMsg = errorMsg e }, Cmd.none
    | ErrorMsg e ->
        { currentModel with IsLoading = false
                            CurrentMsg = (sprintf "%A" e |> errorMsg) },
        Cmd.none
    | StrErrorMsg str ->
        { currentModel with IsLoading = false
                            CurrentMsg = (str |> errorMsg) }, Cmd.none
    | SetMood mood ->
        { currentModel with Comment =
                                { currentModel.Comment with Mood = Some mood } },
        Cmd.none
    | UpdateAuthorText text ->
        { currentModel with Comment =
                                { currentModel.Comment with Author = text } },
        Cmd.none
    | UpdateCommentText text ->
        { currentModel with Comment =
                                { currentModel.Comment with CommentText = text } },
        Cmd.none
    | SubmitForm ->
        match Validation.validateComment currentModel.Comment with
        | Ok comment ->
            { currentModel with IsLoading = true },
            (*CurrentMsg = None*) Cmd.ofPromiseResult postComment comment SubmissionComplete
                StrErrorMsg
        | Error msg ->
            { currentModel with CurrentMsg = (msg |> errorMsg) }, Cmd.none
    | SubmissionComplete response ->
        { currentModel with IsLoading = false
                            CurrentMsg = (infoMsg response) },
        Cmd.ofMsg GetComments
    | ClearNotification -> {currentModel with CurrentMsg = None}, Cmd.none
    | _ -> currentModel, Cmd.none

let safeComponents =
    let components =
        span [] [ a [ Href "https://saturnframework.github.io" ]
                      [ str "Saturn" ]
                  str ", "
                  a [ Href "http://fable.io" ] [ str "Fable" ]
                  str ", "
                  a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
                  str ", "
                  a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ] ]
    p [] [ strong [] [ str "SAFE Template" ]
           str " powered by: "
           components ]

let button txt isLoading onClick =
    Button.button [ Button.IsLoading isLoading
                    Button.IsFullWidth
                    Button.Color IsPrimary
                    Button.OnClick onClick ] [ str txt ]

let inputArea placeholder onChange =
    Textarea.textarea [ Textarea.OnChange onChange
                        Textarea.Placeholder placeholder ] []

let onChange action = OnChange(fun e -> action !!e.target?value)
let field input = Field.div [] [ Field.body [] [ input ] ]
let control input = Field.div [] [ input ]

let commentMoodColor (model : Model) (mood : Mood) =
    match model.Comment.Mood with
    | None -> IsWhite
    | Some s when s <> mood -> IsWhite
    | _ -> moodColor mood

let moodIcon =
    function
    | Good -> Fa.Regular.Smile
    | SoSo -> Fa.Regular.Meh
    | Poor -> Fa.Regular.Frown

let moods (model : Model) dispatch =
    let item score =
        Level.item []
            [ Button.a [ Button.Color(commentMoodColor model score)
                         Button.OnClick(fun _ -> dispatch (SetMood score)) ]
                  [ Fa.span [ moodIcon score
                              Fa.IconOption.Size Fa.Fa2x ] [] ] ]
    Level.level [] [ item Good
                     item SoSo
                     item Poor ]

let author (model : Model) dispatch =
    Input.input [ Input.Placeholder "Author!"
                  Input.ValueOrDefault model.Comment.Author
                  Input.Props [ onChange (UpdateAuthorText >> dispatch) ] ]

let comment (model : Model) dispatch =
    Textarea.textarea
        [ Textarea.Placeholder "Comment!"
          Textarea.ValueOrDefault model.Comment.CommentText
          Textarea.Props [ onChange (UpdateCommentText >> dispatch) ] ] []

let formBox model dispatch =
    Box.box' []
        [ control (moods model dispatch)
          field (author model dispatch)
          field (comment model dispatch)

          control
              (button "Submit!" model.IsLoading
                   (fun _ -> SubmitForm |> dispatch)) ]

let loadingLevel _ _ =
    Level.level []
        [ Level.item [ Level.Item.HasTextCentered ]
              [ Fa.span [ Fa.Regular.Hourglass
                          Fa.Spin
                          Fa.Size Fa.Fa4x ] [] ] ]

let commentsBox model dispatch =
    Box.box' [] (match model.Comments with
                 | Some comments ->
                     [ Table.table [ Table.IsFullWidth ]
                           [ thead [] [ tr [] [ th [] [ str "Author" ]
                                                th [] [ str "Mood" ]
                                                th [] [ str "Comment" ] ] ]

                             tbody []
                                 [ for comment in comments ->
                                       tr [ Style [ WordBreak "break-word" ] ]
                                           [ td []
                                                 [ Fa.span
                                                       [ moodIcon
                                                             comment.Mood.Value

                                                         Fa.IconOption.Size
                                                             Fa.Fa2x ] [] ]
                                             td [] [ str comment.Author ]
                                             td [] [ str comment.CommentText ] ] ] ]

                       control
                           (button "Refresh Comments" false
                                (fun _ -> GetComments |> dispatch)) ]
                 | None -> [ loadingLevel model dispatch ])

let infoBox model dispatch =
    match model.CurrentMsg with
    | None -> div [] []
    | Some(msg, color) ->
        Notification.notification [ Notification.Color color ] [
            Notification.delete [ Props [ OnClick (fun _ -> dispatch ClearNotification) ] ] [ ]
            str msg ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
              [ Navbar.Item.div []
                    [ Heading.h2 [] [ str "How are you feeling?" ] ] ]

          Column.column [ Column.Width(Screen.All, Column.Is4)
                          Column.Offset(Screen.All, Column.Is4) ]
              [ infoBox model dispatch
                formBox model dispatch
                commentsBox model dispatch ]

          Footer.footer []
              [ Content.content
                    [ Content.Modifiers
                          [ Modifier.TextAlignment
                                (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]
#if DEBUG

open Elmish.Debug
open Elmish.HMR
#endif


Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif

|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif

|> Program.run
