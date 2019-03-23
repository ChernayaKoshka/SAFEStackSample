namespace Shared

type Mood =
    | Good
    | SoSo
    | Poor
    static member FromStr(str : string) =
        match str.ToUpper() with
        | "GOOD" -> Good
        | "SOSO" -> SoSo
        | "POOR" -> Poor

type Comment =
    { Author : string
      CommentText : string
      Mood : Mood option }

module Validation =
    open System

    let validateCommentText (comment : Comment) =
        let trimmedText = comment.CommentText.Trim()
        if String.IsNullOrWhiteSpace(trimmedText) then
            Error "Comment field must not be blank."
        elif trimmedText.Length > 144 then
            Error
                (sprintf
                     "Comment must be fewer than 144 characters. Your comment is %d characters over the limit."
                     (trimmedText.Length - 144))
        else Ok { comment with CommentText = trimmedText }

    let validateAuthor (comment : Comment) =
        let trimmedText = comment.Author.Trim()
        if String.IsNullOrWhiteSpace(trimmedText) then
            Error "Author field must not be blank."
        elif trimmedText.Length > 20 then
            Error
                (sprintf
                     "Author must be fewer than 20 characters. Your author is %d characters over the limit."
                     (trimmedText.Length - 20))
        elif trimmedText.Length < 5 then
            Error "Author must be at least 5 characters."
        else Ok { comment with Author = trimmedText }

    let validateMood (comment : Comment) =
        match comment.Mood with
        | Some _ -> Ok comment
        | None -> Error "Please select a mood."

    let validateComment (comment : Comment) =
        validateAuthor comment
        |> Result.bind validateCommentText
        |> Result.bind validateMood
