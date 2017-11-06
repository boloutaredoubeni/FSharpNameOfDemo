namespace FSharpNameOfDemo


type Result<'TSuccess, 'TFailure> = Ok of 'TSuccess | Error of 'TFailure

type AsyncResult<'TSuccess, 'TFailure> = Async<Result<'TSuccess, 'TFailure>>

[<AutoOpen>]
module Nameof =
  open Microsoft.FSharp.Compiler.SourceCodeServices

  let private checker = FSharpChecker.Create() 

  let nameof _ = "hello"

  let private getProjectOptionsFromScript file input =
    async {
      let! (projectOptions, errors) = checker.GetProjectOptionsFromScript (file, input)
      match errors with
      | [] -> return Ok projectOptions
      | _ -> return Error errors
    }

  let private getParsingOptionsFromProjectOptions projectOptions =
     let (parsingOptions, errors) = checker.GetParsingOptionsFromProjectOptions (projectOptions)
     match errors with
     | [] -> Ok parsingOptions
     | _ -> Error errors

  let private parseFile file input parsingOptions =
    async {
      let! parseFileResults = checker.ParseFile(file, input, parsingOptions)
      if parseFileResults.ParseHadErrors
        then return Error parseFileResults.Errors
        else 
          match parseFileResults.ParseTree with
          | Some tree -> return Ok tree 
          | _ -> return Error parseFileResults.Errors
    }

  let getUntypedTree (file, input) =
    async {
      let! (Ok projectOptions) = getProjectOptionsFromScript file input
      let (Ok parsingOptions) = getParsingOptionsFromProjectOptions projectOptions
      let! parseFileResults = parseFile file input parsingOptions
      return parseFileResults
    }