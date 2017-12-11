#!/usr/bin/env fsharpi
// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I "../../packages/FSharp.Compiler.Service/lib/net45/"
#r "FSharp.Compiler.Service.dll"

type AsyncResult<'TSuccess, 'TFailure> = Async<Result<'TSuccess, 'TFailure>>

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

module Async =
    let map f async'= 
        async {
            let! resolved = async'
            return f resolved
        }
    let deferred t = async { return t }

module Result =
    let bind f result =
        match result with
        | Result.Ok t' -> f t'
        | Error _ -> result

    type ResultBuilder () =
        member __.Bind (result, f) = bind result f
        member __.Return ok = Result.Ok ok
        member __.ReturnFrom result = result

    let resultOf = ResultBuilder ()

module AsyncResult =
    let bind f asyncResult =
        async {
            let! result = asyncResult
            match result with
            | Result.Ok ok -> return! f ok
            | Result.Error error -> return Result.Error error
        }

    type AsyncResultBuilder () =
        member __.Bind (asyncResult: AsyncResult<_, _>, f) = bind f asyncResult
        member __.Return asyncOk = async { return Result.Ok asyncOk }

    let asyncResultOf = AsyncResultBuilder ()


[<AutoOpen>]
///
module Nameof =
  open AsyncResult

  let private checker = FSharpChecker.Create() 

  let [<Literal>] NAMEOF = "nameof"

  type Ident with 
    member ident.IsNameOfOperator () = ident.idText = NAMEOF
    member ident.NameOf () = 
      let name = ident.idText
      let range = ident.idRange
      let stringConst = SynConst.String (name, range)
      SynExpr.Const (stringConst, range)

  type NameOfError = FailedToParseLongId | NotAnIdentifier

  let getProjectOptionsResult = function
    | (options, []) -> Result.Ok options
    | (_, errors) -> Result.Error errors

  let nameof = function
    | SynExpr.Ident ident -> Result.Ok (ident.NameOf())
    | SynExpr.LongIdent (_, idents, _, _) -> 
      let lid = idents.Lid
      match lid with
      | [ident] | _::[ident] -> Result.Ok (ident.NameOf())
      | _ -> Error FailedToParseLongId
    | _ -> Error NotAnIdentifier

  
  /// Only accounts for usage in Expressions, not Modules, types or namespaces, attributes, patterns etc
  let mapNameof synExpr =
    Result.resultOf {
        match synExpr with
        | SynExpr.App (_, false, functionExpression, argumentExpression, _) -> 
          match functionExpression with
          | SynExpr.Ident ident when ident.IsNameOfOperator() -> 
            return! nameof argumentExpression
          | _ -> return synExpr
        | _ -> return synExpr
    }

  
  
  let private getProjectOptionsFromScript file input =
    asyncResultOf {
      let! results = checker.GetProjectOptionsFromScript (file, input) |> Async.map getProjectOptionsResult
      return results
    }

  let private getParsingOptionsFromProjectOptions projectOptions =
     let results = checker.GetParsingOptionsFromProjectOptions (projectOptions)
     getProjectOptionsResult results

  let private parseFile file input parsingOptions =
    let (|ParsedTree|HasErrors|) (parseFileResults: FSharpParseFileResults) =
        if parseFileResults.ParseHadErrors
            then HasErrors (Array.toList parseFileResults.Errors)
            else 
              match parseFileResults.ParseTree with
              | Some (ParsedInput.ImplFile (ParsedImplFileInput (modules=modules))) -> ParsedTree modules
              | _ -> HasErrors (Array.toList parseFileResults.Errors)
    async {
      let! parseFileResults = checker.ParseFile(file, input, parsingOptions)
      match parseFileResults with
      | HasErrors errors -> return Error errors
      | ParsedTree modulesAndNamespaces -> return Result.Ok modulesAndNamespaces
    }

  let private getTree(file, input) =
    asyncResultOf {
      let! projectOptions = getProjectOptionsFromScript file input
      let! parsingOptions = getParsingOptionsFromProjectOptions projectOptions |> Async.deferred
      let! parseFileResults = parseFile file input parsingOptions
      return parseFileResults
    }
    
  let runNameofOnFileAndInput (file, input) =
    getTree(file, input) 
    |> Async.RunSynchronously

// TODO: Parse Modules and Namespaces
// TODO: Read input