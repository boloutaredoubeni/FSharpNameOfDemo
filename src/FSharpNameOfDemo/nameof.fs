namespace FSharpNameOfDemo

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Nameof
open Nameof
open Nameof

type AsyncResult<'TSuccess, 'TFailure> = Async<Result<'TSuccess, 'TFailure>>

[<AutoOpen>]
module Nameof =

  let private checker = FSharpChecker.Create() 

  let [<Literal>] NAMEOF = "nameof"

  type Ident with 
    member ident.IsNameOfOperator () = ident.idText = NAMEOF
    member ident.NameOf () = 
      let name = ident.idText
      let range = ident.idRange
      let stringConst = SynConst.String (name, range)
      SynExpr.Const (stringConst, range)

  type NameOfError =  FailedToParseLongId | NotAnIdentifier

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
    match synExpr with
    | SynExpr.App (_, false, functionExpression, argumentExpression, _) -> 
      match functionExpression with
      | SynExpr.Ident ident when ident.IsNameOfOperator() -> 
        do nameof argumentExpression |> ignore
        // FIXME: Do inplace refactor
        synExpr
      | _ -> synExpr
    | _ -> synExpr
  
  let private getProjectOptionsFromScript file input =
    async {
      let! (projectOptions, errors) = checker.GetProjectOptionsFromScript (file, input)
      match errors with
      | [] -> return Result.Ok projectOptions
      | _ -> return Error errors
    }

  let private getParsingOptionsFromProjectOptions projectOptions =
     let (parsingOptions, errors) = checker.GetParsingOptionsFromProjectOptions (projectOptions)
     match errors with
     | [] -> Result.Ok parsingOptions
     | _ -> Error errors

  let private parseFile file input parsingOptions =
    async {
      let! parseFileResults = checker.ParseFile(file, input, parsingOptions)
      if parseFileResults.ParseHadErrors
        then return Error parseFileResults.Errors
        else 
          match parseFileResults.ParseTree with
          | Some tree -> return Result.Ok tree 
          | _ -> return Error parseFileResults.Errors
    }

  let private getUntypedTree(file, input) =
    async {
      // TODO: make an asyncresult computation expr
      let! (Result.Ok projectOptions) = getProjectOptionsFromScript file input
      let (Result.Ok parsingOptions) = getParsingOptionsFromProjectOptions projectOptions
      let! parseFileResults = parseFile file input parsingOptions
      return parseFileResults
    }
    
  let runNameofOnFileAndInput (file, input) =
    getUntypedTree(file, input) 
    |> Async.RunSynchronously
    // |> 