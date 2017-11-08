namespace FSharpNameOfDemo

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

type AsyncResult<'TSuccess, 'TFailure> = Async<Result<'TSuccess, 'TFailure>>

[<AutoOpen>]
module Nameof =

  let private checker = FSharpChecker.Create() 

  let [<Literal>] NAMEOF = "nameof"

  type Ident with member ident.IsNameOfOperator () = ident.idText = NAMEOF

  type SynExpr with 
    member expr.NameOf () = 
      match expr with
      | SynExpr.Ident ident -> Some ident.idText
      | SynExpr.LongIdent (_, [ident], _, _) | SynExpr.LongIdent (_, _::[ident], _, _) -> Some ident.idText
      | _ -> None

  /// 'Fast is F# AST
  type Namer<'Fast when 'Fast: (member NameOf: unit -> option<string>)> = 'Fast
  // Only accounts for usage in Expressions, not Modules, types or namespaces, attributes, patterns etc
  let inline nameof (ident: Namer<_>): option<string> = ident.NameOf()
  let mapNameof = function
    | SynExpr.App (_, false, functionExpression, argumentExpression, _) -> 
      match functionExpression with
      | SynExpr.Ident ident when ident.IsNameOfOperator() -> 
        // FIXME: construct the new tree
        nameof argumentExpression
      | _ -> None
    | _ -> None 
  
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