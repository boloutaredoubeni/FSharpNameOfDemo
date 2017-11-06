#!/usr/bin/env fsharpi
// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I "../../packages/FSharp.Compiler.Service/lib/net45/"
#r "FSharp.Compiler.Service.dll"

open System
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

module Demo =
  let nameof (t: Ident) = t.idText

  let rec collectNamesFromPatterns syntax names = 
    match syntax with
    | SynPat.Named (syntax', name, _, _, _) ->
      let namesInPattern = collectNamesFromPatterns syntax' names
      nameof name :: namesInPattern @ names
    | SynPat.LongIdent (LongIdentWithDots (indentifiers, _), _, _, _, _, _) ->
       [ for id in indentifiers -> nameof id ] @ names
    | _ -> names

  let rec collectNamesFromExpression expression names =
    match expression with
    | SynExpr.IfThenElse (cond, t, f, _, _, _, _) ->
      let falseBranch expression names = Option.fold (fun names' expression' -> collectNamesFromExpression expression' names') names expression
      names
      |> collectNamesFromExpression cond
      |> collectNamesFromExpression t
      |> falseBranch f
    | SynExpr.LetOrUse (_, _, bindings, body, _) ->
      [ for binding in bindings do
          let (Binding (_, _, _, _, _,_, _, pattern, _, init, _, _)) = binding
          let names = collectNamesFromPatterns pattern names
          yield! collectNamesFromExpression init names ]
      |> collectNamesFromExpression body 
    | _ -> names

  let rec collectNamesFromDeclarations declarations names =
    [ for declaration in declarations do
      match declaration with 
      | SynModuleDecl.Let(_, bindings, _) ->
        yield! 
          [ for binding in bindings do
            let (Binding(_, _, _, _, _, _, _, pattern, _, body, _, _)) = binding
            let names = collectNamesFromPatterns pattern names
            yield! collectNamesFromExpression body names ]
      | _ -> yield! names ]

  let rec collectNamesFromModulesAndNamespaces modulesAndNamespaces names =
    [ for moduleOrNamespace in modulesAndNamespaces do
      let (SynModuleOrNamespace (_, _, _, declarations, _, _, _, _)) = moduleOrNamespace
      yield! collectNamesFromDeclarations declarations names ]


  let tupleToResult (ok, errors) =
    match errors with
    | [] -> Result.Ok ok
    | errors -> Result.Error errors

  let checker = FSharpChecker.Create() 
  let getUntypedTree (file, input) =
    async {
      let! (projectOptions, errors) = checker.GetProjectOptionsFromScript (file, input)
      let (parsingOptions, errors) = checker.GetParsingOptionsFromProjectOptions (projectOptions)
      let! parseFileResults = checker.ParseFile(file, input, parsingOptions)
      return parseFileResults.ParseTree.Value
    }

  // Sample input for the compiler service
  let input = """
    let foo() = 
      let msg = "Hello world"
      if true then 
        printfn "%s" msg """
  // File name in Unix format
  let file = "/home/user/Test.fsx"

  exception FsiNotSupportedException

  // Get the AST of sample F# code
  do 
    match getUntypedTree(file, input) |> Async.RunSynchronously with
    | ParsedInput.ImplFile (implFile) -> 
      let (ParsedImplFileInput (_, _, _, _, _, modules, _)) = implFile
      let names = []
      let names = collectNamesFromModulesAndNamespaces modules names
      do printfn "%A" names
    | _ -> raise FsiNotSupportedException
