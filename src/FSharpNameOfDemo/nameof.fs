namespace FSharpNameOfDemo

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

open Aether

///
type AsyncResult<'TSuccess, 'TFailure> = Async<Result<'TSuccess, 'TFailure>>

module Async =
  let map f async'= 
    async {
      let! resolved = async'
      return f resolved
    }
  let deferred t = async { return t }

[<AutoOpen>]
module Result =
    let bind f =
      function
      | Result.Ok t' -> f t'
      | Error error -> Error error

    let combine result1 result2 joinOk joinError =
      match result1, result2 with
      | Result.Ok ok1, Result.Ok ok2 -> Result.Ok (joinOk ok1 ok2)
      | Error err1, Error err2 -> Error (joinError err1 err2)
      | Error err, _ | _, Error err -> Error (joinError err err)

    type ResultBuilder () =
      member __.Bind (result, f) = bind result f
      member __.Return ok = Result.Ok ok
      member __.ReturnFrom result = result

    let resultOf = ResultBuilder ()

    let isOk = 
      function
      | Result.Ok _ -> true
      | _ -> false

    let isError result = (not << isOk) result
    module Operators =
      let (>>=) f result = Result.bind result f

    open Operators

    let traverse (traverser: 'a -> Result<'a, _>) (results: 'a list) =
      let return' = Result.Ok
      let init = return' []
      let folder head tail =
        (traverser head) >>= (fun h ->
        tail >>= (fun t ->
        return' (h :: t)))
      List.foldBack folder results init
module AsyncResult =
  let bind f asyncResult =
    async {
      let! result = asyncResult
      match result with
      | Result.Ok ok -> return! f ok
      | Error error -> return Error error
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

  type SynExpr with
    static member paren =
      (function
      | SynExpr.Paren (expr=expr) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.Paren (_, l, r, range) -> SynExpr.Paren (expr, l, r, range)
        | synExpr -> synExpr)
    static member quote =
      (function
      | SynExpr.Quote (operator=operator; quotedSynExpr=quotedSynExpr) -> Some (operator, quotedSynExpr)
      | _ -> None),
      (fun (operator, quotedSynExpr) -> 
        function
        | SynExpr.Quote (_, isRaw, _, isFromQueryExpression, range) -> SynExpr.Quote (operator, isRaw, quotedSynExpr, isFromQueryExpression, range)
        | synExpr -> synExpr)
  [<RequireQualifiedAccess>]
  module SynExpr =
    let (|ParenSynExpr|_|) = Optic.get SynExpr.paren

    let (|QuoteSynExpr|_|) = Optic.get SynExpr.quote
    let rec mapNameOf synExpr : Result<SynExpr, NameOfError> =
      resultOf {
        match synExpr with
        | ParenSynExpr synExpr' -> return! applyNameOf synExpr SynExpr.paren synExpr'
        | QuoteSynExpr (operator, quoted) -> 
          let result = mapNameOfQuote (operator, quoted)
          if isOk result
            then
              let (Result.Ok (op, q)) = result
              return (setQuoteExpr synExpr (op, q))
            else
              let (Result.Error err) = result
              return! Error err
      }
    and applyNameOf synExpr prism synExpr' = 
      synExpr'
      |> mapNameOf
      |> Result.map (Optic.set prism synExpr)
    and setQuoteExpr expr (op, q) = Optic.set SynExpr.quote (op, q) expr
    and mapNameOfQuote (operator, quoted): Result<SynExpr * SynExpr, NameOfError> = 
         // FIXME: why wont this work?
        let operator = mapNameOf operator
        let quoted = mapNameOf quoted
        combine 
          operator 
          quoted
          (fun op q -> (op, q))
          (fun e _ -> e)

    
    // let mapNameOf synExpr =
    //   resultOf {
    //     match synExpr with
    //     | SynExpr.Paren (expr, l, r, range) -> 
    //   }

  // FIXME: amke me into recursivelet functions
  // type SynExpr with
    /// Only accounts for usage in Expressions, not Modules, types or namespaces, attributes, patterns etc
    // member synExpr.MapNameOf () =
        // Result.resultOf {
        //     match synExpr with
        //     | SynExpr.Paren (expr, l, r, range) -> 
        //         return! SynExpr.ReconstructWithNameOf (fun expr -> SynExpr.Paren (expr, l, r, range)) expr
        //     | SynExpr.Quote (operator, isRaw, quotedSynExpr, isFromQueryExpression, range) -> 
        //         let! operator = operator.MapNameOf ()
        //         let! quotedSynExpr = quotedSynExpr .MapNameOf ()
        //         return! SynExpr.Quote (operator, isRaw, quotedSynExpr, isFromQueryExpression, range)
        //     | SynExpr.Typed (expr=expr) -> return! mapNameof expr
        //     | SynExpr.Tuple (exprs, commaRanges, range) -> 
        //         return! mapToCollection (fun exprs -> SynExpr.Tuple (exprs, commaRanges, range)) exprs
        //     | SynExpr.StructTuple (exprs, commaRanges, range) -> 
        //         return! mapToCollection (fun exprs -> SynExpr.StructTuple (exprs, commaRanges, range)) exprs
        //     | SynExpr.ArrayOrList (isList, exprs, range) -> 
        //         return! mapToCollection (fun exprs -> SynExpr.ArrayOrList (isList, exprs, range)) exprs
        //     // | SynExpr.ObjExpr (
        //     | SynExpr.App (_, false, functionExpression, argumentExpression, _) -> 
        //         match functionExpression with
        //         | SynExpr.Ident ident when ident.IsNameOfOperator() -> 
        //         return! nameof argumentExpression
        //         | _ -> return synExpr
        //     | _ -> return synExpr
        // }

  let getProjectOptionsResult = 
    function
    | (options, []) -> Result.Ok options
    | (_, errors) -> Error errors

  let nameof = 
    function
    | SynExpr.Ident ident -> Result.Ok (ident.NameOf())
    | SynExpr.LongIdent (_, idents, _, _) -> 
      let lid = idents.Lid
      match lid with
      | [ident] | _::[ident] -> Result.Ok (ident.NameOf())
      | _ -> Error FailedToParseLongId
    | _ -> Error NotAnIdentifier

  let private getSynModuleDecls (SynModuleOrNamespace (decls=decls)) = decls

  type LetBindingOrDoExpr =
    private 
    | LetBinding of seq<SynBinding>
    | DoExpr of SequencePointInfoForBinding * SynExpr

  let private (|LetExpr|_|) =
    function
    | SynModuleDecl.Let (_, synBindings, _) -> Some (LetBinding synBindings)
    | _ -> None

  let private (|DoBindings|_|) =
    function
    | SynModuleDecl.DoExpr (seqPointInfo, synExpr, _) -> Some (DoExpr (seqPointInfo, synExpr))
    | _ -> None
  
  let private onlyLetOrDoBindings synModuleDecls =
    synModuleDecls
    |> Seq.fold 
      (fun bindings nextBinding -> 
         match nextBinding with
         | LetExpr letBindings -> letBindings :: bindings
         | DoBindings doBindings -> doBindings :: bindings
         |_ -> bindings)
      []
    |> Seq.ofList

  // FIXME: The syntax is let x = f i in let y = g x in y
  let private getLet'sExpr (Binding (expr=expr)) = expr

  // FIXME: map nameof operator over lett and do constructions 
  let private applyToLetOrDo =
    function
    | LetBinding synBindings -> 
      synBindings
      |> Seq.map getLet'sExpr
    // | DoExpr (seqPoint, synExpr)

  let transformLetOrDoBindings letOrDoBindings =
    letOrDoBindings
    |> Seq.map applyToLetOrDo

  let private transformWithNameOf synModuleOrNamespaces =
    synModuleOrNamespaces
    |> Seq.map (getSynModuleDecls >> onlyLetOrDoBindings >> transformLetOrDoBindings) 

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
      let! modulesOrNamespaces = parseFile file input parsingOptions
      return transformWithNameOf modulesOrNamespaces
    }
    
  let runNameofOnFileAndInput (file, input) =
    getTree(file, input) 
    |> Async.RunSynchronously

// TODO: Read input