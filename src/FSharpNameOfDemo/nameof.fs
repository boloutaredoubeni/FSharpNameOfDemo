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

module Option =

  module Operators =
    let (>>=) option f = Option.bind f option

  open Operators
  let traverse traverser options =
    let folder head tail =
      (traverser head) >>= (fun h ->
      tail >>= (fun t ->
      Some (h :: t)))
    List.foldBack folder options (Some [])

  let sequence (options: list<option<_>>) = traverse id options
  

module Result =

  let combine result1 result2 joinOk joinError =
    match result1, result2 with
    | Result.Ok ok1, Result.Ok ok2 -> Result.Ok (joinOk ok1 ok2)
    | Error err1, Error err2 -> Error (joinError err1 err2)
    | Error err, _ | _, Error err -> Error (joinError err err)

  module Operators =
    let (>>=) result f = Result.bind f result
  
  open Operators

  type ResultBuilder () =
    member __.Bind (result, f) = result >>= f
    member __.Return ok = Result.Ok ok
    member __.ReturnFrom result = result

  let resultOf = ResultBuilder ()

  let isOk = 
    function
    | Result.Ok _ -> true
    | _ -> false

  let isError result = (not << isOk) result

  let traverse (traverser: 'a -> Result<'a, _>) (results: 'a list) =
    let folder head tail =
      (traverser head) >>= (fun h ->
      tail >>= (fun t ->
      Result.Ok (h :: t)))
    List.foldBack folder results (Result.Ok [])
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

  let StringConst (string, range) =
    let constString = (SynConst.String (string, range))
    SynExpr.Const (constString, range)

  let [<Literal>] NAMEOF = "nameof"

  type Ident with 
    member ident.IsNameOfOperator () = ident.idText = NAMEOF
    member ident.NameOf () = 
      let name = ident.idText
      let range = ident.idRange
      StringConst (name, range)

  type NameOfError = FailedToParseLongId | NotAnIdentifier

  type SynExpr with
    // FIXME: join all of these opticals and DRY them up
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
    static member typed =
      (function
      | SynExpr.Typed (expr=expr) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.Typed (_, typeName, range) -> SynExpr.Typed (expr, typeName, range)
        | synExpr -> synExpr)

    static member tuple =
      (function
      | SynExpr.Tuple (exprs=exprs) -> Some exprs
      | _ -> None),
      (fun exprs ->
        function
        | SynExpr.Tuple (_, commaRanges, range) -> SynExpr.Tuple (exprs, commaRanges, range)
        | synExpr -> synExpr)

    static member structTuple =
      (function
      | SynExpr.Tuple (exprs=exprs) -> Some exprs
      | _ -> None),
      (fun exprs ->
        function
        | SynExpr.Tuple (_, commaRanges, range) -> SynExpr.Tuple (exprs, commaRanges, range)
        | synExpr -> synExpr)

    static member arrayOrList =
      (function
      | SynExpr.ArrayOrList (exprs=exprs) -> Some exprs
      | _ -> None),
      (fun exprs ->
        function
        | SynExpr.ArrayOrList (isList, _, range) -> SynExpr.ArrayOrList (isList, exprs, range)
        | synExpr -> synExpr)
    
    static member record =
      (function
      | SynExpr.Record (recordFields=recordFields) ->
        recordFields 
        |> Seq.map (fun (_, maybeSynExpr, _) -> maybeSynExpr)
        |> Seq.toList
        |> Option.sequence) //,
      // (fun exprs ->)
  
  [<RequireQualifiedAccess>]
  module SynExpr =
    let (|ParenSynExpr|_|) = Optic.get SynExpr.paren

    let (|QuoteSynExpr|_|) = Optic.get SynExpr.quote

    let (|TypedSynExpr|_|) = Optic.get SynExpr.typed

    let (|TupleSynExpr|_|) = Optic.get SynExpr.tuple

    let (|StructTupleSynExpr|_|) = Optic.get SynExpr.structTuple

    let (|ArrayOrListSynExpr|_|) = Optic.get SynExpr.arrayOrList

    let rec mapNameOf synExpr : Result<SynExpr, NameOfError> =
      Result.resultOf {
        match synExpr with
        | ParenSynExpr synExpr' -> return! applyNameOf synExpr SynExpr.paren synExpr'
        | QuoteSynExpr (operator, quoted) ->
          let! ok = (operator, quoted) |> mapNameOfQuote
          let (operator, quoted) = ok
          return setQuoteExpr synExpr (operator, quoted)
        | TupleSynExpr synExprs -> return! traverseWithNameOf synExpr SynExpr.tuple synExprs
        | StructTupleSynExpr synExprs -> return! traverseWithNameOf synExpr SynExpr.structTuple synExprs
        | ArrayOrListSynExpr synExprs -> return! traverseWithNameOf synExpr SynExpr.arrayOrList synExprs
        | TypedSynExpr synExpr' -> return! applyNameOf synExpr SynExpr.typed synExpr'
        // TODO: add other cases
        | synExpr -> return synExpr
      }
    and applyNameOf synExpr (prism: Prism<SynExpr, SynExpr>) synExpr' = 
      synExpr'
      |> mapNameOf
      |> Result.map (Optic.set prism synExpr)
    and setQuoteExpr expr (op, q) = Optic.set SynExpr.quote (op, q) expr
    and mapNameOfQuote (operator, quoted) = 
        let operator = mapNameOf operator
        let quoted = mapNameOf quoted
        Result.combine 
          operator 
          quoted
          (fun op q -> (op, q))
          (fun e _ -> e)
    and traverseWithNameOf synExpr (prism: Prism<SynExpr, list<SynExpr>>) exprs = 
      Result.resultOf {
        let! exprs = Result.traverse mapNameOf exprs
        return Optic.set prism exprs synExpr
      }

    
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

  let [<Literal>] NAMEOF_NULL = "null"

  let nameof = 
    function
    | SynExpr.Ident ident -> Result.Ok (ident.NameOf())
    | SynExpr.LongIdent (_, idents, _, _) -> 
      let lid = idents.Lid
      match lid with
      | [ident] | _::[ident] -> Result.Ok (ident.NameOf())
      | _ -> Error FailedToParseLongId
    | SynExpr.Null range -> Result.Ok (StringConst (NAMEOF_NULL, range) )
    | _ -> Error NotAnIdentifier

  let private getSynModuleDecls (SynModuleOrNamespace (decls=decls)) = decls

  // FIXME: also add class methods
  type Bindings =
    private 
    | LetBinding of seq<SynBinding>
    | DoExpr of SequencePointInfoForBinding * SynExpr
    | ClassMember
    | InterfaceMember

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