namespace FSharpNameOfDemo

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

// TODO: apply to SynExpr.App (nameof, id)

open Aether
open System.Reflection.PortableExecutable
open System.Linq.Expressions

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

  module Operators =
    let (>>=) result f = Result.bind f result

  open Operators

  let fail = Error

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

  let ofOption error =
    function
    | Some t -> Result.Ok t
    | _ -> Error error

  let traverse (traverser: 'a -> Result<'a, _>) (results: 'a list) =
    let folder head tail =
      resultOf {
        let! hd = traverser head
        let! tl = tail
        return hd :: tl
      }
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

  let private checker = FSharpChecker.Create()

  let StringConst (string, range) =
    let constString = (SynConst.String (string, range))
    SynExpr.Const (constString, range)

  let [<Literal>] NAMEOF = "nameof"

  let [<Literal>] NAMEOF_NULL = "null"


  type Ident with
    member ident.IsNameOfOperator () = ident.idText = NAMEOF
    member ident.NameOf () =
      let name = ident.idText
      let range = ident.idRange
      StringConst (name, range)

  type NameOfError = FailedToParseLongId | NotAnIdentifier | NotImplemented

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

    // FIXME: for each collection, order and/or field names need to be maintained

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
      | SynExpr.Record (recordFields=recordFields) -> Some recordFields
      | _ -> None),
      (fun recordFields ->
        function
        | SynExpr.Record (baseInfo, copyInfo, _, range) -> SynExpr.Record (baseInfo, copyInfo, recordFields, range)
        | synExpr -> synExpr)

    static member new' =
      (function
      | SynExpr.New (expr=expr) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.New (isProtected, typeName, _, range) -> SynExpr.New (isProtected, typeName, expr, range)
        | synExpr -> synExpr)

    static member objExpr =
      (function
      | SynExpr.ObjExpr (argOptions=argOptions; bindings=bindings; extraImpls=extraImpls) -> Some (argOptions, bindings, extraImpls)
      | _ -> None),
      (fun (argOptions, bindings, extraImpls) ->
        function
        | SynExpr.ObjExpr (objType, _, _, _, newExprRange, range) -> SynExpr.ObjExpr (objType, argOptions, bindings, extraImpls, newExprRange, range)
        | synExpr -> synExpr)

    static member while' =
      (function
      | SynExpr.While (whileExpr=whileExpr; doExpr=doExpr) -> Some (whileExpr, doExpr)
      | _ -> None),
      (fun (whileExpr, doExpr) ->
        function
        | SynExpr.While (seqPoint, _, _, range) -> SynExpr.While (seqPoint, whileExpr, doExpr, range)
        | synExpr -> synExpr)

    static member for' =
      (function
      | SynExpr.For (identBody=identBody; toBody=toBody; doBody=doBody) -> Some (identBody, toBody, doBody)
      | _ -> None),
      (fun (identBody, toBody, doBody) ->
        function
        | SynExpr.For (forSeqPoint, ident, _, (* isDownTo: ? *) bool', _, _, range) -> SynExpr.For (forSeqPoint, ident, identBody, bool', toBody, doBody, range)
        | synExpr -> synExpr)


    static member forEach =
      (function
      | SynExpr.ForEach (seqExprOnly=seqExprOnly; pat=pat; enumExpr=enumExpr; bodyExpr=bodyExpr) -> Some (seqExprOnly, pat, enumExpr, bodyExpr)
      | _ -> None),
      (fun (seqExprOnly, pat, enumExpr, bodyExpr) ->
        function
        | SynExpr.ForEach (forSeqPoint, _, isFromSource, _, _, _, range) -> SynExpr.ForEach (forSeqPoint, seqExprOnly, isFromSource, pat, enumExpr, bodyExpr, range)
        | synExpr -> synExpr)

    static member arrayOrListofSeq =
      (function
      | SynExpr.ArrayOrListOfSeqExpr (_, expr, range) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.ArrayOrListOfSeqExpr (isArray, _, range) -> SynExpr.ArrayOrListOfSeqExpr (isArray, expr, range)
        | synExpr -> synExpr)

    static member computation =
      (function
      | SynExpr.CompExpr (expr=expr) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.CompExpr (isArrayOrList, isNotNakedRefCell, _, range) -> SynExpr.CompExpr (isArrayOrList, isNotNakedRefCell, expr, range)
        | synExpr -> synExpr)

    static member lambda =
      (function
      | SynExpr.Lambda (args=args; body=body) -> Some (args, body)
      | _ -> None),
      (fun (args, body) ->
        function
        | SynExpr.Lambda (fromMethod, inLambdaSeq, _, _, range) -> SynExpr.Lambda (fromMethod, inLambdaSeq, args, body, range)
        | synExpr -> synExpr)

    static member matchLambda =
      (function
      | SynExpr.MatchLambda (_, _, synMatchClauses, _, _) -> Some synMatchClauses
      | _ -> None),
      (fun synMatchClauses ->
        function
        | SynExpr.MatchLambda (isExnMatch, range0, _, matchSeqPoint, range) -> SynExpr.MatchLambda (isExnMatch, range0, synMatchClauses, matchSeqPoint, range)
        | synExpr -> synExpr)

    static member match' =
      (function
      | SynExpr.Match (expr=expr; clauses=clauses) -> Some (expr, clauses)
      | _ -> None),
      (fun (expr, clauses) ->
        function
        | SynExpr.Match (matchSeqPoint, _, _, isExnMatch, range) -> SynExpr.Match (matchSeqPoint, expr, clauses, isExnMatch, range)
        | synExpr -> synExpr)


    static member do' =
      (function
      | SynExpr.Do (expr=expr) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.Do (range=range) -> SynExpr.Do (expr, range)
        | synExpr -> synExpr)

    static member assert' =
      (function
      | SynExpr.Assert (expr=expr) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.Assert (range=range) -> SynExpr.Assert (expr, range)
        | synExpr -> synExpr)

    static member app =
      (function
      | SynExpr.App (funcExpr=funcExpr; argExpr=argExpr) -> Some (funcExpr, argExpr)
      | _ -> None),
      // FIXME: this doesnot account for the case where 'nameof x = "x"'
      (fun (funcExpr, argExpr) ->
        function
        | SynExpr.App (atomicFlag, isInfix, _, _, range) -> SynExpr.App (atomicFlag, isInfix, funcExpr, argExpr, range)
        | synExpr -> synExpr)

    static member letOrUse =
      (function
      | SynExpr.LetOrUse (bindings=bindings; body=body) -> Some (bindings, body)
      | _ -> None),
      (fun (let', expr) ->
        function
        | SynExpr.LetOrUse (isRecursive, isUse, _, _, range) -> SynExpr.LetOrUse (isRecursive, isUse, let', expr, range)
        | synExpr -> synExpr)

    static member tryWith =
      (function
      | SynExpr.TryWith (tryExpr=tryExpr; withCases=withCases) -> Some (tryExpr,withCases)
      | _ -> None),
      (fun (tryExpr, withCases) ->
        function
        | SynExpr.TryWith (tryRange=tryRange; withRange=withRange; range=range; trySeqPoint=trySeqPoint; withSeqPoint=withSeqPoint) ->
          SynExpr.TryWith (tryExpr, tryRange, withCases, withRange, range, trySeqPoint, withSeqPoint)
        | synExpr -> synExpr)

    static member tryFinally =
      (function
      | SynExpr.TryFinally (tryExpr=tryExpr; finallyExpr=finallyExpr) -> Some (tryExpr, finallyExpr)
      | _ -> None),
      (fun (tryExpr, finallyExpr) ->
        function
        | SynExpr.TryFinally (_, _, range, trySeqPoint, finallySeqPoint) -> SynExpr.TryFinally (tryExpr, finallyExpr, range, trySeqPoint, finallySeqPoint)
        | synExpr -> synExpr)

    static member lazy' =
      (function
      | SynExpr.Lazy (lazyExpr, _) -> Some lazyExpr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.Lazy (range=range) -> SynExpr.Lazy (expr, range)
        | synExpr -> synExpr)

  [<RequireQualifiedAccess>]
  module SynExpr =
    let (|ParenSynExpr|_|) = Optic.get SynExpr.paren

    let (|QuoteSynExpr|_|) = Optic.get SynExpr.quote

    let (|TypedSynExpr|_|) = Optic.get SynExpr.typed

    let (|TupleSynExpr|_|) = Optic.get SynExpr.tuple

    let (|StructTupleSynExpr|_|) = Optic.get SynExpr.structTuple

    let (|ArrayOrListSynExpr|_|) = Optic.get SynExpr.arrayOrList

    let (|RecordSynExpr|_|) = Optic.get SynExpr.record

    let (|NewSynExpr|_|) = Optic.get SynExpr.new'

    let (|ObjSynExpr|_|) = Optic.get SynExpr.objExpr

    let (|WhileSynExpr|_|) = Optic.get SynExpr.while'

    let (|ForSynExpr|_|) = Optic.get SynExpr.for'

    let (|ForEachSynExpr|_|) = Optic.get SynExpr.forEach

    let (|ArrayOrListOfSeqSynExpr|_|) =  Optic.get SynExpr.arrayOrListofSeq

    let (|CompSynExpr|_|) = Optic.get SynExpr.computation

    let (|LambdaSynExpr|_|) = Optic.get SynExpr.lambda

    let (|MatchLambdaSynExpr|_|) = Optic.get SynExpr.matchLambda

    let (|MatchSynExpr|_|) = Optic.get SynExpr.match'

    let (|DoSynExpr|_|) = Optic.get SynExpr.do'

    let (|AssertSynExpr|_|) = Optic.get SynExpr.assert'

    /// This must be applied after appNameof
    let (|AppSynExpr|_|) = Optic.get SynExpr.app

    let (|LetOrUseExpr|_|) = Optic.get SynExpr.letOrUse

    let (|TryWithSynExpr|_|) = Optic.get SynExpr.tryWith

    let (|TryFinallyExpr|_|) = Optic.get SynExpr.tryFinally

    let (|LazySynExpr|_|) = Optic.get SynExpr.lazy'

    type SynExpr with
      static member nameof' = ()

    let rec mapNameOf synExpr : Result<SynExpr, NameOfError> =
      Result.resultOf {
        match synExpr with
        | ParenSynExpr synExpr' -> return! applyNameOf synExpr SynExpr.paren synExpr'
        | QuoteSynExpr (operator, quoted) ->
          let! (operator, quoted) = zipWithNameOf (operator, quoted)
          return setQuoteExpr (operator, quoted) synExpr
        | TupleSynExpr synExprs -> return! traverseWithNameOf synExpr SynExpr.tuple synExprs
        | StructTupleSynExpr synExprs -> return! traverseWithNameOf synExpr SynExpr.structTuple synExprs
        | ArrayOrListSynExpr synExprs -> return! traverseWithNameOf synExpr SynExpr.arrayOrList synExprs
        | TypedSynExpr synExpr' -> return! applyNameOf synExpr SynExpr.typed synExpr'
        | RecordSynExpr recordFields -> return! traverseRecordWithNameOf synExpr recordFields
        | WhileSynExpr (whileExpr, doExpr) ->
          let! (whileExpr, doExpr) = zipWithNameOf (whileExpr, doExpr)
          return setWhileExpr (whileExpr, doExpr) synExpr
        | ForSynExpr (identBody, toBody, doBody) ->
          let! (identBody, toBody, doBody) = zip3WithNameOf (identBody, toBody, doBody)
          return setForExpr (identBody, toBody, doBody)  synExpr
        // FIXME: parse SeqExprOnly in SynExpr.ForEach
        | ForEachSynExpr (seqExprOnly, pat, enumExpr, bodyExpr) ->
          let! (enumExpr, bodyExpr) = zipWithNameOf (enumExpr, bodyExpr)
          return setForEachExpr (seqExprOnly, pat, enumExpr, bodyExpr)  synExpr
        // FIXME: add object expression
        | ObjSynExpr _ -> return! Result.fail NotImplemented
        | ArrayOrListOfSeqSynExpr synExpr' -> return! applyNameOf synExpr SynExpr.arrayOrListofSeq synExpr'
        // TODO: add other cases
        | CompSynExpr synExpr' -> return! applyNameOf synExpr SynExpr.computation synExpr'
        | LambdaSynExpr (args, body) ->
          let! body = mapNameOf body
          return setLambdaExpr (args, body) synExpr
        | MatchLambdaSynExpr _ -> return! Result.fail NotImplemented
        | MatchSynExpr (expr, clauses) ->
          let! expr = mapNameOf expr
          return setMatchExpr (expr, clauses) synExpr
        | DoSynExpr expr ->
          let! expr = mapNameOf expr
          return setDoExpr expr synExpr
        | AssertSynExpr expr ->
          let! expr = mapNameOf expr
          return setAssertExpr expr synExpr
        // This must be applied after nameof
        | AppSynExpr (funcExpr, argExpr) ->
          let! (funcExpr, argExpr) = zipWithNameOf (funcExpr, argExpr)
          return setAppExpr (funcExpr, argExpr) synExpr
        | LetOrUseExpr (let', body) ->
          // FIXME: check bindings
          let! body = mapNameOf body
          return setLetExpr (let', body) synExpr
        | TryWithSynExpr (tryExpr, withCases) ->
          let! tryExpr = mapNameOf tryExpr
          return setTryWithExpr (tryExpr, withCases) synExpr
        | TryFinallyExpr (tryExpr, finallyExpr) ->
          let! (tryExpr, finallyExpr) = zipWithNameOf (tryExpr, finallyExpr)
          return setTryFinallyExpr (tryExpr, finallyExpr) synExpr
        | LazySynExpr lazyExpr ->
          let! lazyExpr = mapNameOf lazyExpr
          return setLazyExpr lazyExpr synExpr
        // Function application is where nameof binding is implemented
        // | SynExpr.App (func )
          // Ingoring TypeApp
        | synExpr -> return synExpr
      }
    and applyNameOf synExpr (prism: Prism<_, _>) synExpr' =
      synExpr'
      |> mapNameOf
      |> Result.map (Optic.set prism synExpr)
    and setWhileExpr = Optic.set SynExpr.while'
    and setQuoteExpr = Optic.set SynExpr.quote
    and setLambdaExpr = Optic.set SynExpr.lambda
    and setMatchExpr = Optic.set SynExpr.match'
    and setForExpr = Optic.set SynExpr.for'
    and setForEachExpr = Optic.set SynExpr.forEach
    and setDoExpr = Optic.set SynExpr.do'
    and setAssertExpr = Optic.set SynExpr.assert'
    and setLetExpr = Optic.set SynExpr.letOrUse
    and setAppExpr = Optic.set SynExpr.app
    and setTryWithExpr = Optic.set SynExpr.tryWith
    and setLazyExpr = Optic.set SynExpr.lazy'
    and setTryFinallyExpr = Optic.set SynExpr.tryFinally
    and zipWithNameOf (expr', expr'') =
      Result.resultOf {
        let! expr' = mapNameOf expr'
        let! expr'' = mapNameOf expr''
        return (expr', expr'')
      }
    and zip3WithNameOf (expr', expr'', expr''') =
      Result.resultOf {
        let! expr' = mapNameOf expr'
        let! expr'' = mapNameOf expr''
        let! expr''' = mapNameOf expr'''
        return (expr', expr'', expr''')
      }
    and traverseWithNameOf synExpr (prism: Prism<SynExpr, list<SynExpr>>) exprs =
      Result.resultOf {
        let! exprs = Result.traverse mapNameOf exprs
        return Optic.set prism exprs synExpr
      }
    and traverseRecordWithNameOf synExpr recordFields =
      recordFields
      |> Result.traverse (fun (recordFieldName, maybeSynExpr, maybeBlockSeparator) -> Result.resultOf {
        let! synExpr = Result.ofOption NotAnIdentifier maybeSynExpr
        let! synExpr = mapNameOf synExpr
        return (recordFieldName, Some synExpr, maybeBlockSeparator)
      })
      |> Result.map (fun recordFields -> Optic.set SynExpr.record recordFields synExpr)

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

  open AsyncResult
  let getProjectOptionsResult =
    function
    | (options, []) -> Result.Ok options
    | (_, errors) -> Error errors

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
