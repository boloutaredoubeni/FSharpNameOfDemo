namespace FSharpNameOfDemo

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast

// TODO: apply to SynExpr.App (nameof, id)

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

  let isNameOfOperator (ident: Ident) = ident .IsNameOfOperator ()

  // FIXME: this should carry the range and possibly the expression
  type NameOfError =
    | FailedToParseLongId
    | NotAnIdentifier
    | NotImplemented
    | NameOfIsNotAnOperator

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
        | SynExpr.Quote (_, isRaw, _, isFromQueryExpression, range) ->
          SynExpr.Quote (operator, isRaw, quotedSynExpr, isFromQueryExpression, range)
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
      | SynExpr.Tuple (exprs=exprs) when (not << List.isEmpty) exprs -> Some exprs
      | _ -> None),
      (fun exprs ->
        function
        | SynExpr.Tuple (_, commaRanges, range) -> SynExpr.Tuple (exprs, commaRanges, range)
        | synExpr -> synExpr)

    // FIXME: for each collection, order and/or field names need to be maintained

    static member structTuple =
      (function
      | SynExpr.Tuple (exprs=exprs) when (not << List.isEmpty) exprs -> Some exprs
      | _ -> None),
      (fun exprs ->
        function
        | SynExpr.Tuple (_, commaRanges, range) -> SynExpr.Tuple (exprs, commaRanges, range)
        | synExpr -> synExpr)

    static member arrayOrList =
      (function
      | SynExpr.ArrayOrList (exprs=exprs) when (not << List.isEmpty) exprs -> Some exprs
      | _ -> None),
      (fun exprs ->
        function
        | SynExpr.ArrayOrList (isList, _, range) -> SynExpr.ArrayOrList (isList, exprs, range)
        | synExpr -> synExpr)

    static member record =
      (function
      | SynExpr.Record (recordFields=recordFields)
          // this would happen anyway
          when (not << List.isEmpty) recordFields ->
        Some recordFields
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
      | SynExpr.ObjExpr (argOptions=argOptions; bindings=bindings; extraImpls=extraImpls) ->
        Some (argOptions, bindings, extraImpls)
      | _ -> None),
      (fun (argOptions, bindings, extraImpls) ->
        function
        | SynExpr.ObjExpr (objType, _, _, _, newExprRange, range) ->
          SynExpr.ObjExpr (objType, argOptions, bindings, extraImpls, newExprRange, range)
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
        | SynExpr.For (forSeqPoint, ident, _, (* isDownTo: ? *) bool', _, _, range) ->
          SynExpr.For (forSeqPoint, ident, identBody, bool', toBody, doBody, range)
        | synExpr -> synExpr)

    static member forEach =
      (function
      | SynExpr.ForEach (seqExprOnly=seqExprOnly; pat=pat; enumExpr=enumExpr; bodyExpr=bodyExpr) ->
        Some (seqExprOnly, pat, enumExpr, bodyExpr)
      | _ -> None),
      (fun (seqExprOnly, pat, enumExpr, bodyExpr) ->
        function
        | SynExpr.ForEach (forSeqPoint, _, isFromSource, _, _, _, range) ->
          SynExpr.ForEach (forSeqPoint, seqExprOnly, isFromSource, pat, enumExpr, bodyExpr, range)
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
        | SynExpr.CompExpr (isArrayOrList, isNotNakedRefCell, _, range) ->
          SynExpr.CompExpr (isArrayOrList, isNotNakedRefCell, expr, range)
        | synExpr -> synExpr)

    static member lambda =
      (function
      | SynExpr.Lambda (args=args; body=body) -> Some (args, body)
      | _ -> None),
      (fun (args, body) ->
        function
        | SynExpr.Lambda (fromMethod, inLambdaSeq, _, _, range) ->
          SynExpr.Lambda (fromMethod, inLambdaSeq, args, body, range)
        | synExpr -> synExpr)

    static member matchLambda =
      (function
      | SynExpr.MatchLambda (_, _, synMatchClauses, _, _)
          when (not << List.isEmpty) synMatchClauses ->
        Some synMatchClauses
      | _ -> None),
      (fun synMatchClauses ->
        function
        | SynExpr.MatchLambda (isExnMatch, range0, _, matchSeqPoint, range) ->
          SynExpr.MatchLambda (isExnMatch, range0, synMatchClauses, matchSeqPoint, range)
        | synExpr -> synExpr)

    static member match' =
      (function
      | SynExpr.Match (expr=expr; clauses=clauses)
          when (not << List.isEmpty) clauses ->
        Some (expr, clauses)
      | _ -> None),
      (fun (expr, clauses) ->
        function
        | SynExpr.Match (matchSeqPoint, _, _, isExnMatch, range) ->
          SynExpr.Match (matchSeqPoint, expr, clauses, isExnMatch, range)
        | synExpr -> synExpr)

    static member do' =
      (function
      | SynExpr.Do (expr=expr) | SynExpr.DoBang (expr=expr) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.Do (range=range) -> SynExpr.Do (expr, range)
        | SynExpr.DoBang (range=range) -> SynExpr.DoBang (expr, range)
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
      | SynExpr.LetOrUse (bindings=bindings; body=body)
          when (not << List.isEmpty) bindings ->
        Some (bindings, body)
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
        | SynExpr.TryFinally (_, _, range, trySeqPoint, finallySeqPoint) ->
          SynExpr.TryFinally (tryExpr, finallyExpr, range, trySeqPoint, finallySeqPoint)
        | synExpr -> synExpr)

    static member lazy' =
      (function
      | SynExpr.Lazy (lazyExpr, _) -> Some lazyExpr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.Lazy (range=range) -> SynExpr.Lazy (expr, range)
        | synExpr -> synExpr)

    static member sequential =
      (function
      | SynExpr.Sequential (expr1=expr1; expr2=expr2) -> Some (expr1, expr2)
      | _ -> None),
      (fun (expr1, expr2) ->
        function
        | SynExpr.Sequential (seqPoint=seqPoint; isTrueSeq=isTrueSeq; range=range) ->
          SynExpr.Sequential (seqPoint, isTrueSeq, expr1, expr2, range)
        | synExpr -> synExpr)

    static member ifThenElse =
      (function
      | SynExpr.IfThenElse (ifExpr=ifExpr; thenExpr=thenExpr; elseExpr=elseExpr) -> Some (ifExpr, thenExpr, elseExpr)
      | _ -> None),
      (fun (ifExpr, thenExpr, elseExpr) ->
        function
        | SynExpr.IfThenElse (spIfToThen=spIfToThen; isFromErrorRecovery=isFromErrorRecovery; ifToThenRange=ifToThenRange; range=range) ->
          SynExpr.IfThenElse (ifExpr, thenExpr, elseExpr, spIfToThen, isFromErrorRecovery, ifToThenRange, range)
        | synExpr -> synExpr)

    static member ident =
      (function
      | SynExpr.Ident ident -> Some ident
      | _ -> None),
      (fun ident ->
        function
        | SynExpr.Ident _ -> SynExpr.Ident ident
        | synExpr -> synExpr)

    static member longIdent =
      (function
      | SynExpr.LongIdent (longDotId=(LongIdentWithDots (id=id; dotms=dotms)))
          when (not << List.isEmpty) id ->
        let (i::id) = id
        Some (i::id, dotms)
      | _ -> None),
      (fun (longIdent, dotms) ->
        function
        | SynExpr.LongIdent (isOptional=isOptional; altNameRefCell=altNameRefCell; range=range) ->
          let longIdentId = LongIdentWithDots (longIdent, dotms)
          SynExpr.LongIdent (isOptional, longIdentId, altNameRefCell, range)
        | synExpr -> synExpr)

    static member longIdentSet =
      (function
      | SynExpr.LongIdentSet (longDotId=(LongIdentWithDots (id=id; dotms=dotms)); expr=expr)
          when (not << List.isEmpty) id ->
        let (i::id) = id
        Some ((i::id, dotms), expr)
      | _ -> None),
      (fun ((id, dotms), expr) ->
        function
        | SynExpr.LongIdentSet (range=range) ->
          let longIdentId = LongIdentWithDots (id, dotms)
          SynExpr.LongIdentSet (longIdentId, expr, range)
        | synExpr -> synExpr)

    static member dotGet =
      (function
      | SynExpr.DotGet (expr=expr; longDotId=(LongIdentWithDots (id=id; dotms=dotms)))
          when (not << List.isEmpty) id ->
        let (i::id) = id
        Some (expr, (i::id, dotms))
      | _ -> None),
      (fun (expr, (id, dotms)) ->
        function
        | SynExpr.DotGet (rangeOfDot=rangeOfDot; range=range) ->
          let longIdentId = LongIdentWithDots (id, dotms)
          SynExpr.DotGet (expr, rangeOfDot, longIdentId, range)
        | synExpr -> synExpr)

    static member dotSet =
      (function
      | SynExpr.DotSet (expr1, LongIdentWithDots (id=id; dotms=dotms), expr2, _) when (not << List.isEmpty) id ->
        let (i::id) = id
        Some (expr1, (i::id, dotms), expr2)
      | _ -> None),
      (fun (expr1, (id, dotms), expr2) ->
        function
        | SynExpr.DotSet (range=range) ->
          let longIdentId = LongIdentWithDots (id, dotms)
          SynExpr.DotSet (expr1, longIdentId, expr2, range)
        | synExpr -> synExpr)

    static member dotIndexedGet =
      (function
      | SynExpr.DotIndexedGet (expr, setterArgs, _, _) when (not << List.isEmpty) setterArgs -> Some (expr, setterArgs)
      | _ -> None),
      (fun (expr, setterArgs) ->
        function
        | SynExpr.DotIndexedGet (_, _, range0, range) -> SynExpr.DotIndexedGet (expr, setterArgs, range0, range)
        | synExpr -> synExpr)

    static member dotIndexedSet =
      (function
      | SynExpr.DotIndexedSet (objectExpr=objectExpr; indexExprs=indexExprs; valueExpr=valueExpr)
          when (not << List.isEmpty) indexExprs ->
        Some (objectExpr, indexExprs, valueExpr)
      | _ -> None),
      (fun (objectExpr, indexExpr, valueExpr) ->
        function
        | SynExpr.DotIndexedSet (leftOfSetRange=leftOfSetRange; dotRange=dotRange; range=range) ->
          SynExpr.DotIndexedSet (objectExpr, indexExpr, valueExpr, leftOfSetRange, dotRange, range)
        | synExpr -> synExpr)

    static member namedIndexedPropertySet =
      (function
      | SynExpr.NamedIndexedPropertySet (LongIdentWithDots (id=id; dotms=dotms), expr1, expr2, _)
          when (not << List.isEmpty) id ->
        let (i::id) = id
        Some ((i::id, dotms), expr1, expr2)
      | _ -> None),
      (fun ((id, dotms), expr1, expr2) ->
        function
        | SynExpr.NamedIndexedPropertySet (range=range) ->
          let longIdentId = LongIdentWithDots (id, dotms)
          SynExpr.NamedIndexedPropertySet (longIdentId, expr1, expr2, range)
        | synExpr -> synExpr)

    static member dotNamedIndexedPropertySet =
      (function
      | SynExpr.DotNamedIndexedPropertySet (expr0, LongIdentWithDots (id=id; dotms=dotms), expr1, expr2, _)
          when (not << List.isEmpty) id ->
        let (i::id) = id
        Some (expr0, (i::id, dotms), expr1, expr2)
      | _ -> None),
      (fun (expr0, (id, dotms), expr1, expr2) ->
        function
        | SynExpr.DotNamedIndexedPropertySet (range=range) ->
          let longIdentId = LongIdentWithDots (id, dotms)
          SynExpr.DotNamedIndexedPropertySet (expr0, longIdentId, expr1, expr2, range)
        | synExpr -> synExpr)

    static member typeTest =
      (function
      | SynExpr.TypeTest (expr=expr; typeName=typeName) -> Some (expr, typeName)
      | _ -> None),
      (fun (expr, typeName) ->
        function
        | SynExpr.TypeTest (range=range) -> SynExpr.TypeTest (expr, typeName, range)
        | synExpr -> synExpr)

    static member upcast' =
      (function
      | SynExpr.Upcast (expr=expr; typeName=typeName) -> Some (expr, typeName)
      | _ -> None),
      (fun (expr, typeName) ->
        function
        | SynExpr.Upcast (range=range) -> SynExpr.Upcast (expr, typeName, range)
        | synExpr -> synExpr)

    static member downcast' =
      (function
      | SynExpr.Downcast (expr=expr; typeName=typeName) -> Some (expr, typeName)
      | _ -> None),
      (fun (expr, typeName) ->
        function
        | SynExpr.Downcast (range=range) -> SynExpr.Downcast (expr, typeName, range)
        | synExpr -> synExpr)

    static member inferredUpcast =
      (function
      | SynExpr.InferredUpcast (expr=expr) -> Some (expr)
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.InferredUpcast (range=range) -> SynExpr.InferredUpcast (expr, range)
        | synExpr -> synExpr)

    static member inferredDowncast =
      (function
      | SynExpr.InferredDowncast (expr=expr) -> Some (expr)
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.InferredDowncast (range=range) -> SynExpr.InferredDowncast (expr, range)
        | synExpr -> synExpr)

    static member addressOf =
      (function
      | SynExpr.AddressOf (_, expr, _, _) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.AddressOf (isByref, _, range0, range) -> SynExpr.AddressOf (isByref, expr, range0, range)
        | synExpr -> synExpr)

    static member joinIn =
      (function
      | SynExpr.JoinIn (expr1, _, expr2, _) -> Some (expr1, expr2)
      | _ -> None),
      (fun (expr1, expr2) ->
        function
        | SynExpr.JoinIn (_, range0, _, range) -> SynExpr.JoinIn (expr1, range0, expr2, range)
        | synExpr -> synExpr)

    static member yieldOrReturn =
      (function
      | SynExpr.YieldOrReturnFrom (expr=expr) | SynExpr.YieldOrReturn (expr=expr) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.YieldOrReturn ((b1, b2), _, range) -> SynExpr.YieldOrReturn ((b1, b2), expr, range)
        | SynExpr.YieldOrReturnFrom ((b1, b2), _, range) -> SynExpr.YieldOrReturnFrom ((b1, b2), expr, range)
        | synExpr -> synExpr)

    static member letOrUseBang =
      (function
      | SynExpr.LetOrUseBang (_, _, _, pat, expr1, expr2, _) -> Some (pat, expr1,  expr2)
      | _ -> None),
      (fun (pat, expr1, expr2) ->
        function
        | SynExpr.LetOrUseBang (bindSeqPoint=bindSeqPoint; isUse=isUse; isFromSource=isFromSource; range=range) ->
          SynExpr.LetOrUseBang (bindSeqPoint, isUse, isFromSource, pat, expr1, expr2, range)
        | synExpr -> synExpr)

    static member fixed' =
      (function
      | SynExpr.Fixed (expr=expr) -> Some expr
      | _ -> None),
      (fun expr ->
        function
        | SynExpr.Fixed (range=range) -> SynExpr.Fixed (expr, range)
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

    let (|SequentialSynExpr|_|) = Optic.get SynExpr.sequential

    let (|IfThenElseSynExpr|_|) = Optic.get SynExpr.ifThenElse

    let (|IdentSynExpr|_|) = Optic.get SynExpr.ident

    let (|LongIdentSynExpr|_|) = Optic.get SynExpr.longIdent

    let (|LongIdentSetSynExpr|_|) = Optic.get SynExpr.longIdentSet

    let (|DotGetSynExpr|_|) = Optic.get SynExpr.dotGet

    let (|DotSetSynExpr|_|) = Optic.get SynExpr.dotSet

    let (|DotIndexedGetSynExpr|_|) = Optic.get SynExpr.dotIndexedGet

    let (|DotIndexedSetSynExpr|_|) = Optic.get SynExpr.dotIndexedSet

    let (|NamedIndexedPropSynExpr|_|) = Optic.get SynExpr.namedIndexedPropertySet

    let (|DotNamedIndexedPropSynExpr|_|) = Optic.get SynExpr.dotNamedIndexedPropertySet

    let (|TypeTestSynExpr|_|) = Optic.get SynExpr.typeTest

    let (|UpcastSynExpr|_|) = Optic.get SynExpr.upcast'

    let (|DowncastSynExpr|_|) = Optic.get SynExpr.downcast'

    let (|InferredUpcastSynExpr|_|) = Optic.get SynExpr.inferredUpcast

    let (|InferredDowncastSynExpr|_|) = Optic.get SynExpr.inferredDowncast

    let (|AddressOfSynExpr|_|) = Optic.get SynExpr.addressOf

    let (|JoinInSynExpr|_|) = Optic.get SynExpr.joinIn

    let (|YieldOrReturnSynExpr|_|) = Optic.get SynExpr.yieldOrReturn

    let (|LetOrUseBangSynExpr|_|) = Optic.get SynExpr.letOrUseBang

    let (|FixedSynExpr|_|) = Optic.get SynExpr.fixed'

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
        // FIXME: This must be applied after nameof
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
        | SequentialSynExpr (expr1, expr2) ->
          let! (expr1, expr2) = zipWithNameOf (expr1, expr2)
          return setSequentialExpr (expr1, expr2) synExpr
        | IfThenElseSynExpr (ifExpr, thenExpr, elseExpr) ->
          let! (ifExpr, thenExpr) = zipWithNameOf (ifExpr, thenExpr)
          let! elseExpr = applyOption elseExpr
          return Optic.set SynExpr.ifThenElse (ifExpr, thenExpr, elseExpr) synExpr
        // Function application is where nameof binding is implemented
        // | SynExpr.App (func )
          // Ingoring TypeApp
          //
        | IdentSynExpr ident when ident.IsNameOfOperator () -> return! Result.fail NameOfIsNotAnOperator
        | IdentSynExpr ident -> return Optic.set SynExpr.ident ident synExpr
        | LongIdentSynExpr (longIdents, ranges) ->
          if List.forall (not << isNameOfOperator) longIdents
            then return Optic.set SynExpr.longIdent (longIdents, ranges) synExpr
            else return! Result.fail NameOfIsNotAnOperator
        | LongIdentSetSynExpr ((longIdents, dotms), expr) ->
          if List.forall (not << isNameOfOperator) longIdents
            then
              let! expr = mapNameOf expr
              return Optic.set SynExpr.longIdentSet ((longIdents, dotms), expr) synExpr
            else
              return! Result.fail NameOfIsNotAnOperator
        | DotGetSynExpr (expr, (id, dotms)) ->
          if List.forall (not << isNameOfOperator) id
            then
              let! expr = mapNameOf expr
              return Optic.set SynExpr.dotGet (expr, (id, dotms)) synExpr
            else
              return! Result.fail NameOfIsNotAnOperator
        | DotSetSynExpr (expr1, (id, dotms), expr2) ->
          if List.forall (not << isNameOfOperator) id
            then
              let! (expr1, expr2) = zipWithNameOf (expr1, expr2)
              return Optic.set SynExpr.dotSet (expr1, (id, dotms), expr2) synExpr
            else return! Result.fail NameOfIsNotAnOperator
        | DotIndexedGetSynExpr (expr, args) ->
          let! expr = mapNameOf expr
          return Optic.set SynExpr.dotIndexedGet (expr, args) synExpr
        | DotIndexedSetSynExpr (objectExpr, indexExprs, valueExpr) ->
          let! (objectExpr, valueExpr) = zipWithNameOf (objectExpr, valueExpr)
          return Optic.set SynExpr.dotIndexedSet (objectExpr, indexExprs, valueExpr) synExpr
        | NamedIndexedPropSynExpr ((id, dotms), expr1, expr2) ->
          if List.forall (not << isNameOfOperator) id
            then
              let! (expr1, expr2) = zipWithNameOf (expr1, expr2)
              return Optic.set SynExpr.namedIndexedPropertySet ((id, dotms), expr1, expr2) synExpr
            else
              return! Result.fail NameOfIsNotAnOperator
        | DotNamedIndexedPropSynExpr (expr0, (id, dotms), expr1, expr2) ->
          if List.forall (not << isNameOfOperator) id
            then
              let! (expr0, expr1, expr2) = zip3WithNameOf (expr0, expr1, expr2)
              return Optic.set SynExpr.dotNamedIndexedPropertySet (expr0, (id, dotms), expr1, expr2) synExpr
            else
              return! Result.fail NameOfIsNotAnOperator
        | TypeTestSynExpr (expr, typeName) ->
          let! expr = mapNameOf expr
          return Optic.set SynExpr.typeTest (expr, typeName) synExpr
        | UpcastSynExpr (expr, typeName) ->
          let! expr = mapNameOf expr
          return Optic.set SynExpr.upcast' (expr, typeName) synExpr
        | DowncastSynExpr (expr, typeName) ->
          let! expr = mapNameOf expr
          return Optic.set SynExpr.downcast' (expr, typeName) synExpr
        | InferredDowncastSynExpr expr ->
          let! expr = mapNameOf expr
          return Optic.set SynExpr.inferredDowncast expr synExpr
        | InferredUpcastSynExpr expr ->
          let! expr = mapNameOf expr
          return Optic.set SynExpr.inferredUpcast expr synExpr
        | AddressOfSynExpr expr ->
          let! expr = mapNameOf expr
          return Optic.set SynExpr.addressOf expr synExpr
        | JoinInSynExpr (expr1, expr2) ->
          let! (expr1, expr2) = zipWithNameOf (expr1, expr2)
          return Optic.set SynExpr.joinIn (expr1, expr2) synExpr
        | LetOrUseBangSynExpr (pat, expr1, expr2) ->
          let! (expr1, expr2) = zipWithNameOf (expr1, expr2)
          return Optic.set SynExpr.letOrUseBang (pat, expr1, expr2) synExpr
        | FixedSynExpr expr ->
          let! expr = mapNameOf expr
          return Optic.set SynExpr.fixed' expr synExpr
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
    and setSequentialExpr = Optic.set SynExpr.sequential
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
    and applyOption (maybeExpr : SynExpr option) : Result<SynExpr option, NameOfError>  =
      Result.resultOf {
        if Option.isNone maybeExpr
          then return None
          else
            let (Some expr) = maybeExpr
            let! expr = mapNameOf expr
            return Some expr
      }
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

  open AsyncResult
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

  let private getTree (file, input) =
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
