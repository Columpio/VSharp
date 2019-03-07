namespace VSharp

open FSharpx.Collections
open VSharp.Core
open System.Collections.Generic


module internal Encode =
    type private schemaId = obj
    type private ISecondOrderArgId =
        abstract GeneratePartialApplication : state -> schemaId * (unit -> term list * term list)

    type private schemaApp =
        {
            schema : schemaId; constant : term; resultNum : int;
            state : state option; parameterValues : term list;
            soArgs : IDictionary<ISecondOrderArgId, schemaApp>; foArgs : IDictionary<term, term>
        }

    type private schema =
        {
            id : schemaId; results : term list; apps : ISet<schemaApp>; apptmp : ISet<schemaApp>; parameterInputs : term list;
            soinputs : ISet<ISecondOrderArgId * termType list * termType>; mutable sotmp1 : ISet<ISecondOrderArgId * termType list * termType>;
            mutable sotmp2 : ISet<ISecondOrderArgId * termType list * termType>; sosubsts : ISet<ISecondOrderArgId * term list * term>;
            foinputs : ISet<term>; mutable fotmp1 : ISet<term>; mutable fotmp2 : ISet<term>; // TODO: do we need fotmp2?
        }

    let private schemas = persistent((fun () -> new Dictionary<schemaId, schema>()), id)

    module private RecursionOutcomes =
        type recursionOutcomeKey =
            { funcId : IFunctionIdentifier; location : term option }
            override x.ToString() =
                match x.location with
                | None -> sprintf "%O#res" x.funcId
                | Some loc -> sprintf "%O#%O" x.funcId loc

        let private generateKey funcId loc : recursionOutcomeKey = { funcId = funcId; location = loc }

        let private generateBody funcId loc =
            let summary = Database.QuerySummary funcId
            match loc with
            | None -> summary.result
            | Some loc -> Memory.DereferenceWithoutValidation summary.state loc

        let generate funcId loc =
            generateKey funcId loc :> schemaId, fun () -> ([generateBody funcId loc], [])


    module private SymbolicSubtyping =
        type symbolicSubtypingKey =
            { t : termType; u : termType }
            override x.ToString() = sprintf "[%O <: %O # rel]" x.t x.u

        let private generateKey t u : symbolicSubtypingKey = { t = t; u = u }

        let private generateBody t u =
            // TODO: implement it! [misonijnik]
            let name = sprintf "[%O <: %O]" t u
            Constant name ({ id = name } : emptySource) Bool

        let generate t u =
            generateKey t u, fun () -> ([generateBody t u], [])


    module private LazyInstantiations =

        let private idOfDefinedHeap<'a when 'a : equality> : heap<'a, term, fql> -> string = hash >> toString
        let private idOfStack = hash >> toString
        let rec private idOfHeap = function
            | Defined(_, h) -> idOfDefinedHeap h
            | HigherOrderApplication(f, _, _) -> toString f
            | RecursiveApplication(f, _, _) -> toString f
            | Composition(s, _, h) -> sprintf "[%s ⚪ %s]" (idOfState s) (idOfHeap h)
            | Mutation(h1, h2) -> sprintf "write(%s, %s)" (idOfHeap h1) (idOfDefinedHeap h2)
            | Merged _ -> __unreachable__()

        and private idOfState s =
            sprintf "state(%s, %s, %s)" (idOfStack s.stack) (idOfHeap s.heap) (idOfHeap s.statics)

        type private refTarget =
            | TopLevelHeapTarget
            | TopLevelStaticsTarget of termType
            | StructFieldTarget of string
            | ArrayIndexTarget
            override x.ToString() =
                match x with
                | TopLevelHeapTarget -> "Heap"
                | TopLevelStaticsTarget t -> sprintf "Statics of %O" t
                | StructFieldTarget f -> f
                | ArrayIndexTarget -> "ArrayIdx"
        type private instantiationSchemaSource =
            { heapId : string; refTarget : refTarget list; typ : termType } with
            override x.ToString() =
                sprintf "read#%s#%s#%O" (x.refTarget |> List.map toString |> join "+") x.heapId x.typ

        type private soArgId =
            { refTarget : refTarget list; gen : (state -> schemaId * (unit -> term list * term list)) transparent } with
            interface ISecondOrderArgId with
                override x.GeneratePartialApplication state = x.gen.v state
            override x.ToString() = x.refTarget.ToString()

        let private decomposeArrayIndex ctor acc x =
            let name = IdGenerator.startingWith "arr-idx"
            let constant = Constant name ({ id = name }: emptySource) (Numeric typeof<int>)
            (ArrayIndexTarget, ctor constant), (constant, x)::acc

        let private pathToRefTarget =
            List.mapFold (fun acc -> function
                | StructField(s, _) as seg -> (StructFieldTarget s, seg), acc
                | ArrayLowerBound x -> decomposeArrayIndex ArrayLowerBound acc x
                | ArrayLength x -> decomposeArrayIndex ArrayLength acc x
                | ArrayIndex(x, t) -> decomposeArrayIndex (ArrayIndex << withSnd t) acc x)

        let private generateKey location =
            let refTarget, reference, subst =
                match location.term with
                | Ref(TopLevelHeap(addr, bt, st), path) ->
                    let symbolicAddr, subst =
                        let name = IdGenerator.startingWith "heap-addr"
                        let symbolicAddr = Constant name ({ id = name }: emptySource) (Numeric typeof<int>)
                        let subst = [(symbolicAddr, addr)]
                        symbolicAddr, subst
                    let pair, subst = pathToRefTarget subst path
                    let target, path' = List.unzip pair
                    TopLevelHeapTarget::target, HeapRef symbolicAddr bt st path', subst
                | Ref(TopLevelStatics key, path) ->
                    let pair, subst = pathToRefTarget [] path
                    let target, path' = List.unzip pair
                    (TopLevelStaticsTarget key)::target, StaticRef key path', subst
                | _ -> __notImplemented__()  // TODO: internpool
            let typ =
                match TypeOf location with
                | Reference t
                | Pointer t -> t
                | _ -> __notImplemented__()
            refTarget, reference, typ, List.rev subst

        let private generateHeapAccess location selectHeap mkState = function
            | Defined _
            | Mutation _ as h
            | Composition(_, _, h) ->
                Memory.DereferenceWithoutValidation (mkState h) location
            | RecursiveApplication(f, _, _) ->
                let summary = Database.QuerySummary f
                Memory.DereferenceWithoutValidation (selectHeap summary.state |> mkState) location
            | HigherOrderApplication _ -> __notImplemented__()
            | Merged _ -> __unreachable__()

        let private pickAccessor ref state = function
            | TopLevelHeapTarget ->
                let getter state = state.heap
                let setter h = { Memory.EmptyState with heap = h }
                idOfHeap state.heap, fun () -> generateHeapAccess ref getter setter state.heap
            | TopLevelStaticsTarget _ ->
                let getter state = state.statics
                let setter h = { Memory.EmptyState with statics = h }
                idOfHeap state.statics, fun () -> generateHeapAccess ref getter setter state.statics
            | _ -> __unreachable__() // TODO [miha2733]: intern pool

        let generateSoArg location =
            let refTarget, symbolicRef, typ, subst = generateKey location
            assert(not refTarget.IsEmpty)
            let generateSoSchema state =
                let idOfHeap, generateAccess = pickAccessor symbolicRef state refTarget.Head
                let key = { heapId = idOfHeap; refTarget = refTarget; typ = typ } :> schemaId
                key, fun () -> ([generateAccess ()], List.map fst subst)
            let argId : soArgId = { refTarget = refTarget; gen = { v = generateSoSchema } }
            argId :> ISecondOrderArgId, subst

        let generateApp constant selectHeap mkState heap location =
            let refTarget, symbolicRef, typ, subst = generateKey location
            let substFrom, substTo = List.unzip subst
            let heap, state =
                match heap with
                | Composition(s, _, h) -> h, Some s
                | _ -> heap, None
            let key = { heapId = idOfHeap heap; refTarget = refTarget; typ = typ } :> schemaId
            let app = {
                schema = key; constant = constant; resultNum = 0; state = state; parameterValues = substTo;
                soArgs = new Dictionary<_, _>(); foArgs = new Dictionary<_, _>()
            }
            app, fun () -> ([generateHeapAccess symbolicRef selectHeap mkState heap], substFrom)


    let private compose state constant =
        match constant.term with
        | Constant(_, source, _) ->
            match source with
            | :? INonComposableSymbolicConstantSource -> constant
            | :? IStatedSymbolicConstantSource as source -> source.Compose compositionContext.Empty state
            | _ -> __notImplemented__()
        | _ -> __unreachable__()

    let rec private generateSchema key generate =
        if schemas.Value.ContainsKey key then ()
        else
            let terms, parameters = generate ()
            let schema = {
                id = key; results = terms; apps = HashSet<_>(); apptmp = new HashSet<_>(); parameterInputs = parameters
                soinputs = new HashSet<_>(); sotmp1 = new HashSet<_>(); sotmp2 = new HashSet<_>(); sosubsts = new HashSet<_>();
                foinputs = new HashSet<_>(); fotmp1 = new HashSet<_>(); fotmp2 = new HashSet<_>()
            }
            schemas.Value.[key] <- schema
            terms |> ConstantsOf |> Seq.iter (processConstant schema.apps schema.sotmp1 schema.sosubsts schema.fotmp1)

    and private processConstant apps soargs sosubsts foargs constant =
        match constant.term with
        | Constant(_, source, typ) ->
            match source with
            | LazyInstantiationEpsilon { term = Ref(TopLevelStack _, _) }
            | KeyInitializedSource _ ->
                foargs.Add constant |> ignore
            | RecursionOutcome(funcId, state, location, _) ->
                let key, generate = RecursionOutcomes.generate funcId location
                let app = {
                    schema = key; constant = constant; resultNum = 0; state = Some state; parameterValues = []
                    soArgs = new Dictionary<_, _>(); foArgs = new Dictionary<_, _>()
                }
                apps.Add app |> ignore
                generateSchema key generate
            | LazyInstantiationEpsilon location ->
                let argId, subst = LazyInstantiations.generateSoArg location
                soargs.Add(argId, List.map (fst >> TypeOf) subst, typ) |> ignore
                sosubsts.Add(argId, List.map snd subst, constant) |> ignore
            | LazyInstantiation({ term = Ref(TopLevelHeap _, _) } as location, Some heap, _)
            | LazyInstantiation({ term = Ptr(TopLevelHeap _, _, _, _) } as location, Some heap, _) ->
                let getter (state : state) = state.heap
                let setter heap = { Memory.EmptyState with heap = heap }
                let app, generate = LazyInstantiations.generateApp constant getter setter heap location
                apps.Add app |> ignore
                generateSchema app.schema generate
            | LazyInstantiation({ term = Ref(TopLevelStatics _, _) } as location, Some heap, _)
            | LazyInstantiation({ term = Ptr(TopLevelStatics _, _, _, _) } as location, Some heap, _) ->
                let getter (state : state) = state.statics
                let setter heap = { Memory.EmptyState with statics = heap }
                let app, generate = LazyInstantiations.generateApp constant getter setter heap location
                apps.Add app |> ignore
                generateSchema app.schema generate
            // TODO [miha2733]: intern pool
            | SymbolicSubtypeSource(t, u) ->
                let key, generate = SymbolicSubtyping.generate t u
                let app = {
                    schema = key; constant = constant; resultNum = 0; state = None; parameterValues = [];
                    soArgs = new Dictionary<_, _>(); foArgs = new Dictionary<_, _>()
                }
                apps.Add app |> ignore
                generateSchema key generate
            | :? emptySource -> ()
            | _ -> __notImplemented__()
        | _ -> __unreachable__()

    let rec private traverseAppDependencies (visited : ISet<schemaId>) foldSchema foldApp schema acc (app : schemaApp) =
        let acc = foldApp acc schema app
        let acc = Seq.fold (traverseAppDependencies visited foldSchema foldApp schema) acc app.soArgs.Values
        traverseSchemaDependencies visited foldSchema foldApp acc app.schema

    and private traverseSchemaDependencies visited foldSchema foldApp acc key =
        if visited.Add key then
            let schema = schemas.Value.[key]
            let acc = foldSchema acc schema
            Seq.fold (traverseAppDependencies visited foldSchema foldApp schema) acc schema.apps
        else acc

    let private traverseApps folder acc rootKey =
        traverseSchemaDependencies (new HashSet<_>()) always folder acc rootKey

    let private traverseSchemas folder acc rootKey =
        traverseSchemaDependencies (new HashSet<_>()) folder (fun acc _ _ -> acc) acc rootKey

    let private pushFreshDependencies rootKey =
        let foldSchemaApp smthChanged (schema : schema) (app : schemaApp) =
            smthChanged ||
            app.schema <> schema.id &&
                let other = schemas.Value.[app.schema]
                let pushSO = Seq.fold (fun acc i -> schema.soinputs.Contains i || schema.sotmp1.Add i || acc) false other.sotmp1
                let pushFO = Seq.fold (fun acc i -> schema.foinputs.Contains i || schema.fotmp1.Add i || acc) false other.fotmp1
                pushSO || pushFO  // Do not change to Seq.fold ... || Seq.fold ...! Both must be evaluated.
        while traverseApps foldSchemaApp false rootKey do ()

    let private generateArgs (schema : schema) (app : schemaApp) =
        let tmp2 =
            match app.state with
            | None -> schema.fotmp1 :> seq<_>
            | Some state ->
                seq {
                    for i in schema.fotmp1 do
                        let value = compose state i
                        app.foArgs.[i] <- value
                        yield value
                }
            |> ConstantsOf
        tmp2.ExceptWith schema.fotmp1
        tmp2.ExceptWith schema.foinputs
        Seq.iter (processConstant schema.apptmp schema.sotmp2 schema.sosubsts schema.fotmp2) tmp2

    let private flushFreshDependencies rootKey =
        let foldSchemaApp () (schema : schema) (app : schemaApp) =
            generateArgs schema app
            match app.state with
            | Some state ->
                let other = schemas.Value.[app.schema]
                for i, _, _ in other.sotmp1 do
                    let key, genSchema = i.GeneratePartialApplication state
                    generateSchema key genSchema
                    let hoapp = { schema = key; constant = Nop; resultNum = 0; state = None; parameterValues = []; soArgs = new Dictionary<_, _>(); foArgs = new Dictionary<_,_>() }
                    app.soArgs.Add(i, hoapp)
                    generateArgs schema hoapp
            | None -> ()

        traverseApps foldSchemaApp () rootKey
        let flushTmp acc schema =
            let result = acc || schema.apptmp.Count > 0 || schema.fotmp2.Count > 0 || schema.sotmp2.Count > 0
            schema.apps.UnionWith schema.apptmp
            schema.apptmp.Clear()
            schema.foinputs.UnionWith schema.fotmp1
            schema.fotmp1 <- schema.fotmp2
            schema.fotmp2 <- new HashSet<_>()
            schema.soinputs.UnionWith schema.sotmp1
            schema.sotmp1 <- schema.sotmp2
            schema.sotmp2 <- new HashSet<_>()
            result
        traverseSchemas flushTmp false rootKey

    let private generateSchemas terms =
        let rootKey = "query"
        // TODO: iterate while not saturated
        generateSchema rootKey (fun () -> terms, [])
        while (pushFreshDependencies rootKey; flushFreshDependencies rootKey) do ()
        let result = schemas.Value.[rootKey]
        schemas.Value.Remove(rootKey) |> ignore
        result

    let rec private toDnf = function
        | Disjunction ts -> List.collect toDnf ts
        | Conjunction ts ->
            let dnfs = List.map toDnf ts
            let shuffle xss yss =
                List.collect (fun xs -> List.map (List.append xs) yss) xss
            List.reduce shuffle dnfs
        | t -> [[t]]

    let private unguardTerm = function
        | { term = Union gvs } -> List.collect (fun (g, v) -> List.map (withSnd v) (toDnf g)) gvs
        | t -> [[], t]

    let private unguardTerms mapper terms =
        let unguarded = List.map unguardTerm terms
        unguarded |> List.cartesianMap (fun gvs ->
            let gss, vs = List.unzip gvs
            List.concat gss, mapper vs) |> List.ofSeq

    let private encodeQueryToSchema terms k =
        schemas.Reset()
        let querySchema = generateSchemas terms
        let result = k querySchema
        schemas.Restore()
        result


    module internal Relations =

        let private unguardRelApp (app : soRelationalApplication) =
            unguardTerms (fun args -> { app with args = args }) app.args

        let private unguardRelApps mapper = function
            | [] -> [mapper ([], [])]
            | apps ->
                let unguardedApps = List.map unguardRelApp apps
                unguardedApps |> List.cartesianMap (List.unzip >> mapper) |> List.ofSeq

        let private schema2FoRel (schema : schema) : foRelation =
            let args = List.append3 (List.ofSeq schema.foinputs) schema.parameterInputs schema.results
            { id = schema.id.ToString(); signature = List.map TypeOf args }

        let private soRels = persistent((fun () -> new Dictionary<schemaId, soRelation>()), id)
        let private foRels = persistent((fun () -> new Dictionary<schemaId * ISecondOrderArgId, foRelation>()), id)

        let private schema2Rel (schema : schema) =
            let mapArg (argId, domain, typ) =
                let signature = List.append domain [typ]
                let argSchema = { id = "tau_" + toString argId + (hash schema.id |> toString); signature = signature }
                foRels.Value.Add((schema.id, argId), argSchema)
                signature
            let relArgs = schema.soinputs |> Seq.map mapArg |> List.ofSeq
            soRels.Value.Add(schema.id, { foRel = schema2FoRel schema; relArgs = relArgs })

        let private schema2HeadApp (schema : schema) : soRelationalApplication =
            let rel = soRels.Value.[schema.id]
            let soArgs = schema.soinputs |> Seq.map (fun (argId, _, _) -> foRels.Value.[schema.id, argId] |> FoArg) |> List.ofSeq
            { symbol = rel; relArgs = soArgs; args = List.append3 (List.ofSeq schema.foinputs) schema.parameterInputs schema.results }

        let private schemaArg2RelArg sourceSchemaId (soArgs : IDictionary<_, _>) (argId, _, _) =
            if soArgs.ContainsKey argId then
                let schemaApp = soArgs.[argId]
                let schema = schemas.Value.[schemaApp.schema]
                let rel = soRels.Value.[schemaApp.schema]
                let soArgs = schema.soinputs |> Seq.map (fun (argId, _, _) -> foRels.Value.[sourceSchemaId, argId]) |> List.ofSeq
                let foArgs = schema.foinputs |> Seq.map (fun i -> if schemaApp.foArgs.ContainsKey i then schemaApp.foArgs.[i] else i) |> List.ofSeq
                SoArg(rel, soArgs, foArgs)
            else FoArg foRels.Value.[sourceSchemaId, argId]

        let private soSubst2RelApp schemaId (argId, args, constant) : soRelationalApplication =
            let soRel = { foRel = foRels.Value.[schemaId, argId]; relArgs = [] }
            { symbol = soRel; relArgs = []; args = List.append args [constant] }

        let private schemaApps2RelApps sourceSchemaId (apps : schemaApp seq) : soRelationalApplication list =
            let groups = apps |> Seq.groupBy (fun app -> app.schema, app.parameterValues, app.soArgs, app.foArgs)
            let groupToRelApp idx ((schemaId, parameterValues, soArgs : IDictionary<_, _>, foArgs : IDictionary<_, _>), apps) =
                let schema = schemas.Value.[schemaId]
                let rel = soRels.Value.[schemaId]
                let args = Seq.map (fun input -> if foArgs.ContainsKey input then foArgs.[input] else input) schema.foinputs |> List.ofSeq
                let addResutingConst map (app : schemaApp) =
                    let i = app.resultNum
                    assert(not <| Map.containsKey i map)
                    Map.add i app.constant map
                let resultingConstMap = Seq.fold addResutingConst Map.empty apps
                let resultingConsts = schema.results |> List.mapi (fun i t ->
                    if Map.containsKey i resultingConstMap then resultingConstMap.[i]
                    else
                        let name = sprintf "res#%i" (i + idx)
                        Constant name ({ id = name }: emptySource) (TypeOf t))
                let relArgs = Seq.map (schemaArg2RelArg sourceSchemaId soArgs) schema.soinputs |> List.ofSeq
                { symbol = rel; relArgs = relArgs; args = List.append3 args parameterValues resultingConsts }, idx + schema.results.Length
            Seq.mapFold groupToRelApp 0 groups |> fst |> List.ofSeq

        let private isApplicationFor (constants : ISet<term>) (app : soRelationalApplication) =
            match app.args with
            | [] -> false
            | xs -> constants.Contains(List.last xs)

        let private filterBody constraints body =
            let rec loop acc apps terms =
                let constants = ConstantsOf terms
                let passed, failed = apps |> List.partition (isApplicationFor constants)
                match passed with
                | [] -> acc
                | _ -> loop (List.append acc passed) failed (passed |> List.collect (fun app -> List.append app.args (app.relArgs |> List.collect (fun relArg -> relArg.Args))))
            loop [] body constraints

        let private encodeRuleSchema key =
            let schema = schemas.Value.[key]
            let head = schema2HeadApp schema
            let unguardedHeads = unguardRelApp head
            unguardedHeads |> List.collect (fun (gs, head) ->
                schema.apps
                |> schemaApps2RelApps key
                |> List.append (schema.sosubsts |> Seq.map (soSubst2RelApp schema.id) |> List.ofSeq)
                |> unguardRelApps (fun (gss, body) ->
                    let constraints = List.concat (gs::gss)
                    { constraints = constraints; body = filterBody (List.append head.args constraints) body; head = Some head }))

        let private rootContextClause id argId domain typ =
            let name = "root-ctx-" + toString argId
            let rel = { id = name; signature = List.append domain [typ] }
            let domainArgs = List.map (fun t -> let n = IdGenerator.startingWith "root-ctx#arg" in Constant n ({ id = n } : emptySource) t) domain
            // TODO (here and above): Constant ... is not always correct. Sometimes we may return pointers.
            let head = { symbol = { foRel = rel; relArgs = [] }; relArgs = []; args = List.append domainArgs [Constant "root-ctx-result" ({ id = "root-ctx-result" } : emptySource) typ] }
            foRels.Value.Add((id, argId), rel)
            { constraints = []; body = []; head = Some head }

        let encodeQuery terms : SOCHCSystem =
            let k querySchema =
                soRels.Reset()
                foRels.Reset()
                Seq.iter schema2Rel schemas.Value.Values
                let rootContextRules = querySchema.soinputs |> Seq.map (fun (argId, domain, typ) -> rootContextClause querySchema.id argId domain typ) |> List.ofSeq
                let queries =
                    querySchema.apps
                    |> schemaApps2RelApps querySchema.id
                    |> unguardRelApps (fun (gss, body) ->
                        let constraints = List.concat (terms::gss)
                        { constraints = constraints; body = filterBody constraints body; head = None })
                let rules = Seq.map encodeRuleSchema schemas.Value.Keys
                let result = List.concat (queries::rootContextRules::(List.ofSeq rules))
                soRels.Restore()
                foRels.Restore()
                result
            encodeQueryToSchema terms k
