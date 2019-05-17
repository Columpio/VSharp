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
            let summary = Explore funcId id
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
            | Merged ghs ->
                ghs
                |> List.map (fun (g, h) -> sprintf "\t%O -> %s" g (idOfHeap h))
                |> join ",,"
                |> sprintf "merge(\n%s\n)"

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
            | Merged _
            | Defined _
            | Mutation _ as h
            | Composition(_, _, h) ->
                Memory.DereferenceWithoutValidation (mkState h) location
            | RecursiveApplication(f, _, _) ->
                let summary = Explore f id
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
            let results, parameters = generate ()
            let schema = {
                id = key; results = results; apps = new HashSet<_>(); apptmp = new HashSet<_>(); parameterInputs = parameters
                soinputs = new HashSet<_>(); sotmp1 = new HashSet<_>(); sotmp2 = new HashSet<_>(); sosubsts = new HashSet<_>();
                foinputs = new HashSet<_>(); fotmp1 = new HashSet<_>(); fotmp2 = new HashSet<_>()
            }
            schemas.Value.[key] <- schema
            results |> ConstantsOf |> Seq.iter (processConstant schema.apps schema.sotmp1 schema.sosubsts schema.fotmp1)

    and private processConstant apps soargs sosubsts foargs constant =
        match constant.term with
        | Constant(_, source, typ) ->
            match source with
            | LazyInstantiationEpsilon { term = Ref(TopLevelStack _, _) }
            | KeyInitializedSource _ ->
                foargs.Add constant |> ignore
            | LazyInstantiationEpsilon location ->
                let argId, subst = LazyInstantiations.generateSoArg location
                soargs.Add(argId, List.map (fst >> TypeOf) subst, typ) |> ignore
                sosubsts.Add(argId, List.map snd subst, constant) |> ignore
            | RecursionOutcome(funcId, state, location, _) ->
                let key, generate = RecursionOutcomes.generate funcId location
                let app = {
                    schema = key; constant = constant; resultNum = 0; state = Some state; parameterValues = []
                    soArgs = new Dictionary<_, _>(); foArgs = new Dictionary<_, _>()
                }
                apps.Add app |> ignore
                generateSchema key generate
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
        if visited.Add key && schemas.Value.ContainsKey key then
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
            app.schema <> schema.id && schemas.Value.ContainsKey app.schema &&
                let other = schemas.Value.[app.schema]
                let pushSO = Seq.fold (fun acc i -> (not (schema.soinputs.Contains i) && schema.sotmp1.Add i) || acc) false other.sotmp1
                let pushFO = Seq.fold (fun acc i -> (not (schema.foinputs.Contains i) && schema.fotmp1.Add i) || acc) false other.fotmp1
                pushSO || pushFO  // Do not change to Seq.fold ... || Seq.fold ...! Both must be evaluated.
        while traverseApps foldSchemaApp false rootKey do ()

    let private generateFOArgs (schema : schema) (app : schemaApp) =
        let tmp2 =
            if not (schemas.Value.ContainsKey app.schema)
                then schema.fotmp1 :> seq<_> // TODO: redundant
                else
                    let other = schemas.Value.[app.schema]
                    let schemaFO = new HashSet<_>(other.fotmp1)
                    schemaFO.UnionWith other.foinputs
                    let schemaFOKeys = List.filter (not << app.foArgs.ContainsKey) <| List.ofSeq schemaFO
                    let schemaFOValues =
                        match app.state with
                        | None -> schemaFOKeys
                        | Some state -> List.map (compose state) schemaFOKeys
                    List.iter2 (fun k v -> app.foArgs.Add(k, v)) schemaFOKeys schemaFOValues
                    schemaFOValues :> seq<_>
            |> ConstantsOf
        tmp2.ExceptWith schema.fotmp1
        tmp2.ExceptWith schema.foinputs
        Seq.iter (processConstant schema.apptmp schema.sotmp2 schema.sosubsts schema.fotmp2) tmp2

    let private flushFreshDependencies rootKey =
        let schemaIdToSchemaApp key =
            { schema = key; constant = Nop; resultNum = 0; state = None; parameterValues = []; soArgs = new Dictionary<_, _>(); foArgs = new Dictionary<_,_>() }

        let foldSchemaApp () (schema : schema) (app : schemaApp) =
            generateFOArgs schema app
            match app.state with
            | Some state ->
                let other = schemas.Value.[app.schema]
                for i, _, _ in Seq.append other.soinputs other.sotmp1 do
                    if not (app.soArgs.ContainsKey i)
                    then
                        let key, genSchema = i.GeneratePartialApplication state
                        generateSchema key genSchema
                        let hoapp = schemaIdToSchemaApp key
                        app.soArgs.Add(i, hoapp)
                        generateFOArgs schema hoapp
            | None when schemas.Value.ContainsKey app.schema ->
                let other = schemas.Value.[app.schema]
                for ((soname, _, _) as soarg) in Seq.append other.soinputs other.sotmp1 do
                    if not (schema.sotmp1.Contains soarg) && not (schema.soinputs.Contains soarg) then schema.sotmp2.Add(soarg) |> ignore
                    if not (app.soArgs.ContainsKey soname) then
                        app.soArgs.Add(soname, schemaIdToSchemaApp soname)
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
            List.concat gss, mapper vs)

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
                let apps = new HashSet<_>(apps) |> List.ofSeq
                let unguardedApps = List.map unguardRelApp apps
                let xx = unguardedApps |> List.map List.length |> List.fold (*) 1
                let allApps = unguardedApps |> List.cartesian
                let allApps = allApps |> Seq.map (List.unzip >> mapper) |> List.ofSeq
                allApps

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
            maybe {
                let! schemaApp = Dict.tryGetOptionValue soArgs argId
                let! schema = Dict.tryGetOptionValue schemas.Value schemaApp.schema
                let rel = soRels.Value.[schemaApp.schema]
                let soArgs = schema.soinputs |> Seq.map (fun (argId, _, _) -> foRels.Value.[sourceSchemaId, argId]) |> List.ofSeq
                let foArgs = schema.foinputs |> Seq.map (fun i -> if schemaApp.foArgs.ContainsKey i then schemaApp.foArgs.[i] else i) |> List.ofSeq
                return SoArg(rel, soArgs, foArgs)
            } |> Option.defaultWith (fun () -> FoArg foRels.Value.[sourceSchemaId, argId])

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

        let private filterBody (constraints : term list) (body : _ list) =
            let rec loop acc (apps : _ seq) (terms : term seq) =
                let constants = ConstantsOf terms
                let passed, failed = apps |> List.ofSeq |> List.partition (isApplicationFor constants)
                match passed with
                | [] -> acc
                | _ -> loop (List.append acc passed) failed (passed |> List.collect (fun app -> List.append app.args (app.relArgs |> List.collect (fun relArg -> relArg.Args))))
            loop [] (new HashSet<_>(body)) (new HashSet<_>(constraints))

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
                        let constraints = new HashSet<_>(List.concat (terms::gss)) |> List.ofSeq
                        { constraints = constraints; body = filterBody constraints body; head = None })
                let rules = Seq.map encodeRuleSchema schemas.Value.Keys
                let result = List.concat (queries::rootContextRules::(List.ofSeq rules))
                soRels.Restore()
                foRels.Restore()
                result
            encodeQueryToSchema terms k


    module internal OCaml =

        let private artificialVariableNamePrefix = "g"
        let private codeVariableNamePrefix = "x"

        type internal VariableNamer (funcDeclarations) =
            let variableNames = new Dictionary<obj, string>()
            let funcDecls = new Dictionary<OCamlVar, _>()
            let initFunction (name, (soargs : OCamlVar list, foargs : OCamlVar list, parameters), _) =
                funcDecls.Add(name, ([soargs], [foargs], parameters))

            let splitParamater fsign_arg (argSet : ISet<OCamlVar>) =
                let fsign_arg = fsign_arg |> List.map (List.partition (argSet.Contains))
                List.mapFold (fun wasFound (l, r) -> if wasFound then assert (l = []); [], wasFound elif r = [] then [l], false else [l; r], true) false fsign_arg
                |> fst
                |> List.concat
            let rec fixArgumentOrder = function
                | OConstant _
                | Var _ -> ()
                | UnOp(_, x) -> fixArgumentOrder x
                | BinOp(_, xs) -> List.iter fixArgumentOrder xs
                | Call(f, soargs, foargs, parameters) when funcDecls.ContainsKey f ->
                    let fsign_so, fsign_fo, fsign_params = funcDecls.[f]
                    let fsign_so_size = fsign_so |> List.sumBy List.length
                    let fsign_fo_size = fsign_fo |> List.sumBy List.length
                    let soSet = new HashSet<_>(List.map fst soargs)
                    let foSet = new HashSet<_>(List.map fst foargs)
                    let fsign_so, fsign_fo =
                        if soSet.Count <> fsign_so_size then
                            if not (foSet.Count = 0 && List.length parameters = 0) then __unreachable__()
                            splitParamater fsign_so soSet, []
                        elif foSet.Count <> fsign_fo_size then
                            if not (List.length parameters = 0) then __unreachable__()
                            fsign_so, splitParamater fsign_fo foSet
                        else
                            if List.length parameters > List.length fsign_params then __unreachable__()
                            fsign_so, fsign_fo

                    funcDecls.[f] <- (fsign_so, fsign_fo, fsign_params)
                    soargs @ foargs |> List.map snd |> List.append parameters |> List.iter fixArgumentOrder
                | Call _ -> () // TODO: partial applications =(
                | Assert(g, e) -> fixArgumentOrder g; fixArgumentOrder e
                | Tuple ts -> List.iter fixArgumentOrder ts
                | Let(_, body, expr) -> fixArgumentOrder body; fixArgumentOrder expr
                | IfThenElse(cond, thenExpr, elseExpr) ->
                    fixArgumentOrder cond
                    fixArgumentOrder thenExpr
                    fixArgumentOrder elseExpr
            do List.iter initFunction funcDeclarations
            do List.iter (thd3 >> fixArgumentOrder) funcDeclarations
            let funcDecls' = new Dictionary<OCamlVar, _>()
            do for kvp in funcDecls do
                let name, (soargs, foargs, parameters) = kvp.Key, kvp.Value
                funcDecls'.Add(name, List.concat (soargs @ foargs) @ parameters)
            member private x.variableNames = variableNames
            member private x.funcDeclarations = funcDecls'

            static member private namePattern =
                if Options.ReadableOCamlCode()
                    then fun o -> sprintf "%O_%d" (System.Text.RegularExpressions.Regex.Replace(toString o, "(^[^a-z_]|[^a-zA-Z0-9]+)", "_"))
                    else always (sprintf "%s%d" codeVariableNamePrefix)

            member x.GetName(o : obj) =
                Dict.getValueOrUpdate x.variableNames o (fun () -> VariableNamer.namePattern o x.variableNames.Count)

            member x.OrderFunctionDeclaration(funcName) = x.funcDeclarations.[funcName]

            member x.OrderFunctionArgs(funcName, soargs, foargs, parameters) =
                if x.funcDeclarations.ContainsKey funcName then
                    let funcDeclArgs = x.funcDeclarations.[funcName]
                    let oargsD = new Dictionary<OCamlVar, OCamlExpr>()
                    List.iter oargsD.Add (soargs @ foargs)
                    let oargs =
                        funcDeclArgs
                        |> List.takeWhile oargsD.ContainsKey
                        |> List.map (fun arg -> oargsD.[arg])
                    oargs @ parameters
                else
                    if not (List.length soargs = 0 && List.length foargs = 0) then __notImplemented__()
                    else parameters

        and OCamlVar =
            | OVar of obj
            member internal x.ToString(variableNamer : VariableNamer) =
                match x with
                | OVar s ->
                    match s with
                    | :? string as s when s <> "()" -> s
                    | _ -> variableNamer.GetName(s)

        and OCamlExpr =
            | OConstant of string
            | Var of OCamlVar
            | Let of OCamlVar * OCamlExpr * OCamlExpr
            | IfThenElse of OCamlExpr * OCamlExpr * OCamlExpr
            | BinOp of OperationType * OCamlExpr list
            | UnOp of OperationType * OCamlExpr
            | Call of OCamlVar * (OCamlVar * OCamlExpr) list * (OCamlVar * OCamlExpr) list * OCamlExpr list
            | Assert of OCamlExpr * OCamlExpr                                 // assert (firstexpr) ; secondexpr
            | Tuple of OCamlExpr list
            member internal x.ToString(variableNamer) =
                let toStringE (s : OCamlExpr) = s.ToString(variableNamer)
                let toStringV (s : OCamlVar) = s.ToString(variableNamer)
                match x with
                | OConstant s -> s
                | Var s -> toStringV s
                | UnOp(op, x) ->
                    let op =
                        match op with
                        | OperationType.UnaryMinus -> "-"
                        | OperationType.LogicalNeg -> "not"
                        | _ -> __notImplemented__()
                    sprintf "(%s %s)" op (toStringE x)
                | BinOp(op, xs) ->
                    let op =
                        match op with
                        | OperationType.LogicalAnd -> "&&"
                        | OperationType.LogicalOr -> "||"
                        | OperationType.Add -> "+"
                        | OperationType.Multiply -> "*"
                        | OperationType.Less -> "<"
                        | OperationType.LessOrEqual -> "<="
                        | OperationType.Greater -> ">"
                        | OperationType.GreaterOrEqual -> ">="
                        | OperationType.Equal -> "="
                        | _ -> __notImplemented__()
                    xs |> List.map toStringE |> List.reduce (fun x y -> sprintf "%s %s %s" x op y)
                    |> sprintf "(%s)"
                | Call(f, [], [], []) -> toStringV f
                | Call(f, soargs, foargs, parameters) ->
                    variableNamer.OrderFunctionArgs(f, soargs, foargs, parameters)
                    |> List.map toStringE |> join " "
                    |> sprintf "(%s %s)" (toStringV f)
                | Assert(g, e) -> sprintf "(assert (%s); %s)" (toStringE g) (toStringE e)
                | Tuple [t] -> toStringE t // sprintf "(%s)" <| (ts |> List.map toStringE |> join ", ")
                | Tuple ts -> internalfailf "Tuples are not widely supported: %O" ts
                | Let(var, body, expr) ->
                    sprintf "let %s = %s in %s" (toStringV var) (toStringE body) (toStringE expr)
                | IfThenElse(cond, thenExpr, elseExpr) ->
                    sprintf "(if %s then %s else %s)" (toStringE cond) (toStringE thenExpr) (toStringE elseExpr)
                | _ -> __notImplemented__()
        and OCamlProgram =
            | LetRecursive of (OCamlVar * (OCamlVar list * OCamlVar list * OCamlVar list) * OCamlExpr * termType option) list // function name * arguments * body * return type
            static member GenerateVariableNamer lts =
                new VariableNamer(List.map (fun (name, args, body, _) -> name, args, body) lts)

            override x.ToString() =
                match x with
                | LetRecursive lts ->
                    let variableNamer = OCamlProgram.GenerateVariableNamer lts
                    let toStringE (s : OCamlExpr) = s.ToString(variableNamer)
                    let toStringV (s : OCamlVar) = s.ToString(variableNamer)

                    let functionToString (name, _, body, _) =
                        let args = variableNamer.OrderFunctionDeclaration(name)
                        sprintf "%s %s =\n%s" (toStringV name) (args |> List.map toStringV |> join " ") ("\t" + toStringE body)
                    lts
                    |> List.rev
                    |> List.map functionToString
                    |> join "\nand "
                    |> (+) "let rec "
//                    |> List.map (sprintf "let rec %s\n")
//                    |> join "\n"
//                    match List.map functionToString (List.rev lts) with
//                    | [] -> ""
//                    | lt::lts ->
//                        sprintf "let rec %s\n%s" lt (lts |> List.map ((+) "and ") |> join "\n")

        let private gensym =
            let mutable n = -1
            let pat = sprintf "%s%d" artificialVariableNamePrefix
            fun () ->
                n <- n + 1
                OVar (pat n)

        let private undefinedBehaviorName = OVar "ub"
        let private undefinedBehaviorBody =
            let arg = OVar (Constant "x" ({id = "x"} : emptySource) (Numeric typeof<int>))
            undefinedBehaviorName,
            ([], [], [arg]),
            Assert(BinOp(OperationType.Equal, [OConstant "0"; OConstant "1"]), Var arg),
            None
        let private undefinedBehaviorCall arg = Call(undefinedBehaviorName, [], [], [arg])

        let private makeBinary op = function
            | [] -> __unreachable__()
            | xs -> BinOp(op, xs)

        let private makeStrictBinary op = function
            | [_; _] as xs -> BinOp(op, xs)
            | _ -> __unreachable__()

        let private makeUnary op = function
            | [x] -> UnOp(op, x)
            | _ -> __unreachable__()

        let private encodeConcrete (obj : obj) = function
            | Bool -> OConstant (if (obj :?> bool) then "true" else "false")
            | Numeric t when t = typeof<char> -> __notImplemented__()
            | Numeric _ ->
                match obj with
                | :? concreteHeapAddress as addr ->
                    addr
                    |> Seq.zip Seq.primes
                    |> Seq.map bigint.Pow
                    |> Seq.fold (*) bigint.One
                    |> toString
                    |> OConstant
                | _ -> OConstant (obj.ToString())
            | _ -> __notImplemented__()

        let private encodeExpression op args =
            match op with
            | Operator(op, _) ->
                match op with
                | OperationType.LogicalAnd
                | OperationType.LogicalOr -> makeBinary op args
                | OperationType.Add
                | OperationType.Multiply
                | OperationType.Less
                | OperationType.LessOrEqual
                | OperationType.Greater
                | OperationType.GreaterOrEqual
                | OperationType.Equal -> makeStrictBinary op args
                | OperationType.UnaryMinus
                | OperationType.LogicalNeg -> makeUnary op args
                | _ -> __notImplemented__()
            | _ -> __notImplemented__()

        let private consSet x (s : ISet<'a>) =
            let s' = new HashSet<'a>(s)
            s'.Add x |> ignore
            s' :> ISet<'a>

        let private encodeSchema schema =
            let exploredSchemas = new HashSet<schema>()
            let schemaQueue = new Queue<schema>()
            let schemaQueueEnqueue s = if exploredSchemas.Add s then schemaQueue.Enqueue s

            let processSchema schema =
                let makeFOKeysList s = s.foinputs |> List.ofSeq
                let makeSOKeysList s = s.soinputs |> List.ofSeq |> List.map fst3

                let makeArgs argsKeys argsDictionary =
                    let argsVars = List.map (fun s -> OVar s) argsKeys
                    let argsValues = List.map (Dict.tryGetOptionValue argsDictionary) argsKeys
                    let argsVars', argsValues =
                        List.zip argsVars argsValues
                        |> List.choose (function (k, Some v) -> Some (k, v) | _ -> None)
                        |> List.unzip
                    let isPartialApply = List.length argsVars <> List.length argsVars'
                    isPartialApply, argsVars', argsValues

                let rec encodeSchemaApp dbs app k =
                    if app.constant = Nop && app.foArgs.Count = 0 && app.soArgs.Count = 0 && app.parameterValues.Length = 0 && not (schemas.Value.ContainsKey app.schema)
                        then k (Var (OVar app.schema), dbs)
                        else
                            let callSchema = schemas.Value.[app.schema]
                            schemaQueueEnqueue callSchema

                            let soIsPartialApply, soArgsVars', soArgsValues = makeArgs (makeSOKeysList callSchema) app.soArgs
                            Cps.List.mapFoldk encodeSchemaApp dbs soArgsValues (fun (soArgsValues, dbs) ->

                            let foIsPartialApply, foArgsVars', foArgsValues = makeArgs (makeFOKeysList callSchema) app.foArgs
                            encodeList dbs foArgsValues (fun (foArgsValues, dbs) ->

                            encodeList dbs app.parameterValues (fun (parameterInputs, dbs) ->
                            if soIsPartialApply then
                                assert (foArgsValues = [] && parameterInputs = [])
                            elif foIsPartialApply then
                                assert (parameterInputs = [])
                            let soArgs = List.zip soArgsVars' soArgsValues
                            let foArgs = List.zip foArgsVars' foArgsValues
                            let call = Call(OVar app.schema, soArgs, foArgs, parameterInputs)
                            k (call, dbs))))

                and encodeConstant src typ t ((definedVars : ISet<term>, _) as dbs) k =
                    let ovar = OVar t
                    let var = Var ovar

//                    let isDefinedFast = // should be faster than definedVars.Contains
//                        let srcType = src.GetType()
//                        let isEqualToT c =
//                            match c.term with
//                            | Constant(_, src', typ') when typ = typ' && src'.GetType() = srcType -> Some src'
//                            | _ -> None
//                        let similarVars = definedVars |> List.ofSeq |> List.choose isEqualToT
//                        List.exists ((=) src) similarVars
                    if definedVars.Contains t// isDefinedFast
                        then k (var, dbs)
                        else
                            let funcApp = schema.apps |> Seq.tryFind (fun sapp -> sapp.constant = t)
                            match funcApp with
                            | Some app ->
                                encodeSchemaApp dbs app (fun (call, dbs) ->
                                if Options.InlineOCaml()
                                    then k (call, dbs)
                                    else
                                        let (definedVars, bodyGenerator) = dbs
                                        k (var, (consSet t definedVars, fun restBody -> bodyGenerator (Let(ovar, call, restBody)))))
                            | None ->
                                match schema.sosubsts |> Seq.tryFind (thd3 >> ((=) t)) with
                                | None -> __unreachable__()
                                | Some (argFuncName, argFuncArgs, _) ->
                                    encodeList dbs argFuncArgs (fun (argFuncArgs, dbs) ->
                                    let call = Call(OVar argFuncName, [], [], argFuncArgs)
                                    if Options.InlineOCaml()
                                        then k (call, dbs)
                                        else
                                            let (definedVars, bodyGenerator) = dbs
                                            k (var, (consSet t definedVars, fun restBody -> bodyGenerator (Let(ovar, call, restBody)))))

                and encodeTerm dbs t k =
//                    let tt = (">>> Encode: " + toString t)

//                    let tt2 = tt.Substring(0, min 100 tt.Length)
//                    System.IO.File.AppendAllText("/tmp/log1.txt", "\n\n" + toString t)
//                    System.IO.File.WriteAllText("/tmp/log1.txt", toString t)
//                    printfn "%s" tt
//                    System.Console.Out.Flush()
                    match t.term with
                    | Concrete(obj, typ) -> k (encodeConcrete obj typ, dbs)
                    | Constant(name, src, typ) -> encodeConstant src typ t dbs k
                    | Expression(op, args, typ) ->
                        encodeList dbs args (fun (args, dbs) ->
                        k (encodeExpression op args, dbs))
                    | Union [] -> __notImplemented__()
                    | Union gvs ->
                        let ite (cond, thenBranch) elseBranch = IfThenElse(cond, thenBranch, elseBranch)
                        let gs, vs = List.unzip gvs
                        let ubDefaultValue =
                            match List.head vs |> TypeOf with
                            | Bool -> OConstant "true"
                            | _ -> OConstant "0" // TODO: it's not always int
                        encodeList dbs gs (fun (gs, (definedVars, bodyGenerator)) ->
                        Cps.List.mapk (encodeTerm (definedVars, id)) vs (fun vs ->
                        let unionAsAValue =
                            vs
                            |> List.map (fun (v, (_, body)) -> body v)
                            |> List.zip gs
                            |> fun gvs -> List.foldBack ite gvs (undefinedBehaviorCall ubDefaultValue)
                        let unionGenName = gensym ()
                        k (Var unionGenName, (definedVars, fun restBody -> bodyGenerator (Let(unionGenName, unionAsAValue, restBody))))))
                    | Ref(TopLevelHeap(tl, _, _), _) -> encodeTerm dbs tl k
                    | Ref(NullAddress, []) -> encodeTerm dbs (MakeZeroAddress ()) k
                    | _ -> internalfailf "Encode: %O" t

                and encodeList (definedVars, definitions) ts k =
                    Cps.List.mapFoldk encodeTerm (definedVars, definitions) ts k

                let foKeysList = makeFOKeysList schema
                let foArgsVars = List.map (fun s -> OVar s) foKeysList
                let definedVars = new HashSet<term>(foKeysList @ schema.parameterInputs) // we don't need to include soinputs here because there is a check for .sosubsts in encodeConstant
                let soKeysList = makeSOKeysList schema
                let soArgsVars = List.map (fun s -> OVar s) soKeysList
                let returnType =
                    match schema.results with
                    | [result] -> Some(TypeOf result)
                    | _::_ -> None // multiple results are not supported due to weak Tuple support in OCaml verifiers
                    | [] -> __unreachable__()
//                System.IO.File.AppendAllText("/tmp/log1.txt", "\n\nschema: " + (schema.results |> List.map toString |> join " ; "))
                encodeList (definedVars, id) schema.results (fun (results, (_, bodyGenerator)) ->
                encodeList (definedVars, id) schema.parameterInputs (fun (parameters, _) ->
                let parameters = parameters |> List.map (function Var v -> v | _ -> __unreachable__())
                (OVar schema.id, (soArgsVars, foArgsVars, parameters), results, bodyGenerator, returnType)))

            schemaQueueEnqueue schema
            seq {
                while schemaQueue.Count <> 0 do
                    yield processSchema (schemaQueue.Dequeue())
            } |> List.ofSeq

        let encodeQuery terms =
            let k (querySchema : schema) =
                let x = schemas.Value
                printfn "%d" x.Count
                match encodeSchema querySchema with
                | [] -> __unreachable__()
                | (_, mainArgs, queries, mainBody, _)::functions ->
                    let functions =
                        let substituteResult = function
                            | (name, args, [result], body, returnType) -> name, args, body result, returnType
                            | _ -> __unreachable__()
                        List.map substituteResult functions
                    let query =
                        let okReturnCode = OConstant "0"
                        IfThenElse(BinOp(OperationType.LogicalAnd, queries), undefinedBehaviorCall okReturnCode, okReturnCode)
                    let main = OVar "main", mainArgs, mainBody query, None
//                    System.IO.File.AppendAllText("/tmp/log1.txt", "\n\n" + "DONE ENCODING")
//                    let main = OVar "main", mainArgs, Call(OVar "main2", [], List.map Var mainArgs), None
                    LetRecursive (main::functions @ [undefinedBehaviorBody])

//            if List.contains (Union []) terms
//                then
//                    LetRecursive [(OVar "main", ([OVar "x"], [], []), Assert(BinOp(OperationType.Equal, [OConstant "0"; OConstant "0"]), OConstant "0"), None); undefinedBehaviorBody]
//                else
            encodeQueryToSchema terms k