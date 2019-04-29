namespace VSharp
open Encode.OCaml
open VSharp
open VSharp.Core
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Collections
open Logger

type private OCamlCode = string
type private SMTlib2Code = string list

module private TranslationUtils =
    type private TempFile(contents : string) =
        let path = Path.GetTempFileName()
        do File.WriteAllText(path, contents)
        member x.Path = path
        interface System.IDisposable with
            member x.Dispose() = () // TODO: File.Delete(x.Path)

    let internal flipSMTResult = function
        | SmtSat _ -> SmtUnsat
        | SmtUnsat -> SmtSat null
        | t -> t

    let internal optionToSMTResult = function
        | Some r -> r
        | None -> SmtUnknown "Time limit"

    let internal runToolOnCode code setupProcess =
        use file = new TempFile(code)
        printLog Trace "Code: %s" file.Path
        use p = new System.Diagnostics.Process()
        p.StartInfo.WorkingDirectory <- Path.Combine(__SOURCE_DIRECTORY__, "foreign_solvers")
        p.StartInfo.RedirectStandardOutput <- true
        p.StartInfo.RedirectStandardError <- true
        p.StartInfo.UseShellExecute <- false
        setupProcess p.StartInfo file.Path
        p.Start() |> ignore
        let output = new Generic.List<string>()
        let error = new Generic.List<string>()
        let addToList (l : Generic.List<_>) x = if x <> null then l.Add x
        p.OutputDataReceived.Add(fun args -> addToList output args.Data)
        p.ErrorDataReceived.Add(fun args -> addToList error args.Data)
        p.BeginErrorReadLine()
        p.BeginOutputReadLine()
        let secondsTimeout = 30
        let exited = p.WaitForExit(secondsTimeout * 1000)
        if not exited then p.Kill()

        match List.ofSeq output, List.ofSeq error with
        | [], [] -> None
        | res, [] -> Some res
        | res, err -> internalfail (res @ err |> join "\n")


type internal R_typeSolver() =
    member private x.OCamlToSMT (program : OCamlProgram) : SMTlib2Code option =
        let setup (si : System.Diagnostics.ProcessStartInfo) codePath =
            si.FileName <- Path.Combine(si.WorkingDirectory, "r_type")
            si.Arguments <- sprintf "--infer off %s" codePath
        let code = toString program
        TranslationUtils.runToolOnCode code setup

    member x.Solve (program : OCamlProgram) =
        maybe {
            let! smtlib2Code = x.OCamlToSMT program
            let result =
                Z3.solveSMTlib2 smtlib2Code
                |> TranslationUtils.flipSMTResult
                |> fun r -> printLog Trace "SOSolver got: %O" r; r
            return result
        } |> TranslationUtils.optionToSMTResult

type internal DefMonoSolver() =
    static member private supportedRelations = [
        OperationType.Less; OperationType.LessOrEqual;
        OperationType.Greater; OperationType.GreaterOrEqual
    ]

    member private x.OCamlToMonotone (program : OCamlProgram) =
        match program with
        | LetRecursive lts ->
            let variableNamer = OCamlProgram.GenerateVariableNamer lts
            let toStringV (s : OCamlVar) = s.ToString(variableNamer)
            let rec toStringE = function
                | OConstant s -> s
                | Var s -> toStringV s
                | UnOp(OperationType.LogicalNeg, x) ->
                    let negateBinOp = function
                        | OperationType.LogicalAnd -> OperationType.LogicalOr
                        | OperationType.LogicalOr -> OperationType.LogicalAnd
                        | OperationType.Less -> OperationType.GreaterOrEqual
                        | OperationType.LessOrEqual -> OperationType.Greater
                        | OperationType.GreaterOrEqual -> OperationType.Less
                        | OperationType.Greater -> OperationType.LessOrEqual
                        | _ -> __notImplemented__()
                    match x with
                    | Var (OVar s) -> Var (OVar ("not", s)) // DefMono doesn't support implication and negation, so we should overapproximate here
                    | UnOp(OperationType.LogicalNeg, x) -> x
                    | BinOp(OperationType.Equal, xy) ->
                        BinOp(OperationType.LogicalOr,
                              [BinOp(OperationType.Less, xy); BinOp(OperationType.Greater, xy)])
                    | BinOp(OperationType.LogicalAnd as op, xs)
                    | BinOp(OperationType.LogicalOr as op, xs) ->
                        BinOp(negateBinOp op, List.map (fun x -> UnOp(OperationType.LogicalNeg, x)) xs)
                    | BinOp(op, xs) when List.contains op DefMonoSolver.supportedRelations ->
                        BinOp(negateBinOp op, xs)
                    | _ -> __unreachable__()
                    |> toStringE
                | BinOp(op, xs) ->
                    let op =
                        match op with
                        | OperationType.LogicalAnd -> "&&"
                        | OperationType.LogicalOr -> "||"
                        | OperationType.Add -> "+"
                        | OperationType.LessOrEqual -> "<="
                        | OperationType.Less -> "<"
                        | OperationType.GreaterOrEqual -> ">="
                        | OperationType.Greater -> ">"
                        | OperationType.Equal -> "="
                        | _ -> __notImplemented__()
                    xs |> List.map toStringE |> List.reduce (fun x y -> sprintf "%s %s %s" x op y)
                    |> sprintf "(%s)"
                | Call(f, [], [], []) -> toStringV f
//                | Call(f, oargs, parameters) when f = OVar "main2" ->
//                    variableNamer.OrderFunctionArgs(f, oargs) @ parameters
//                    |> List.map toStringE |> join " "
//                    |> sprintf "(%s %s)" (toStringV f)
                | Assert(g, e) -> toStringE g
                | Tuple [t] -> toStringE t // sprintf "(%s)" <| (ts |> List.map toStringE |> join ", ")
                | Tuple ts -> internalfailf "Tuples are not widely supported: %O" ts
//                | Let(var, body, expr) ->
//                    sprintf "let %s = %s in %s" (toStringV var) (toStringE body) (toStringE expr)
//                | IfThenElse(cond, thenExpr, elseExpr) ->
//                    sprintf "(if %s then %s else %s)" (toStringE cond) (toStringE thenExpr) (toStringE elseExpr)
                | _ -> __notImplemented__()
            let toStringType = function
                | Bool -> "bool"
                | Numeric t when t = typeof<int> -> "int"
                | _ -> __notImplemented__()
            let collectArgumentTypes (name, (soargs, foargs, ps), body, returnType) =
                let getArgType = function
                    | OVar (:? term as t) -> toStringType <| Terms.TypeOf t
                    | _ -> __notImplemented__()
                let args = soargs @ foargs @ ps
                let args = List.map (fun arg -> arg, getArgType arg) args
                let args =
                    match returnType with
                    | Some returnType -> args @ [(OVar "res", toStringType returnType)]
                    | None -> args
                name, args, body
            let lts = List.map collectArgumentTypes lts
            let functionToString (name, args, body) =
                let args = args |> List.map (fun (arg, typ) -> sprintf "\%s: %s. " (toStringV arg) typ)
                sprintf "%s := %s(\n%s\n);" (toStringV name) (join "" args) (toStringE body)
            let environmentToString (name, args, _) =
                sprintf "%s : %s -> bool" (toStringV name) (args |> List.map snd |> join " -> ")
            let query =
                let mainArgs =
                    lts
                    |> List.find (fun (OVar name, _, _) -> name.ToString() = "main") |> snd3
                    |> List.map (mapfst toStringV)
                let args = mainArgs |> List.map (fun (arg, typ) -> sprintf "E %s: %s." arg typ) |> join " "
                let call = mainArgs |> List.map fst |> join " "
                sprintf "%s main %s" args call
            let code =
                List.concat [
                    ["environment"];
                    List.map environmentToString lts;
                    [""; "program"];
                    List.map functionToString lts;
                    [""; "goal"];
                    [query]
                ]
            join "\n" code

    member private x.MonotoneToSMT code : SMTlib2Code option =
        let setup (si : System.Diagnostics.ProcessStartInfo) codePath =
            si.FileName <- Path.Combine(si.WorkingDirectory, "DefMono")
            si.Arguments <- sprintf "--file %s --pure" codePath
        TranslationUtils.runToolOnCode code setup

    member x.Solve (program : OCamlProgram) =
        maybe {
            let monotoneCode = x.OCamlToMonotone program
            let! smtlib2Code = x.MonotoneToSMT monotoneCode
            let result =
                Z3.solveSMTlib2 smtlib2Code
                |> TranslationUtils.flipSMTResult
                |> fun r -> printLog Trace "SOSolver got: %O" r; r
            return result
        } |> TranslationUtils.optionToSMTResult

type internal DOrderSolver() =
    member x.SolveCode code =
        let setup (si : System.Diagnostics.ProcessStartInfo) codePath =
            si.EnvironmentVariables.["LD_LIBRARY_PATH"] <- sprintf "/home/columpio/Documents/HOCHC/DOrder/external/z3/lib/:%s" si.EnvironmentVariables.["LD_LIBRARY_PATH"]
            si.WorkingDirectory <- Path.Combine(si.WorkingDirectory, "DOrder")
            si.FileName <- Path.Combine(si.WorkingDirectory, "DOrder")
            si.Arguments <- sprintf "-I /home/columpio/Documents/HOCHC/DOrder/ -v 0 -w A %s" codePath

        maybe {
            let! result = TranslationUtils.runToolOnCode code setup
            if Regex.Matches(join "\n" result, "Assertion may fail").Count > 1
                then return SmtSat null
                else return SmtUnsat
        } |> TranslationUtils.optionToSMTResult

    member x.Solve (program : OCamlProgram) =
        let code = toString program
        x.SolveCode code

type internal MochiSolver() =
    member x.SolveCode code =
        let setup (si : System.Diagnostics.ProcessStartInfo) codePath =
            si.FileName <- Path.Combine(si.WorkingDirectory, "mochi")
            si.Arguments <- sprintf "-only-result %s" codePath
        maybe {
            let! result = TranslationUtils.runToolOnCode code setup
            printLog Trace "Mochi got:\n%s" (join "\n" result)
            match result |> List.tryFind (fun r -> r.Contains "afe!") with
            | Some s when s = "Unsafe!" ->
//                let cex_index = result |> List.findIndex (fun s -> s.StartsWith "Input for")
//                let cex = result.[cex_index+1] // TODO: use it
                return SmtSat null
            | Some s when s = "Safe!" -> return SmtUnsat
            | _ ->
//                Verification failed:
//                    MoCHi could not refute an infeasible error path
//                    due to the incompleteness of the refinement type system
                return SmtUnknown "Incompleteness of the refinement type system"
        } |> TranslationUtils.optionToSMTResult

    member x.Solve (program : OCamlProgram) =
        let code = toString program
        x.SolveCode code

type internal HumanSolver() =
    member x.SolveNumber n =
        let answer_filepath = Path.Combine(__SOURCE_DIRECTORY__, "benchmarks_gold", sprintf "%d.gold" n)
        if File.Exists answer_filepath
            then
                let answer = File.ReadAllText answer_filepath
                if answer.StartsWith "sat" then SmtSat null
                elif answer.StartsWith "unsat" then SmtUnsat
                else SmtUnknown (sprintf "Invalid gold file (%s) format:\n%s" answer_filepath answer)
            else SmtUnknown "No gold file specified by user!"
