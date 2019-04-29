namespace VSharp

open Microsoft.Z3
open VSharp.Core

type public ISolver<'TAst, 'TResult> =
    abstract member Solve : term list -> 'TResult

type public ISmtSolver<'TAst> =
    inherit ISolver<'TAst, SmtResult>

type public IZ3Solver =
    inherit ISmtSolver<AST>

type public Z3Solver() =
    member x.SolveWithSOSolver sosolver terms =
        match sosolver with
        | HORUSSolverOption ->
            let sochcs = Encode.Relations.encodeQuery terms
            printfn "\n\n\n ------------------------------ SOCHCS -------------------------------------\n\n%s\n\n\n" (CHCs.dump sochcs)
            let chcs = CHCs.toFirstOrder sochcs
            Z3.solve chcs
        | R_typeSolverOption ->
            let ocamlProgram = Encode.OCaml.encodeQuery terms
            R_typeSolver().Solve ocamlProgram
        | DefMonoSolverOption ->
            let ocamlProgram = Encode.OCaml.encodeQuery terms
            DefMonoSolver().Solve ocamlProgram
        | DOrderSolverOption ->
            let ocamlProgram = Encode.OCaml.encodeQuery terms
            DOrderSolver().Solve ocamlProgram
        | MochiSolverOption ->
            let ocamlProgram = Encode.OCaml.encodeQuery terms
            MochiSolver().Solve ocamlProgram
        | AllSolversOption ->
            let ocamlProgram = Encode.OCaml.encodeQuery terms
            let sochcs = Encode.Relations.encodeQuery terms

            match ocamlProgram with
            | Encode.OCaml.LetRecursive lts when List.length lts > 2 ->
                let bench_path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "benchmarks")
                let benchmarks =
                    System.IO.Directory.GetFiles bench_path
                    |> List.ofArray
                    |> List.choose (fun path -> if System.IO.Path.GetExtension path = ".terms" then Some(int(System.IO.Path.GetFileNameWithoutExtension path)) else None)
                let nextNamePrefix =
                    match benchmarks with
                    | [] -> 0
                    | _ -> List.max benchmarks + 1
                let termFile = System.IO.Path.Combine(bench_path, nextNamePrefix.ToString() + ".terms")
                System.IO.File.WriteAllLines(termFile, terms |> List.map toString |> Array.ofList)

                let code = toString ocamlProgram
                System.IO.File.WriteAllText(System.IO.Path.ChangeExtension(termFile, "ml"), code)

                System.IO.File.WriteAllText(System.IO.Path.ChangeExtension(termFile, "sochcs"), CHCs.dump sochcs)

                let results =
                    [
                        "HORUS", Z3.solve (CHCs.toFirstOrder sochcs);
                        "r_type", R_typeSolver().Solve ocamlProgram;
    //                    "DefMono", DefMonoSolver().Solve ocamlProgram; // Not fully supported: no `not` operator
//                        "DOrder", DOrderSolver().SolveCode code; // Doesn't work due to function reordering
                        "Mochi", MochiSolver().SolveCode code;
                        "Human", HumanSolver().SolveNumber nextNamePrefix;
                    ]
                let resultsS = results |> List.map (fun (name, res) -> sprintf "%s\t%O" name res) |> Array.ofList
                System.IO.File.WriteAllLines(System.IO.Path.ChangeExtension(termFile, "results"), resultsS)
                results |> List.last |> snd
            | _ -> MochiSolver().Solve ocamlProgram


    interface IZ3Solver with
        member x.Solve terms = x.SolveWithSOSolver (Options.SOCHCSolver ()) terms

type public Z3Simplifier() =
    interface IPropositionalSimplifier with
        member x.Simplify t = Z3.simplifyPropositional t

type public SmtSolverWrapper<'TAst>(solver : ISmtSolver<'TAst>) =
    interface VSharp.Core.ISolver with
        override x.Solve term =
            match solver.Solve [term] with
            | SmtSat _ -> VSharp.Core.Sat
            | SmtUnsat -> VSharp.Core.Unsat
            | SmtUnknown _ -> VSharp.Core.Unknown
        override x.SolvePathCondition term pc =
            // TODO: solving should be incremental!
            match solver.Solve (term::pc) with
            | SmtSat _ -> VSharp.Core.Sat
            | SmtUnsat -> VSharp.Core.Unsat
            | SmtUnknown _ -> VSharp.Core.Unknown
