namespace VSharp.Core

type ExplorationMode = TrustConventions | CompleteExploration
type RecursionUnrollingModeType = SmartUnrolling | NeverUnroll | AlwaysUnroll
type SOCHCSolver = HORUSSolverOption | R_typeSolverOption | DefMonoSolverOption | DOrderSolverOption | MochiSolverOption | AllSolversOption

module public Options =

    let mutable private explorationMode = TrustConventions
    let public ExplorationMode() = explorationMode

//    let mutable private recursionUnrollingMode = SmartUnrolling
    let mutable private recursionUnrollingMode = NeverUnroll
    let public RecursionUnrollingMode () = recursionUnrollingMode

    let mutable private sochcSolver = AllSolversOption
    let public SOCHCSolver () = sochcSolver

    let mutable private readableOCamlCode = false
    let public ReadableOCamlCode () = readableOCamlCode