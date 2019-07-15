namespace VSharp.Core
open System

type ExplorationMode = TrustConventions | CompleteExploration
type RecursionUnrollingModeType = SmartUnrolling | NeverUnroll | AlwaysUnroll

module public Options =

    let mutable private explorationMode = TrustConventions
    let public ExplorationMode() = explorationMode

    let mutable private recursionUnrollingMode =
        let mode = Environment.GetEnvironmentVariable("VSHARPMODE")
        match mode with
        | "never" -> NeverUnroll
        | "smart" | null | _ -> SmartUnrolling

    let public RecursionUnrollingMode () = recursionUnrollingMode
