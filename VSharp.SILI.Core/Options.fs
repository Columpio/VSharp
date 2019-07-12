namespace VSharp.Core

type ExplorationMode = TrustConventions | CompleteExploration
type RecursionUnrollingModeType = SmartUnrolling | NeverUnroll | AlwaysUnroll

module public Options =

    let mutable private explorationMode = TrustConventions
    let public ExplorationMode() = explorationMode

    let mutable private recursionUnrollingMode = SmartUnrolling
    let public RecursionUnrollingMode () = recursionUnrollingMode
