namespace VSharp.Core

open System.Collections.Generic
open VSharp.Logger

type functionSummary = { result : term; state : state }


module internal Database =
    let private summaries = new Dictionary<IFunctionIdentifier, functionSummary>()

    let reported id = summaries.ContainsKey id

    let report id result state =
//        if (summaries.ContainsKey id) then VSharp.Prelude.__unreachable__()
        let result = ControlFlow.resultToTermWithNoReturnMarker result
        let summary = { result = result; state = state}
        printLog Info "For %O got %O\n%O\n\n" id result (State.dumpMemory state)
        summaries.[id] <- summary
//        summaries.Add(id, summary)
        summary

    let querySummary id =
        if summaries.ContainsKey id then Some(summaries.[id]) else None
