namespace BenchUtils
open System 

type Timed<'a> = { Ticks : int64 ; Value : 'a ; Label : string }

type BenchAction =
    | TimedLoopAction of (unit -> unit)

type BenchResult = 
    | TimedLoop of Timed<int>

module BenchTools = 

    let rec findMaxElapsedTicks resultList acc  = 
        match resultList with 
        | (TimedLoop t)::tl -> (if t.Ticks < acc then acc else t.Ticks) |> findMaxElapsedTicks tl
        | _ -> acc
    
    let applyLooping sizeLoop func2BeApply args = 
        for it in 1..sizeLoop do
            func2BeApply args |> ignore

    let timedRepeatAction loopSize measureWarmUp internalLoop = 
        let timed = { Ticks = (int64)0 ; Value = 0 ; Label = "Main loop counter" }
        let sw = new System.Diagnostics.Stopwatch()
        let rec repeat count time = 
            if count = loopSize 
            then time
            else 
                sw.Start() |> ignore
                internalLoop() |> ignore
                sw.Stop() |> ignore
                let time' = 
                    if count > measureWarmUp 
                    then { time with Ticks = time.Ticks + sw.ElapsedTicks ; Value = time.Value+1 }
                    else time
                sw.Reset()
                repeat (count+1) time'
        (repeat 0 timed) |> TimedLoop

    let repeatBench mainLoopSize measureWarmUp benchAction =
        timedRepeatAction mainLoopSize measureWarmUp benchAction