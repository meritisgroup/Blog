
module PrimitiveObsession
    
    open BenchUtils

    type Weigth = float

    type UnitaryPrice = float

    type _Weigth = { Value:float }

    type _UnitaryPrice = { Value:float }

    let computePrice (weigth:float, unitaryPrice:float) = weigth*unitaryPrice

    let computePrice' (weigth:Weigth, unitaryPrice:UnitaryPrice) = weigth*unitaryPrice

    let computePrice'' (weigth:_Weigth, unitaryPrice:_UnitaryPrice) = weigth.Value*unitaryPrice.Value

    let testPrimitiveObsession unit = 
        let farg1 = 2.0
        let sarg1 = 3.0
        let (farg2:Weigth) = 2.0
        let (sarg2:UnitaryPrice) = 3.0
        let (farg3:_Weigth) = { Value = 2.0 }
        let (sarg3:_UnitaryPrice) = { Value = 3.0 }
        let func1 () = BenchTools.applyLooping 1000000 computePrice (farg1, sarg1)
        let func2 () = BenchTools.applyLooping 1000000 computePrice' (farg2, sarg2)
        let func3 () = BenchTools.applyLooping 1000000 computePrice'' (farg3, sarg3)
        let results  = (BenchTools.repeatBench 100 50 func1)::(BenchTools.repeatBench 100 50 func2)::(BenchTools.repeatBench 100 50 func3)::[]
    
        let maxTicks = BenchTools.findMaxElapsedTicks results ((int64)0) |> float
    
        let header = "Function name"::"Elapsed ticks "::"Ticks per main cycle"::"Percent of max"::[]
        let functNames = "computePrice"::"computePrice'"::"computePrice''"::[]
        let lineBuider (TimedLoop item) = (item.Ticks.ToString())::(((((float)item.Ticks)/((float)item.Value))).ToString())::((((float)item.Ticks)*100.0/maxTicks).ToString())::[]
        let preLines = List.map lineBuider results
        let lines = List.map2 (fun funName line -> funName::line ) functNames preLines
        ResultFormater.formatResult 1 1 "-" "|" header lines
