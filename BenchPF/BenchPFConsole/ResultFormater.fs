namespace BenchUtils
open System 

module ResultFormater =

    let repeatCharacter n a =
        let rec buildList it acc = 
            match it with 
            | 0 -> acc
            | _ -> buildList (it-1) (a::acc)
        buildList n [] |> (fun l -> List.toArray l) |> (fun (t:string array) -> System.String.Concat(t))
    
    let rec addSep sep elems = 
        match elems with
        | hd::tl when tl <> [] -> hd::sep::(addSep sep tl)
        | _ as rest -> rest
    
    let listToString listOfString = List.toArray listOfString |> (fun (t:string array) -> System.String.Concat(t))

    let headerSeparator markCharracter colSep colWidths = 
        List.foldBack (fun colWidth acc -> (repeatCharacter colWidth markCharracter)::acc) colWidths []
        |> addSep colSep 
        |> listToString

    let findColWith header lines = 
        let buildMaxLengthList line = List.foldBack (fun item acc -> (String.length item)::acc) line []
        let buildListOfMax list1 list2 = List.map2 (fun locMax item -> max locMax item) list1 list2
        let startWidth = buildMaxLengthList header
        List.map buildMaxLengthList lines |> List.fold (fun acc len -> buildListOfMax acc len) startWidth  
    
    let formatLine colSep padLeft colwidths line =
        let stringFormat width (str:string) = str.PadLeft(str.Length+padLeft).PadRight(width)
        List.map2 stringFormat colwidths line 
        |> addSep colSep 
        |> listToString

    let formatResult padLeft padrigth lineSep colSep headerList lineslist =
        let padding     = padLeft + padrigth
        let colWidths   = findColWith headerList lineslist |> List.map (fun elem -> elem+padding)
        let formatLine' = formatLine colSep padLeft colWidths
        let corpus      = List.foldBack (fun line acc -> (formatLine' line)::acc) lineslist []
        if headerList = [] 
        then corpus 
        else (formatLine' headerList)::(headerSeparator lineSep colSep colWidths)::corpus
        |> addSep System.Environment.NewLine
        |> listToString
