module Legislative.Filters

open Legislative.Types

let extractEffectElementType (entry:LegFeed.Entry) =
    match entry.Content with
    | Some c -> c.Effect.Type
    | None when entry.Effect.IsSome -> entry.Effect.Value.Type
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)
        

let isCommencementEntry entry =
    match (extractEffectElementType entry) with
    | "Commencement Order" -> true
    | "coming into force" -> true
    | "Appointed Day(s)" -> true
    | t when t.Contains "amendment to earlier commencing S" -> true
    //| (e:LegFeed.Entry) when (e.Content.Effect.Type = "Commencement Order") || (e.Content.Effect.Type = "coming into force") || (e.Content.Effect.Type = "Appointed Day(s)") || ((e.Content.Effect.Type.Contains "amendment to earlier commencing S")) -> true
    | _ -> false


let extractAffectedClass (entry:LegFeed.Entry) = 
    match entry.Content with
    | Some c -> c.Effect.AffectedClass
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectedClass
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectingdClass (entry:LegFeed.Entry) = 
    match entry.Content with
    | Some c -> c.Effect.AffectingClass
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectingClass
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectedTitles (entry: LegFeed.Entry) = 
    match entry.Content with
    | Some c -> c.Effect.AffectedTitles
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectedTitles
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectingTitles (entry: LegFeed.Entry) = 
    match entry.Content with
    | Some c -> c.Effect.AffectingTitles |> Array.map(fun f -> f.Value)
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectingTitles
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectedYear (entry: LegFeed.Entry) =
    match entry.Content with
    | Some c -> c.Effect.AffectedYear
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectedYear
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectingYear (entry: LegFeed.Entry) =
    match entry.Content with
    | Some c -> c.Effect.AffectingYear
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectingYear
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectingNumber (entry: LegFeed.Entry) =
    match entry.Content with
    | Some c -> c.Effect.AffectingNumber
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectingNumber
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectedNumber (entry: LegFeed.Entry) =
    match entry.Content with
    | Some c -> c.Effect.AffectedNumber
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectedNumber
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectedProvisions (entry: LegFeed.Entry) =
    match entry.Content with
    | Some c -> c.Effect.AffectedProvisions
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectedProvisions
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectingProvisions (entry: LegFeed.Entry) =
    match entry.Content with
    | Some c -> c.Effect.AffectingProvisions
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectingProvisions
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractEffectId (entry: LegFeed.Entry) =
    match entry.Content with
    | Some c -> c.Effect.EffectId
    | None when entry.Effect.IsSome -> entry.Effect.Value.EffectId
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectedUri (entry: LegFeed.Entry) =
    match entry.Content with
    | Some c -> c.Effect.AffectedUri
    | None when entry.Effect.IsSome -> entry.Effect.Value.AffectedUri
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractEffectType (entry: LegFeed.Entry) =
    match entry.Content with
    | Some c -> c.Effect.Type
    | None when entry.Effect.IsSome -> entry.Effect.Value.Type
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let extractAffectedProvisionsSections (entry: LegFeed.Entry): string array = 
    match entry.Content with
    | Some c -> c.Effect.AffectedProvisions2.Sections |> Array.map(fun s -> s.String.Value)
    | None when entry.Effect.IsSome -> [|entry.Effect.Value.AffectedProvisions2|]
    | _ -> failwith (sprintf "No Effect Element found in %s" entry.Id)

let siBoundVolumesFilter (entry:LegFeed.Entry) : bool =
        // try
            let affectedClass = extractAffectedClass entry
            let affectingClass = extractAffectingdClass entry
            match isCommencementEntry entry with
                | true -> false
                | false -> 
                    match (affectedClass, affectingClass) with
                    | ("NorthernIrelandStatutoryRule",_)  -> false
                    | ("NorthernIrelandAct",_)  -> false
                    | ("NorthernIrelandOrderInCouncil",_)  -> false
                    | ("IrelandAct",_) -> false
                    | (_,"WelshNationalAssemblyAct") -> true
                    | (_,"ScottishAct") -> true
                    | (_,"UnitedKingdomPublicGeneralAct") -> true
                    | (_,"UnitedKingdomStatutoryInstrument") -> true
                    | (_,"ScottishStatutoryInstrument") -> true
                    | (_,"UnitedKingdomChurchInstrument") -> true
                    | (_,"UnitedKingdomChurchMeasure") -> true
                    | (_,"UnitedKingdomLocalAct") -> true
                    | (_,"WelshStatutoryInstrument") -> true
                    | (_,"WelshParliamentAct") -> true
                    | _ -> false
        // with
        //     | :? System.Exception as e ->
        //         printf "Error processing Entry %s, %s" entry.Id e.Message
        //         false

let siBoundVolumesCommencementFilter (entry: LegFeed.Entry): bool = 
    //tweaked types as some files have new content wrapper element but sibv doesn't seem to so cater for both options with option type
    //try      
        let affectedClass = extractAffectedClass entry
        let affectingClass = extractAffectingdClass entry
        
        match isCommencementEntry entry with
            | false -> false
            | true -> 
                match (affectedClass, affectingClass) with
                | ("NorthernIrelandStatutoryRule",_)  -> false
                | ("NorthernIrelandAct",_)  -> false
                | ("NorthernIrelandOrderInCouncil",_)  -> false
                | ("IrelandAct",_) -> false
                | (_,"WelshNationalAssemblyAct") -> true
                | (_,"ScottishAct") -> true
                | (_,"UnitedKingdomPublicGeneralAct") -> true
                | (_,"UnitedKingdomStatutoryInstrument") -> true
                | (_,"ScottishStatutoryInstrument") -> true
                | (_,"UnitedKingdomChurchInstrument") -> true
                | (_,"UnitedKingdomChurchMeasure") -> true
                | (_,"UnitedKingdomLocalAct") -> true
                | (_,"WelshStatutoryInstrument") -> true
                | (_,"WelshParliamentAct") -> true
                | _ -> false
        
        
    // with
    //     | :? System.Exception as e ->
    //         printf "Error processing Entry %s, %s" entry.Id e.Message
    //         false

let chronTableFilter (entry: LegFeed.Entry) : bool =    
    match isCommencementEntry entry with
    | true -> false
    | false ->
        let affectingClass = extractAffectingdClass entry 
        match affectingClass with
        | "WelshNationalAssemblyAct" -> true
        | "ScottishAct" -> true
        | "ScottishStatutoryInstrument" -> true    
        | "UnitedKingdomChurchMeasure" -> true
        | "UnitedKingdomLocalAct" -> true
        | "UnitedKingdomPublicGeneralAct" -> true    
        | "UnitedKingdomStatutoryInstrument" -> true
        | "WelshStatutoryInstrument" -> true
        | "WelshParliamentAct" -> true   
        | _ -> false

let chronTableCommencementFilter (entry: LegFeed.Entry): bool = 
    match isCommencementEntry entry with
    | false -> false
    | true ->
        let affectingClass = extractAffectingdClass entry
        match affectingClass with
        | "WelshNationalAssemblyAct" -> true
        | "ScottishAct" -> true
        | "ScottishStatutoryInstrument" -> true    
        | "UnitedKingdomChurchMeasure" -> true
        | "UnitedKingdomLocalAct" -> true
        | "UnitedKingdomPublicGeneralAct" -> true    
        | "UnitedKingdomStatutoryInstrument" -> true
        | "WelshStatutoryInstrument" -> true
        | "WelshParliamentAct" -> true   
        | _ -> false

let tableVIFilter (entry: LegFeed.Entry): bool =
        match isCommencementEntry entry with
        | true -> false
        | false ->
                let affectingClass = extractAffectingdClass entry
                match affectingClass with
                | "WelshNationalAssemblyAct" -> true
                | "ScottishAct" -> true
                | "NorthernIrelandOrderInCouncil" -> true
                // probable bug introduced when isCommencementEntry added, ignore for time being
                //| "NorthernIrelandStatutoryRule" && e.Content.Effect.AffectingClass = "UnitedKingdomLocalAct" -> true
                | "ScottishStatutoryInstrument" -> true
                | "UnitedKingdomStatutoryInstrument" -> true
                | "UnitedKingdomChurchMeasure" -> true
                | "UnitedKingdomChurchInstrument" -> true
                | "UnitedKingdomLocalAct" -> true
                | "UnitedKingdomPublicGeneralAct" -> true
                | "WelshStatutoryInstrument" -> true
                | "WelshParliamentAct" -> true 
                | _ -> false

let tableVICommencementFilter (entry: LegFeed.Entry): bool =
    match isCommencementEntry entry with
        | false -> false
        | true ->
                match (extractAffectingdClass entry) with
                | "WelshNationalAssemblyAct" -> true
                | "ScottishAct" -> true
                | "NorthernIrelandOrderInCouncil" -> true
                // probable bug introduced when isCommencementEntry added, ignore for time being
                //| e when e.Content.Effect.AffectingClass = "NorthernIrelandStatutoryRule" && e.Content.Effect.AffectingClass = "UnitedKingdomLocalAct" -> true
                | "ScottishStatutoryInstrument" -> true
                | "UnitedKingdomStatutoryInstrument" -> true
                | "UnitedKingdomChurchMeasure" -> true
                | "UnitedKingdomChurchInstrument" -> true
                | "UnitedKingdomLocalAct" -> true
                | "UnitedKingdomPublicGeneralAct" -> true
                | "WelshStatutoryInstrument" -> true
                | "WelshParliamentAct" -> true 
                | _ -> false

let aspavFilter (entry: LegFeed.Entry): bool =
    match isCommencementEntry entry with
    | true -> false
    | false ->
        match (extractAffectingdClass entry) with
        | "ScottishAct" -> true
        |  "ScottishStatutoryInstrument" -> true
        |  "UnitedKingdomLocalAct" -> true
        |  "UnitedKingdomPublicGeneralAct" -> true
        |  "UnitedKingdomStatutoryInstrument" -> true
        | _ -> false

let aspavFilterCommencement (entry: LegFeed.Entry): bool =
    match isCommencementEntry entry with
    | false -> false
    | true ->
        match (extractAffectingdClass entry) with
        | "ScottishAct" -> true
        | "ScottishStatutoryInstrument" -> true
        | "UnitedKingdomLocalAct" -> true
        | "UnitedKingdomPublicGeneralAct" -> true
        | "UnitedKingdomStatutoryInstrument" -> true
        | _ -> false

let scotsSiBvFilter (entry: LegFeed.Entry): bool =
    match isCommencementEntry entry with
    | true -> false
    | false ->
        match (extractAffectingdClass entry) with
        | "ScottishAct" -> true
        | "ScottishStatutoryInstrument" -> true
        | "UnitedKingdomLocalAct" -> true
        | "UnitedKingdomPublicGeneralAct" -> true
        | "UnitedKingdomStatutoryInstrument" -> true
        | _ -> false
   
let scotsSiBvFilterCommencement (entry: LegFeed.Entry): bool =
    match isCommencementEntry entry with
    | false -> false
    | true ->
        match (extractAffectingdClass entry) with
        | "ScottishAct" -> true
        | "ScottishStatutoryInstrument" -> true
        | "UnitedKingdomLocalAct" -> true
        | "UnitedKingdomPublicGeneralAct" -> true
        | "UnitedKingdomStatutoryInstrument" -> true
        | _ -> false

let anawAvFilter (entry: LegFeed.Entry): bool =
    match isCommencementEntry entry with
    | true -> false
    | false ->
        match (extractAffectingdClass entry) with
        | "WelshNationalAssemblyAct" -> true
        | "UnitedKingdomImpactAssessment" -> true
        | "UnitedKingdomPublicGeneralAct" -> true
        | "UnitedKingdomStatutoryInstrument" -> true
        | "WelshStatutoryInstrument" -> true
        | "WelshAssemblyMeasure" -> true
        | "WelshParliamentAct" -> true  
        | _ -> false

let anawAvFilterCommencement (entry: LegFeed.Entry): bool =
    match isCommencementEntry entry with
    | false -> false
    | true ->
        match (extractAffectingdClass entry) with
        | "WelshNationalAssemblyAct" -> true
        | "UnitedKingdomImpactAssessment" -> true
        | "UnitedKingdomPublicGeneralAct" -> true
        | "UnitedKingdomStatutoryInstrument" -> true
        | "WelshStatutoryInstrument" -> true
        | "WelshAssemblyMeasure" -> true
        | "WelshParliamentAct" -> true  
        | _ -> false

let welshSiBvFilter (entry: LegFeed.Entry): bool =
    match isCommencementEntry entry with
    | true -> false
    | false ->
        match (extractAffectingdClass entry) with
        |  "WelshNationalAssemblyAct" -> true
        |  "UnitedKingdomImpactAssessment" -> true
        |  "UnitedKingdomPublicGeneralAct" -> true
        |  "UnitedKingdomStatutoryInstrument" -> true
        |  "WelshStatutoryInstrument" -> true
        |  "WelshAssemblyMeasure" -> true
        |  "WelshParliamentAct" -> true 
        | _ -> false

let welshSiBvCommencementFilter (entry: LegFeed.Entry): bool =
    match isCommencementEntry entry with
    | false -> false
    | true ->    
        match (extractAffectingdClass entry) with
        |  "WelshNationalAssemblyAct" -> true
        |  "UnitedKingdomImpactAssessment" -> true
        |  "UnitedKingdomPublicGeneralAct" -> true
        |  "UnitedKingdomStatutoryInstrument" -> true
        |  "WelshStatutoryInstrument" -> true
        |  "WelshAssemblyMeasure" -> true
        |  "WelshParliamentAct" -> true 
        | _ -> false


        
