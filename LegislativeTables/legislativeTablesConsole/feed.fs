module LegislativeFeed

open FSharp.Data
open Outputtypes
open FsHtml
open System
open System.Threading
open Legislative.Types
open Legislative.Filters
open Humanizer



let yearPrefix (entry:LegFeed.Entry) = 
    match (extractAffectedClass entry) with
    | "ScottishStatutoryInstrument" -> "SSI"
    | "UnitedKingdomStatutoryInstrument" -> "SI"
    | "WelshStatutoryInstrument" -> "WSI"
    | "UnitedKingdomChurchMeasure" -> "GSM"
    | _ -> ""

let shortAffectedClass  (entry:LegFeed.Entry) =
    match (extractAffectedClass entry) with
    | "EnglandAct" -> "aep"    
    | "GreatBritainAct" -> "apgb"
    | "IrelandAct" -> "aip"
    | "NorthernIrelandAct" -> "nia"
    | "NorthernIrelandAssemblyMeasure" -> "mnia"
    | "NorthernIrelandParliamentAct" -> "apni"
    | "NorthernIrelandOrderInCouncil" -> "nisi"    
    | "NorthernIrelandDraftOrderInCouncil" -> ""
    | "NorthernIrelandStatutoryRule" -> "nisr"
    | "NorthernIrelandDraftStatutoryRule" -> "nidsr"
    | "ScottishAct" -> "asp"  
    | "ScottishOldAct" -> "aosp"  
    | "ScottishStatutoryInstrument" -> "ssi"
    | "ScottishDraftStatutoryInstrument" -> "sdsi"
    | "UnitedKingdomChurchInstrument" -> "ukci"
    | "UnitedKingdomChurchMeasure" -> "ukcm"
    | "UnitedKingdomPrivateAct" -> ""
    | "UnitedKingdomPublicGeneralAct" -> "ukpga"    
    | "UnitedKingdomLocalAct" -> "ukla" 
    | "UnitedKingdomMinisterialOrder" -> "ukmo" 
    | "UnitedKingdomStatutoryInstrument" -> "uksi"
    | "UnitedKingdomDraftStatutoryInstrument" -> "ukdsi"
    | "UnitedKingdomImpactAssessment" -> "ukia" 
    | "WelshAssemblyMeasure" -> "mwa" 
    | "WelshNationalAssemblyAct" -> "anaw"
    | "WelshParliamentAct" -> "asc"
    | "WelshStatutoryInstrument" -> "wsi"
    | "WelshDraftStatutoryInstrument" -> "wdsi"
    | "UnitedKingdomStatutoryRuleOrOrder" -> "uksro"
    | _ -> "not matched"


let getNumberForLegislationAffected (entry:LegFeed.Entry) =
    match (extractAffectedClass entry) with
    | "UnitedKingdomPublicGeneralAct"
    | "GreatBritainAct"
    | "EnglandAct"
    | "IrelandAct"
    | "NorthernIrelandAct"
    | "ScottishOldAct" -> sprintf "c %i" (extractAffectedNumber entry)
    | "UnitedKingdomLocalAct"
    | "UnitedKingdomLocalActRevised" -> sprintf "c.%s" ((extractAffectedNumber entry).ToRoman().ToLower())
    | "ScottishAct" ->
                    if ((extractAffectedYear entry) < 1800) then
                         sprintf "c. %i" (extractAffectedNumber entry)
                    else 
                         sprintf "asp %i" (extractAffectedNumber entry)
    | "NorthernIrelandParliamentAct"
    | "NorthernIrelandAssemblyMeasure" -> sprintf "Chapter %i" (extractAffectedNumber entry)
    | "WelshAssemblyMeasure" -> sprintf "nawm %i" (extractAffectedNumber entry)
    | "WelshParliamentAct" -> sprintf "asc %i" (extractAffectedNumber entry)
    | "WelshStatutoryInstrument" -> sprintf "WSI %i" (extractAffectedNumber entry)
    | "UnitedKingdomStatutoryInstrument" -> sprintf "SI %i" (extractAffectedNumber entry)
    | _ -> sprintf "No. %i" (extractAffectedNumber entry)

let getNumberForLegislationAffecting (entry:LegFeed.Entry) =
    match (extractAffectingdClass entry) with
    | "UnitedKingdomPublicGeneralAct"
    | "GreatBritainAct"
    | "EnglandAct"
    | "IrelandAct"
    | "NorthernIrelandAct"
    | "ScottishOldAct" -> sprintf "c %i" (extractAffectingNumber entry)
    | "UnitedKingdomLocalAct"
    | "UnitedKingdomLocalActRevised" -> sprintf "c.%s" ((extractAffectingNumber entry).ToRoman().ToLower())
    | "ScottishAct" ->
                    if ((extractAffectedYear entry) < 1800) then
                         sprintf "c. %i" (extractAffectingNumber entry) 
                    else 
                         sprintf "asp %i" (extractAffectingNumber entry)
    | "NorthernIrelandParliamentAct"
    | "NorthernIrelandAssemblyMeasure" -> sprintf "Chapter %i" (extractAffectingNumber entry)
    | "WelshAssemblyMeasure" -> sprintf "nawm %i" (extractAffectingNumber entry)
    | "WelshNationalAssemblyAct" -> sprintf "anaw %i" (extractAffectingNumber entry)
    | "WelshParliamentAct" -> sprintf "asc %i" (extractAffectingNumber entry)
    | "UnitedKingdomChurchMeasure" -> sprintf "gsm %i" (extractAffectingNumber entry)
    | "NorthernIrelandStatutoryRule" -> sprintf "/%i" (extractAffectingNumber entry)
    | "UnitedKingdomChurchInstrument" -> sprintf "/%i" (extractAffectingNumber entry)
    | _ -> sprintf "No. %i" (extractAffectingNumber entry)

let resultsAffectedTitle (entry:LegFeed.Entry) = 
       
    let titles = extractAffectedTitles entry

    match titles.Length with
    | 0 -> "No Affected Title in feed for entry"
    | _ -> 
            match (extractAffectedClass entry) with
            | "UnitedKingdomChurchMeasure" ->
                if ((extractAffectedYear entry) < 1970)
                    then sprintf "CAM %s" titles.[0]
                    else sprintf "GSM %s" titles.[0]
            | _ -> titles.[0]

let getEffectForLegislation (entry:LegFeed.Entry) =
    extractEffectType entry
    // match effect.Type with
    // | t when t.Contains("repealed") -> t.Replace("repealed", "rep")
    // | t when t.Contains("amended") -> t.Replace("amended", "am")
    // | t when t.Contains("amend") -> t.Replace("amend", "am")
    // | t when t.Contains("applied") -> t.Replace("applied", "appl")
    // | t when t.Contains("appointed day(s)") -> t.Replace("appointed day(s)", "appt day(s)")
    // | t when t.Contains("substituted") -> t.Replace("substituted", "subst")
    // | t when t.Contains("modified") -> t.Replace("modified", "mod")
    // | t when t.Contains("with modifications") -> t.Replace("with modifications", "(mods)")
    // | t when t.Contains("prospectively") -> t.Replace("amended", "(prosp)")
    // | _ -> effect.Type
    

let resultsChangedProvision  (entry:LegFeed.Entry) =
    // same for SIBoundVolumes and ChronTables so far
    // <xsl:apply-templates select="ukm:Effect/ukm:AffectedProvisions"/>
    try
        try
            let prov = extractAffectedProvisions entry  // this needs to select text in section as well
            // TODO needs to include this as well - entry.Content.Effect.AffectedProvisions2.Sections ?
            // <xsl:apply-templates select="ukm:Effect" mode="resultsEffect"/>
            let sections = (extractAffectedProvisionsSections entry)
                            //|> Array.map (fun s -> s.String.Value)
                            |> String.concat " "
            let effect = getEffectForLegislation entry
            sprintf "%s %s" prov effect
        with
            | :? System.Exception as e ->
                sprintf "Exception in resultsChangedProvision - entry: %s" entry.Id
    finally
        sprintf ""
        


let resultsAffectingYearNumber (entry:LegFeed.Entry) =
    let number = getNumberForLegislationAffecting entry
    match (extractAffectingdClass entry) with
    | "UnitedKingdomChurchInstrument"
    | "NorthernIrelandStatutoryRule" -> sprintf "%i%s" (extractAffectingYear entry) number
    | _ -> sprintf "%i %s" (extractAffectingYear entry) number

let resultsAffectingYearNumberSI (entry:LegFeed.Entry) =
    sprintf "%i/%i" (extractAffectingYear entry) (extractAffectingNumber entry)

let resultsAffectingProvision (entry:LegFeed.Entry) = 
    // TODO some translate    
    extractAffectingProvisions entry

let trimAffectingTitle (title:string) (year:int) = 
    let parts = title.Split("Act")        
    parts.[0].Replace((year.ToString()),"").TrimEnd()

let resultsAffectingTitle (entry:LegFeed.Entry) =
    let numberSI = resultsAffectingYearNumberSI entry
    let number = resultsAffectingYearNumber entry
    let provision = resultsAffectingProvision entry
    match (extractAffectingdClass entry) with
    | "ScottishStatutoryInstrument" -> sprintf "SSI %s, %s" numberSI provision
    | "UnitedKingdomStatutoryInstrument" -> sprintf "SI %s, %s" numberSI provision
    | "WelshStatutoryInstrument" -> sprintf "WSI %s, %s" numberSI provision
    | "WelshAssemblyMeasure" ->
                            match ((extractAffectingTitles entry)) |> Seq.exists (fun s -> s.Contains "Wales") with
                            | true ->
                                        let t = (extractAffectingTitles entry).[0] 
                                        let foo = t.Split([|"Wales"|],System.StringSplitOptions.None)
                                        sprintf "%sW) %s, %s" foo.[0] number  provision
                            | false -> 
                                        sprintf "%s, %s" number provision 
    | "UnitedKingdomChurchMeasure" ->
                            match (extractAffectingTitles entry) with
                            | titles when (extractAffectingTitles entry) |> Seq.exists (fun s -> s.Contains "Amendment") ->
                                    let t = titles.[0] 
                                    let foo = t.Split([|"Amendment"|],System.StringSplitOptions.None)
                                    sprintf "%sAmdt) %s, %s" (foo.[0].TrimEnd()) number  provision
                            | titles when (extractAffectingTitles entry) |> Seq.exists (fun s -> s.Contains "Measure") ->
                                    let t = titles.[0] 
                                    let foo = t.Split([|"Measure"|],System.StringSplitOptions.None)
                                    sprintf "%s, %s, %s" (foo.[0].TrimEnd()) number  provision
                            | titles -> sprintf "%s, %s" number provision

    | "ScottishAct" ->
                    // If title contains "(Scotland)" replace with "(S)"" 
                    // only take the string upto the "Act ..."
                    match (extractAffectingTitles entry) |> Seq.exists (fun s -> s.Contains "Scotland") with
                    | true ->
                                let t = (extractAffectingTitles entry).[0]
                                let replacedScotland = t.Replace("(Scotland)", "(S)") 
                                let splitOn = replacedScotland.Split([|" Act"|],System.StringSplitOptions.None)
                                sprintf "%s, %s, %s" (splitOn.[0].TrimEnd()) number  provision
                    | false -> 
                                let t = (extractAffectingTitles entry).[0]
                                let splitOn = t.Split([|" Act"|],System.StringSplitOptions.None)
                                sprintf "%s, %s, %s" (splitOn.[0].TrimEnd()) number provision
    | "NorthernIrelandOrderInCouncil" -> sprintf "NISI %s, %s" numberSI provision
    | "NorthernIrelandStatutoryRule" -> sprintf "%s, SR %s, %s" (trimAffectingTitle ((extractAffectingTitles entry).[0].Replace("Northern Ireland","NI")) (extractAffectingYear entry) ) number provision
    | "UnitedKingdomChurchInstrument" -> sprintf "Archbishops' Instrument, %s, %s"  number provision
    | _ -> sprintf "%s, %s, %s" (trimAffectingTitle (extractAffectingTitles entry).[0] (extractAffectingYear entry) ) number provision

let buildRegnalYear  (entry:LegFeed.Entry) =    
    let shortAffectedClass = shortAffectedClass entry
    let foo = (extractAffectedUri entry).Replace(( sprintf "http://www.legislation.gov.uk/id/%s/" shortAffectedClass), "")
    let possYear = foo.Substring(0,4)
    let result = System.Int32.TryParse possYear
    match result with
    | true,_ -> None
    | false,_ ->
                 let lastSlash = foo.LastIndexOf('/')
                 let bar = foo.Substring(0,lastSlash)
                 let parts = bar.Split('/')
                 let reg = parts |> Seq.head
                 let duoregex = new System.Text.RegularExpressions.Regex "^([A-Z][a-z]+)([0-9])(and)(1)([A-Z][a-z]+)([0-9])?$"
                 let duoMatch = duoregex.Match reg
                 let test = match duoMatch.Success with
                            | true -> sprintf "%s. %s & %s %s. %s" duoMatch.Groups.[1].Value duoMatch.Groups.[2].Value duoMatch.Groups.[4].Value duoMatch.Groups.[5].Value duoMatch.Groups.[6].Value
                            | false -> 
                                        let singleRegex = new System.Text.RegularExpressions.Regex "^([A-Z][a-z]+)([0-9])$"
                                        let singleMatch = singleRegex.Match reg
                                        match singleMatch.Success with
                                        | true -> sprintf "%s. %s"  singleMatch.Groups.[1].Value singleMatch.Groups.[2].Value
                                        | false -> sprintf "%s." reg

                 let nums = parts |> Seq.tail |> Seq.head 
                 Some (sprintf "(%s %s)" (nums.Replace("-"," & ")) test)

let convertEntry (entry:LegFeed.Entry) =    
    {effectId = (EffectId (extractEffectId entry));
     legislativeYear= (LegislativeYear ((extractAffectedYear entry).ToString()));
      shortTitle= (ShortTitle (resultsAffectedTitle entry));
       affectedProvision=( AffectedProvision (resultsChangedProvision entry));
        affectingTitle= (AffectingTitle (resultsAffectingTitle entry));
        numberOfMeasureAffected= getNumberForLegislationAffected entry;
        numberOfMeasureAffecting = getNumberForLegislationAffecting entry;
        regnalYear = buildRegnalYear entry}

let selectFilter = function
| ChronTable -> chronTableFilter
| ChronTableCommencement -> chronTableCommencementFilter
| SIBoundVolumes -> siBoundVolumesFilter
| SIBoundVolumesCommencement -> siBoundVolumesCommencementFilter
| TableVI -> tableVIFilter
| TableVICommencement -> tableVICommencementFilter
| AspAnnualVolume -> aspavFilter
| AspAnnualVolumeCommencement -> aspavFilterCommencement
| ScotsSIBoundVolumes -> scotsSiBvFilter
| ScotsSIBoundVolumesCommencement -> scotsSiBvFilterCommencement
| AnawAnnualVolume -> anawAvFilter
| AnawAnnualVolumeCommencement -> anawAvFilterCommencement
| WelshSIBoundVolumes -> welshSiBvFilter
| WelshSIBoundVolumesCommencement -> welshSiBvCommencementFilter


let convertFeedToHtml (documentType: DocumentType) (entriesInput:LegFeed.Entry [])  =
    
    let groupedEntries = entriesInput 
                            |> Seq.filter (selectFilter documentType)
                            |> Seq.map convertEntry
                            |> Seq.groupBy (fun o -> o.legislativeYear) 
                            |> Seq.toList


    let outputFirstSectionRow (entry:OutputRow) =
        let (ShortTitle title) = entry.shortTitle
        let (AffectedProvision howAffected) = entry.affectedProvision
        let (AffectingTitle affectingTitle) = entry.affectingTitle
        let (EffectId effectId) = entry.effectId
        tr [
             td (["class" %= "number"] @ %entry.numberOfMeasureAffected )
             td (["class" %= "title"] @ %title )
             td (["class" %= "AffectedProvisions"] @ %howAffected)
             td (["class" %= "resultsAffectingTitle"] @ %affectingTitle)
             td (["class" %= "resultsEffectId"] @ %effectId)
           ]        
                            

    let outputSectionRow (entries:OutputRow list) = 
        entries 
        |> List.map (fun e -> 
                                let (AffectedProvision howAffected) = e.affectedProvision
                                let (AffectingTitle affectingTitle) = e.affectingTitle
                                let (EffectId effectId) = e.effectId
                                tr [
                                     td ["class" %= "number"]
                                     td ["class" %= "title"]
                                     td (["class" %= "AffectedProvisions"] @ %howAffected)
                                     td (["class" %= "resultsAffectingTitle"] @ %affectingTitle)
                                     td (["class" %= "resultsEffectId"] @ %effectId)
                                   ]        
                            )

    let outputSectionRowFull year (entries:OutputRow list) = 
        
        entries 
        |> List.map (fun e -> 
                                let (ShortTitle title) = e.shortTitle
                                let yrAndChap = sprintf "%s %s" year e.numberOfMeasureAffected
                                let (AffectedProvision howAffected) = e.affectedProvision
                                let (AffectingTitle affectingTitle) = e.affectingTitle
                                let (EffectId effectId) = e.effectId
                                tr [
                                     td (["class" %= "number"] @ %yrAndChap )
                                     td (["class" %= "title"] @ %title )
                                     td (["class" %= "AffectedProvisions"] @ %howAffected)
                                     td (["class" %= "resultsAffectingTitle"] @ %affectingTitle)
                                     td (["class" %= "resultsEffectId"] @ %effectId)
                                ]                                   
                            )


    let ouputRegnalYearRow (year) documentType =
        match documentType with
        | ChronTable
        | ChronTableCommencement 
        | TableVI
        | TableVICommencement
        | SIBoundVolumesCommencement
        | SIBoundVolumes 
        | AspAnnualVolume
        | AspAnnualVolumeCommencement
        | ScotsSIBoundVolumes
        | ScotsSIBoundVolumesCommencement
        | AnawAnnualVolume
        | AnawAnnualVolumeCommencement
        | WelshSIBoundVolumes
        | WelshSIBoundVolumesCommencement ->
                            Console.WriteLine (year.ToString())
                            tr [
                                td (["class" %= "regnal"] @ %year)
                                td ["class" %= "title"]
                                td ["class" %= "AffectedProvisions"]
                                td ["class" %= "resultsAffectingTitle"]
                                td ["class" %= "resultsEffectId"]
                            ] 

    let outputRegnalString outputRow =
        match outputRow.regnalYear with
        | Some s ->        
                    Console.WriteLine (outputRow.regnalYear.ToString())
                    Some( tr[
                         td (["class" %= "regnal"] @ %s)
                         td ["class" %= "title"]
                         td ["class" %= "AffectedProvisions"]
                         td ["class" %= "resultsAffectingTitle"]
                         td ["class" %= "resultsEffectId"]
                    ])
        | None -> None

    let sections year (rows:seq<OutputRow>) :  (Html list) = 
        let sections = rows |> Seq.groupBy (fun r -> r.numberOfMeasureAffected) |> Seq.toList
        sections  |> Seq.fold (fun acc rowgrp -> (
                                                    let (_ , rows) = rowgrp
                                                    match documentType with
                                                    | ChronTable
                                                    | ChronTableCommencement 
                                                    | TableVI
                                                    | TableVICommencement
                                                    | SIBoundVolumes
                                                    | SIBoundVolumesCommencement 
                                                    | AspAnnualVolume
                                                    | AspAnnualVolumeCommencement
                                                    | ScotsSIBoundVolumes
                                                    | ScotsSIBoundVolumesCommencement
                                                    | AnawAnnualVolume
                                                    | AnawAnnualVolumeCommencement
                                                    | WelshSIBoundVolumes
                                                    | WelshSIBoundVolumesCommencement -> 
                                                        //let r = (rows |> Seq.head |> outputRegnalString )
                                                        let f = rows |> Seq.head |> outputFirstSectionRow
                                                        let t = rows |> Seq.tail |> Seq.toList |> outputSectionRow
                                                        acc @ [f] @t
                                                    
                                                )
                              ) []

    let rows (entries:((LegislativeYear * seq<OutputRow>) list)): (Html list) =
        entries |> Seq.fold (fun acc entry -> (
                                                let ((LegislativeYear year), rows) = entry
                                                let regnal = ouputRegnalYearRow year
                                                let regnalString = rows |> Seq.head |> outputRegnalString 
                                                let sections = sections year rows
                                                //printfn "sections lenght: %i" sections.Length
                                                match regnalString with
                                                | Some r -> acc @ [regnal documentType] @ [r] @ sections
                                                | None-> acc @ [regnal documentType] @ sections  
                                              )
                            ) []
    
    rows groupedEntries



let startTags = """
<html>

<head>
    <style type="text/css">
        table {
            font-family: Times New Roman;
            font-size: 9pt;
            border-width: 1px;
            border-color: #666666;
            border-collapse: collapse;
            background-color: #ffffff;
        }

        table th {
            border-width: 1px;
            padding: 8px;
            border-style: solid;
            border-color: #666666;
            background-color: #ffffff;
        }

        table td.last {
            border-width: 0px 1px 1px 1px;
            padding: 8px;
            border-style: solid;
            border-color: #666666;
            background-color: #ffffff;
        }

        table td {
            border-width: 0px 1px 0px 1px;
            padding: 8px;
            border-style: solid;
            border-color: #666666;
            background-color: #ffffff;
        }
    </style>
    <meta>
    <title>Generated by Nagaraj for TSO</title>
    </meta>
</head>
<table>
    <thead>
        <th>Year and Chapter of Act or No of Measure</th>
        <th>Short title</th>
        <th>How Affected</th>
        <th>2016 Chapter of Act or No of Measure or SI or No of asp or SSI</th>
        <th class=~EffectId~>Effect Id</th>
    </thead>
"""

let endTags = """</table></html>"""