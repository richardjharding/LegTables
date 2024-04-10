open System
open FSharp.Data
open System.IO
open LegislativeFeed
open Outputtypes
open FsHtml
open Argu
open ArgumentParser
open Legislative.Types
open System.Buffers.Text




let replacements (appSettings:LegConfig.ReplaceConfig) = 
    appSettings.ReplaceText.Adds
    |> Seq.map (fun a -> (a.MatchText, a.ReplaceText))

let performTextReplacements (htmlText:string) (replacements:seq<string * string>) =
    replacements
    |> Seq.fold (fun (state:string) replaceConfig -> (
                                                        let ((matchtext:string), (replace:string)) = replaceConfig
                                                        state.Replace(matchtext, replace)
    )) htmlText

let baseUrl appSettings = 
    let setting = Array.find (fun (a:LegConfig.Add) -> a.Key = "LegislationBaseURL") appSettings
    setting.Value.Value


let querySuffix appSettings = 
    let setting = Array.find (fun (a:LegConfig.Add) -> a.Key = "LegislationQueryString") appSettings
    setting.Value.Value

let tempFolder appSettings = 
    let output = Array.find (fun (a:LegConfig.Add) -> a.Key = "TempFolder") appSettings
    output.Value.Value

let documentType (appSettings:LegConfig.Configuration) = 
    let docTypeSetting = Array.find (fun (a:LegConfig.Add) -> a.Key = "DocumentType") appSettings.AppSettings.Adds
    match docTypeSetting.Value.Value with
    | "ChronTable" -> ChronTable
    | "ChronTableCommencement" -> ChronTableCommencement
    | "SIBoundVolumes" -> SIBoundVolumes
    | "SIBoundVolumesCommencement" -> SIBoundVolumesCommencement
    | "TableVI" -> TableVI
    | "TableVICommencement" -> TableVICommencement
    | "AspAnnualVolume" -> AspAnnualVolume
    | "AspAnnualVolumeCommencement" -> AspAnnualVolumeCommencement
    | "ScotsSIBoundVolumes" -> ScotsSIBoundVolumes
    | "ScotsSIBoundVolumesCommencement" -> ScotsSIBoundVolumesCommencement
    | "AnawAnnualVolume" -> AnawAnnualVolume
    | "AnawAnnualVolumeCommencement" -> AnawAnnualVolumeCommencement
    | "WelshSIBoundVolumes" -> WelshSIBoundVolumes
    | "WelshSIBoundVolumesCommencement" -> WelshSIBoundVolumesCommencement
    | _ -> failwith "unrecognised document type in settings"    

let buildAffecedYr startYear endYear =
    match startYear, endYear with
    | -1, -1 -> ""
    | -1 , _ -> ""  
    | start, -1 -> sprintf "%i-%i" start DateTime.Now.Year
    | start, endyr -> sprintf "%i-%i" start endyr
      

let generateFeedUrl appSettings (chron:LegConfig.Add3) affectingYearOverride = 
    
    
    let url = match chron.AffectedDocumentType with
              | "uksro" -> "https://editorial.legislation.gov.uk/changes/affected/"
              | _ -> baseUrl appSettings

    let query = querySuffix appSettings

    let affectedRange = buildAffecedYr chron.AffectedStartYear chron.AffectedEndYear

    let affectingYear affectingYearOverride =
        match affectingYearOverride with
        | Some yr -> yr.ToString()
        | None -> sprintf "%i" chron.AffectingYear 

    //let affectingYear = sprintf "%i" chron.AffectingYear
    // TODO make handling trainling slashes more robust
    sprintf "%s%s/%s/affecting/all/%s/%s" url chron.AffectedDocumentType affectedRange (affectingYear affectingYearOverride) query

let downloadFeeds appSettings affectingYear (chron:LegConfig.Add3) =
    let folder = tempFolder appSettings

    let filepath page =         
        let filePath = sprintf "%s_%02i.feed" (Path.GetFileNameWithoutExtension chron.OutputFileName) page
        Directory.CreateDirectory folder |> ignore
        System.IO.Path.Combine [|folder ; filePath|] 

    let feedUrl = generateFeedUrl appSettings chron affectingYear
    let url = sprintf "%s1" feedUrl    
    printfn "Fetching feed from : %s" url
    //let credentials = "richard@sprydondesigns.com:crowded guava coven steamy"
    let username = System.Environment.GetEnvironmentVariable "LEGFEEDUSERNAME"
    let password = System.Environment.GetEnvironmentVariable "LEGFEEDPASSWORD"
    let credentials = sprintf "%s:%s" username password
    let encoded = System.Convert.ToBase64String (System.Text.Encoding.UTF8.GetBytes credentials)
    let authHeader = Seq.singleton  ("Authorization", (sprintf "Basic %s" encoded))
    let feedXml = Http.RequestString (url, httpMethod = "GET", headers=authHeader)

    File.WriteAllText((filepath 1), feedXml)
    // check the leg:morePages = that is total pages - can then fetch remaining pages async
    
    let feed = LegFeed.Parse feedXml
    let morePages = feed.MorePages
    if(morePages > 1) then
        let range = [2..morePages]
        range |> List.iter (fun i -> (
                                        printf "%i " i
                                        let path = filepath i
                                        let url = sprintf "%s%i" feedUrl i
                                        printfn "Fetching feed from : %s" url
                                        let feedXml = Http.RequestString (url, httpMethod = "GET" , headers=authHeader)
                                        File.WriteAllText(path, feedXml)
                                        ))
        ()
    else
        ()
    //printfn "%s" feedXml
    
let loadAndProcessFeed (appSettings:LegConfig.Configuration) (chron:LegConfig.Add3) affectingYear (filePaths:string list) =
    let feeds = filePaths |> Seq.map (LegFeed.Load ) 

    let entries = feeds |> Seq.collect (fun f -> f.Entries)
    //printfn "entry count %i" (Seq.length (entries |> (Seq.filter (selectFilter (documentType appSettings)))))

    let trs = entries |> Seq.toArray |> (convertFeedToHtml (documentType appSettings) )      
    let c = trs |> Seq.toList      

    let output = tbody ( c )                      

    let stringReplaceConfigs  = replacements appSettings.ReplaceConfig
    let before = output |> Html.toString
    let after = performTextReplacements before stringReplaceConfigs 
    
    let path = sprintf "%s\\%s%s" (Path.GetDirectoryName chron.OutputFileName) (Path.GetFileNameWithoutExtension chron.OutputFileName) ".html" 
    printfn "Writing html output: %s" path
    let updatedStartTags =
        match affectingYear with
        | Some yr -> startTags.Replace ("<th>2016 Chapter of Act" ,(sprintf "<th>%i Chapter of Act" yr))
        | None -> startTags.Replace ("<th>2016 Chapter of Act" ,(sprintf "<th>%i Chapter of Act" chron.AffectingYear))
    
    System.IO.File.WriteAllText (path, (sprintf "%s%s%s" updatedStartTags after endTags))



let feedFileMask (chron:LegConfig.Add3) =
    Path.GetFileNameWithoutExtension chron.OutputFileName

let transformSection settings (appSettings:LegConfig.Configuration) affectingYear (chron:LegConfig.Add3)  =
    let files = System.IO.Directory.GetFiles (tempFolder settings)
    files 
    |> Seq.filter (fun f -> f.Contains (feedFileMask chron))
    |> Seq.sort
    |> Seq.toList
    |> loadAndProcessFeed appSettings chron affectingYear
    

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "LegislativeTables.exe")
    let results = parser.Parse argv    

    let generate (settingsPath:string) = 
        printfn "Starting chron generation......"
        let sample = LegConfig.Load(settingsPath)
        let chronConfigs = sample.ChronConfig.GenerateChrons  

        let affectingYear = 
            match results.TryGetResult AffectingYear with
            | Some yr -> 
                        printfn "Settng affecting year to %i" yr.Value
                        yr
            | None -> 
                printfn "Please enter an Affecting Year:"
                let y = int (Console.ReadLine())
                (Some y)

        match results.TryGetResult SkipFeedDownload  with
        | Some s -> ()
        | None -> 
            let folder = tempFolder sample.AppSettings.Adds
            printfn "Deleting previous feed files from %s" folder
            let di = System.IO.DirectoryInfo folder
            match di.Exists with
            | true -> di.EnumerateFiles() |> Seq.iter (fun fi -> fi.Delete())
            | false -> ()                        
            chronConfigs.Adds
            |> Array.iter (downloadFeeds sample.AppSettings.Adds affectingYear)    
        
        chronConfigs.Adds
        |> Array.iter (transformSection sample.AppSettings.Adds sample affectingYear)

        printfn "Completed chron generation......"
        0 // return an integer exit code

    
    let settingsFilePath = results.TryGetResult SettingsFile
    let docType = results.TryGetResult DocumentType
    match docType with
    | Some dt -> 
                printfn "Generating output for %s" (dt.ToString())                
                match settingsFilePath with
                | None -> 
                                // get default settings file for type
                                printfn "Using default settings for doc type"
                                match dt with
                                | ChronTable -> generate "./config/settingsChronTable.xml"
                                | ChronTableCommencement -> generate "./config/settingsChronCommencement.xml"
                                | SIBoundVolumes -> generate "./config/settingsSIBV.xml"
                                | SIBoundVolumesCommencement -> generate "./config/settingsSIBVCommencement.xml"
                                | TableVI -> generate "./config/settingsTableVI.xml"
                                | TableVICommencement -> generate "./config/settingsTableVICommencement.xml"
                                | AspAnnualVolume -> generate "./config/settingsAspAnnualVolume.xml"
                                | AspAnnualVolumeCommencement -> generate "./config/settingsAspAnnualVolumeCommencement.xml"
                                | ScotsSIBoundVolumes -> generate "./config/settingsScotsSIBV.xml"
                                | ScotsSIBoundVolumesCommencement -> generate "./config/settingsScotsSIBVCommencement.xml"
                                | AnawAnnualVolume -> generate "./config/settingsAnawAnnualVolume.xml"
                                | AnawAnnualVolumeCommencement -> generate "./config/settingsAnawAnnualVolumeCommencement.xml"
                                | WelshSIBoundVolumes -> generate "./config/settingsWelshSIBV.xml"
                                | WelshSIBoundVolumesCommencement -> generate "./config/settingsWelshSIBVCommencement.xml"
                                
                                
                | Some settingsPath -> 
                                printfn "Overriding default settings file with %s" settingsPath
                                generate settingsPath
                
    | None ->                 
                match settingsFilePath with
                | None -> 
                                printfn "%s" (parser.PrintUsage())
                                0
                | Some settingsPath -> 
                                printfn "No document type supplied, attempting to infer from settings file"
                                generate settingsPath
                                
