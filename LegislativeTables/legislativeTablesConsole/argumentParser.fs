module ArgumentParser

open Argu
open Legislative.Types


type CLIArguments = 
    | [<AltCommandLine("-dt")>] DocumentType of DocumentType
    | [<AltCommandLine("-s")>][<EqualsAssignment>] SettingsFile of path:string
    | [<AltCommandLine("-o")>][<EqualsAssignment>] OutputFolder of path:string
    | [<AltCommandLine("-y")>] AffectingYear of int option
    | [<AltCommandLine("-sd")>] SkipFeedDownload
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | SettingsFile _ -> "Path to the settings xml file to override the default settings file based on document type"
            | OutputFolder _ -> "Folder where output and temp files are written to"
            | SkipFeedDownload -> "skips downloading of feed files"
            | AffectingYear _ -> "overrides the affecting year set in the settings file"
            | DocumentType _ -> "Select the output document type"

