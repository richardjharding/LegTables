module Legislative.Types

open FSharp.Data

type LegFeed = XmlProvider<"""Section_ukpga_1969_1992_01.feed""">


type LegConfig = XmlProvider<"""./config/settings.xml""">

type DocumentType =
    | ChronTable
    | ChronTableCommencement
    | SIBoundVolumes
    | SIBoundVolumesCommencement
    | TableVI
    | TableVICommencement
    | AspAnnualVolume
    | AspAnnualVolumeCommencement
    | ScotsSIBoundVolumes
    | ScotsSIBoundVolumesCommencement
    | AnawAnnualVolume
    | AnawAnnualVolumeCommencement
    | WelshSIBoundVolumes
    | WelshSIBoundVolumesCommencement