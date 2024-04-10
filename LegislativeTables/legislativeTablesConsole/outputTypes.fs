module Outputtypes

type EffectId = EffectId of string
type AffectingTitle = AffectingTitle of string
type AffectedProvision = AffectedProvision of string
type LegislativeYear = LegislativeYear of string
type ShortTitle = ShortTitle of string

type OutputRow = {
                   numberOfMeasureAffected: string 
                   numberOfMeasureAffecting: string
                   legislativeYear:LegislativeYear
                   shortTitle: ShortTitle
                   affectedProvision:AffectedProvision  
                   affectingTitle:AffectingTitle
                   effectId:EffectId
                   regnalYear: string option
                 }


