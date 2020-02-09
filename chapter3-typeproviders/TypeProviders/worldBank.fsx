#load "references.fsx"
open FSharp.Data

let wb = WorldBankData.GetDataContext()
let countries = wb.Countries

let pop2000 = [ for c in countries -> c.Indicators.``Population, total``.[2000]]
let pop2010 = [ for c in countries -> c.Indicators.``Population, total``.[2010]]

// TODO revist RProvider