module JobInformationExtraction

open System
open System.Globalization
open System.Text.RegularExpressions
open JobInformationDefinitions

let getDescriptionWords (description : string) : string list =
    let is_not_number (s : string) =
        match Double.TryParse(s,
                              Globalization.NumberStyles.Any,
                              CultureInfo.InvariantCulture)
              with
              | true, _ -> false
              | _       -> true
    let description_words =
        description.Split ' '
        |> Seq.map (fun x -> x.Trim())
        |> Seq.collect (fun x -> x.Split '/') // .NET/C#
        |> Seq.choose (fun x ->
                       let m = Regex("[\w#\+\.\-]+").Match x
                       if m.Success
                       then Some m.Value
                       else None)
        |> Seq.map (fun x -> x.TrimEnd '.')
        |> Seq.map (fun x -> x.Trim '-')
        |> Seq.filter ((<>)"")
        |> Seq.filter ((<>)"+")
        |> Seq.filter is_not_number
        |> Seq.map (fun x -> x.ToLower())
    description_words
    |> List.ofSeq

let getTags (keywords : KeyWord list) (description_words : string list) : string list =
    let non_composite_keywords =
        keywords
        |> Seq.choose (fun k ->
                      match k with
                      | Single k -> Some k
                      | _ -> None)
        |> Set.ofSeq
    let composite_keywords =
        keywords
        |> Seq.choose (fun k ->
                       match k with
                       | Composite(k1,k2) -> Some (k1,k2)
                       | _ -> None)
        |> Set.ofSeq
    let non_composite_tags =
        description_words
        |> Set.ofList
        |> Seq.filter (fun x -> Set.contains x non_composite_keywords)
        |> List.ofSeq
    let composite_tags =
        description_words
        |> Seq.windowed 2
        |> Seq.map (fun [|x;y|] -> x,y)
        |> Seq.filter (fun x -> Set.contains x composite_keywords)
        |> Seq.map (fun (x,y) -> sprintf "%s %s" x y)
        |> List.ofSeq
    List.concat [non_composite_tags; composite_tags]

let countWords (words : string seq) =
    words
    |> Seq.countBy id

let getRareWords (n : int) (word_count : Map<string,int>) (words : string seq) =
    words
    |> Seq.sortBy (fun w ->
                   match Map.tryFind w word_count with
                   | Some count -> count
                   | None -> 1)
    |> Seq.take n
