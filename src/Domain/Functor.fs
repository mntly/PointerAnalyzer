module PointerAnalyzer.AbsDom.Functor

open System.Collections.Generic
open PointerAnalyzer.AbsDom.Signature

exception NotSingletonException

type SetDomain<'a when 'a: comparison> (elem: Elem<'a>) =
  inherit AbstractDomain<Set<'a>> ()

  override _.bot = Set.empty

  override _.leq x y = Set.isSubset x y

  override _.join x y = Set.union x y

  override _.toString x =
    x
    |> Set.toList
    |> List.map elem.toString
    |> String.concat ", "
    |> sprintf "{%s}"

  override _.isBot x = Set.isEmpty x

  member _.make xs = Set.ofList xs

  member _.add x xs = Set.add x xs

  member _.filter pred xs = Set.filter pred xs

  member _.map mapper xs = Set.map mapper xs

  member _.choose chooser xs =
    Set.toSeq xs |> Seq.choose chooser |> Set.ofSeq

  member _.count xs = Set.count xs

  member _.getSingleton xs =
    if Set.count xs <> 1 then
      raise NotSingletonException
    else
      Set.minElement xs

type Prod2Domain<'a, 'b> (aDom: AbstractDomain<'a>, bDom: AbstractDomain<'b>) =
  inherit AbstractDomain<'a * 'b> ()

  override _.bot = aDom.bot, bDom.bot

  override _.leq x y =
    let ax, bx = x
    let ay, by = y
    aDom.leq ax ay && bDom.leq bx by

  override _.join x y =
    let ax, bx = x
    let ay, by = y
    aDom.join ax ay, bDom.join bx by

  override _.toString x =
    let a, b = x
    sprintf "<%s, %s>" (aDom.toString a) (bDom.toString b)

  override _.isBot x =
    let a, b = x
    aDom.isBot a && bDom.isBot b

type MapDomain<'k, 'v when 'k: comparison>
  (keyElem: Elem<'k>, valueDom: AbstractDomain<'v>) =
  inherit AbstractDomain<Map<'k, 'v>> ()

  override _.bot = Map.empty

  override _.leq x y =
    x
    |> Map.forall (fun key value ->
      match Map.tryFind key y with
      | Some other -> valueDom.leq value other
      | None -> false)

  override _.join x y =
    y
    |> Map.fold
      (fun acc key value ->
        match Map.tryFind key acc with
        | Some old -> Map.add key (valueDom.join old value) acc
        | None -> Map.add key value acc)
      x

  override _.toString x =
    x
    |> Map.toList
    |> List.map (fun (key, value) ->
      sprintf "%s -> %s" (keyElem.toString key) (valueDom.toString value))
    |> String.concat "\n"

  override _.isBot x = Map.isEmpty x

  member _.find key map =
    match Map.tryFind key map with
    | Some value -> value
    | None -> valueDom.bot

  member _.add key value map = Map.add key value map

  member _.contains key map = Map.containsKey key map
