namespace CWTools.Process.Scopes

open System
open CWTools.Common
open CWTools.Process.Scopes
open CWTools.Common.NewScope
module STL =
    open CWTools.Common.STLConstants
    open CWTools.Utilities.Utils
    open Microsoft.FSharp.Collections.Tagged

    let defaultDesc = "Scope (/context) switch"
    let scopedEffects() = [
//        ScopedEffect("space_owner", [scopeManager.ParseScope() "Ship"; scopeManager.ParseScope() "Fleet"; scopeManager.ParseScope() "GalacticObject"; scopeManager.ParseScope() "Planet"], scopeManager.ParseScope() "Country", EffectType.Link, defaultDesc, "", true);
    ]
    let effectInnerScopes() =[
        //These are from blackninja9939
  //      "any_ship",  scopeManager.ParseScope() "Ship";
  //      "every_ship",  scopeManager.ParseScope() "Ship";
  //      "random_ship",  scopeManager.ParseScope() "Ship";
        ]
    let effectInnerScopeFunctions() = [
//        "any_neighbor_system", (Some (scopeManager.ParseScope() "GalacticObject")), ["ignore_hyperlanes"];
    ]

    let addInnerScope (des : DocEffect list) =
        let withSimple =
             des |> List.map (fun de ->
                match effectInnerScopes() |> List.tryPick (function | (n, t) when n = de.Name -> Some (t) |_ -> None) with
                | Some t -> ScopedEffect(de, Some t, true, [], false, false) :> DocEffect
                | None -> de)
        withSimple |> List.map (fun de ->
                match effectInnerScopeFunctions() |> List.tryPick (function | (n, t, s) when n = de.Name -> Some (t, s) |_ -> None) with
                | Some (t, s) -> ScopedEffect(de, t, false, s, false, false) :> DocEffect
                | None -> de)




    let oneToOneScopes =
        let from i = fun ((s), change) -> {s with Scopes = (s.GetFrom i)::s.Scopes}, (false, true)
        let prev = fun ((s), change) -> {s with Scopes = s.PopScope}, (false, true)
        [
        "THIS", id;
        "ROOT", fun ((s), change) -> {s with Scopes = s.Root::s.Scopes}, (false, true);
        "FROM", from 1;
        "FROMFROM", from 2;
        "FROMFROMFROM", from 3;
        "FROMFROMFROMFROM", from 4;
        "PREV", prev;
        "PREVPREV", prev >> prev;
        "PREVPREVPREV", prev >> prev >> prev;
        "PREVPREVPREVPREV", prev >> prev >> prev >> prev
    ]
    let oneToOneScopesNames = List.map fst oneToOneScopes
    type EffectMap = Map<string, Effect, InsensitiveStringComparer>
    let changeScope = Scopes.createChangeScope oneToOneScopes (Scopes.simpleVarPrefixFun "var:") true

    let sourceScope (effects : Effect list) (key : string) =
        let key = if key.StartsWith("hidden:", StringComparison.OrdinalIgnoreCase) then key.Substring(7) else key
        let keys = key.Split('.') |> List.ofArray
        let inner (nextKey : string) =
            let onetoone = oneToOneScopes |> List.tryFind (fun (k, _) -> k == nextKey)
            match onetoone with
            | Some (_) -> None
            | None ->
                let effect = (effects)
                            |> List.choose (function | :? ScopedEffect as e -> Some e |_ -> None)
                            |> List.tryFind (fun e -> e.Name == nextKey)
                match effect with
                |None -> None
                |Some e -> Some e.Scopes
        keys |> List.fold (fun acc k -> match acc with |Some e -> Some e |None -> inner k) None |> Option.defaultValue scopeManager.AllScopes

