namespace CWTools.Validation.EU4
open CWTools.Common
open CWTools.Games
open CWTools.Validation.Rules
open System

module EU4Compute =

    let computeEU4Data (foldRules : unit -> FoldRules<_> option) (e : Entity) =
        // let eventIds = if e.entityType = EntityType.Events then e.entity.Children |> List.choose (function | :? Event as e -> Some e.ID |_ -> None) else []
        // let setvariables = STLValidation.getEntitySetVariables e
        // let setflags = findAllSetFlags e
        // let savedeventtargets = STLValidation.findAllSavedEventTargetsInEntity e
        let referencedtypes = (if foldRules().IsSome then Some ((foldRules().Value.GetReferencedTypes)(e)) else None)
        let definedvariable = (if foldRules().IsSome then Some ((foldRules().Value.GetDefinedVariables )(e)) else None)
        // let hastechs = getAllTechPrereqs e
        let scriptedeffectparams = Some (EU4Validation.getScriptedEffectParamsEntity e)

        EU4ComputedData(referencedtypes, definedvariable, scriptedeffectparams)
