module PointerAnalyzer.TypeInfer.TypeConstraintSolver

open Microsoft.Z3
open PointerAnalyzer.AbsDom.TypeConstraint
open PointerAnalyzer.AbsDom.TypeMap

type TypeSolution =
  { Constraints: ConstraintSet
    Conflicts: Set<TypeId> }

type TypeConstraintSolverModule () =
  member _.solve typeIds constraints =
    use ctx = new Context ()
    let fp = ctx.MkFixedpoint ()

    let domainSize =
      if Set.isEmpty typeIds then
        1UL
      else
        uint64 (Set.maxElement typeIds) + 1UL

    let typeIdSort = ctx.MkFiniteDomainSort ("TypeId", domainSize) :> Sort

    let parameters = ctx.MkParams ()
    parameters.Add ("engine", "datalog") |> ignore
    fp.Parameters <- parameters

    let addResult =
      ctx.MkFuncDecl (
        "addResult",
        [| typeIdSort; typeIdSort; typeIdSort |],
        ctx.BoolSort
      )

    let subResult =
      ctx.MkFuncDecl (
        "subResult",
        [| typeIdSort; typeIdSort; typeIdSort |],
        ctx.BoolSort
      )

    let same =
      ctx.MkFuncDecl ("same", [| typeIdSort; typeIdSort |], ctx.BoolSort)

    let address = ctx.MkFuncDecl ("address", [| typeIdSort |], ctx.BoolSort)

    let value = ctx.MkFuncDecl ("value", [| typeIdSort |], ctx.BoolSort)

    let conflict = ctx.MkFuncDecl ("conflict", [| typeIdSort |], ctx.BoolSort)

    [ addResult; subResult; same; address; value; conflict ]
    |> List.iter fp.RegisterRelation

    let app (relation: FuncDecl) arguments =
      ctx.MkApp (relation, Array.ofList arguments) :?> BoolExpr

    let addRule variables premises conclusion =
      let body =
        match premises with
        | [ premise ] -> premise
        | premises -> ctx.MkAnd (Array.ofList premises)

      fp.AddRule (
        ctx.MkForall (Array.ofList variables, ctx.MkImplies (body, conclusion)),
        null
      )

    let x = ctx.MkConst ("X", typeIdSort)
    let y = ctx.MkConst ("Y", typeIdSort)
    let z = ctx.MkConst ("Z", typeIdSort)

    let addressOf variable = app address [ variable ]
    let valueOf variable = app value [ variable ]
    let sameOf left right = app same [ left; right ]
    let addOf result left right = app addResult [ result; left; right ]
    let subOf result left right = app subResult [ result; left; right ]

    // Same(X,Y) propagates either known type in both directions because
    // Same constraints are inserted as ordered pairs below.
    addRule [ x; y ] [ sameOf x y; addressOf x ] (addressOf y)

    addRule [ x; y ] [ sameOf x y; valueOf x ] (valueOf y)

    // X = Y + Z
    addRule [ x; y; z ] [ addOf x y z; addressOf y; valueOf z ] (addressOf x)

    addRule [ x; y; z ] [ addOf x y z; valueOf y; addressOf z ] (addressOf x)

    addRule [ x; y; z ] [ addOf x y z; valueOf y; valueOf z ] (valueOf x)

    addRule [ x; y; z ] [ addOf x y z; addressOf x; valueOf y ] (addressOf z)

    addRule [ x; y; z ] [ addOf x y z; addressOf x; valueOf z ] (addressOf y)

    addRule [ x; y; z ] [ addOf x y z; valueOf x ] (valueOf y)
    addRule [ x; y; z ] [ addOf x y z; valueOf x ] (valueOf z)

    // X = Y - Z
    addRule [ x; y; z ] [ subOf x y z; addressOf y; valueOf z ] (addressOf x)

    addRule [ x; y; z ] [ subOf x y z; addressOf y; addressOf z ] (valueOf x)

    addRule [ x; y; z ] [ subOf x y z; valueOf y ] (valueOf x)
    addRule [ x; y; z ] [ subOf x y z; valueOf y ] (valueOf z)
    addRule [ x; y; z ] [ subOf x y z; addressOf x ] (addressOf y)
    addRule [ x; y; z ] [ subOf x y z; addressOf x ] (valueOf z)

    addRule [ x ] [ addressOf x; valueOf x ] (app conflict [ x ])

    let intExpr (typeId: TypeId) =
      ctx.MkNumeral (uint64 typeId, typeIdSort)

    let addFact relation arguments =
      arguments
      |> List.map intExpr
      |> app relation
      |> fun fact -> fp.AddRule (fact, null)

    Set.iter
      (fun constraint_ ->
        match constraint_ with
        | Address typeId -> addFact address [ typeId ]
        | Value typeId -> addFact value [ typeId ]
        | AddResult (result, left, right) ->
          addFact addResult [ result; left; right ]
        | SubResult (result, left, right) ->
          addFact subResult [ result; left; right ]
        | Same ids ->
          Set.iter
            (fun tidl ->
              Set.iter
                (fun tidr ->
                  if tidl <> tidr then
                    addFact same [ tidl; tidr ])
                ids)
            ids)
      constraints
    |> ignore

    let isDerived relation typeId =
      let query = app relation [ intExpr typeId ]
      fp.Query query = Status.SATISFIABLE

    let constraints, conflicts =
      typeIds
      |> Set.fold
        (fun (constraints, conflicts) typeId ->
          let constraints =
            if isDerived address typeId then
              Set.add (Address typeId) constraints
            else
              constraints

          let constraints =
            if isDerived value typeId then
              Set.add (Value typeId) constraints
            else
              constraints

          let conflicts =
            if isDerived conflict typeId then
              Set.add typeId conflicts
            else
              conflicts

          constraints, conflicts)
        (constraints, Set.empty)

    { Constraints = constraints
      Conflicts = conflicts }

module TypeConstraintSolver =
  let create () = TypeConstraintSolverModule ()
