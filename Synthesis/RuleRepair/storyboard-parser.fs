module storyboard_parser

open FParsec
open Microsoft.FSharp
open System.Net
open System.IO

let scenarioFilename = System.Environment.GetCommandLineArgs().GetValue(1).ToString()
let cfgFilename = System.Environment.GetCommandLineArgs().GetValue(2).ToString()

type StateExpr =
    | Loc of string
    | Deref of string * string
    | AbstLoc of string * string
    | AbstLocDeref of string * string * string
    | Value of int16

type DataSels =
    | DataSelector of string list

type ConstantVars =
    | ConstantVariable of string list

type MetaVars =
    | MetaVariable of string list

type ChoiceSels =
    | ChoiceSelector of string list

type ConstantPointerVars =
    | ConstantPointerVariable of string list

type StatePred =
    | PointsTo of StateExpr * StateExpr
    | NotPointsTo of StateExpr * StateExpr
    | LessThan of StateExpr * StateExpr
    
type InputScenario =
    | InputScenarioDesc of StatePred list

type OutputScenario =
    | OutputScenarioDesc of StatePred list

type IntermediateScenario =
    | IntermediateScenarioDesc of (string * StatePred list)


type Scenario =
    | ScenarioDesc of (string * InputScenario * OutputScenario * IntermediateScenario list) 
    //| ScenarioIODesc of (string * InputScenario * OutputScenario)

type ScenarioList =
    | Scenarios of Scenario list

type StateExprTuple =
    | StateTuple of StateExpr * StateExpr

type ListStateExprTuple =
    | StateTuples of StateExprTuple list

type UnfoldPred =
    | Unfold of (StateExpr * StateExpr * ListStateExprTuple * ListStateExprTuple * StatePred list * StateExpr list)

type FoldPred =
    | Fold of (StateExpr * StateExpr * ListStateExprTuple * ListStateExprTuple * StatePred list * StateExpr list)

type StoryboardType =
    | Storyboard of (ScenarioList * UnfoldPred list * FoldPred list * DataSels list * ConstantVars list * MetaVars list * ChoiceSels list * ConstantPointerVars list)

type ScenUnfoldFoldType =
    | SUF of (Scenario list * UnfoldPred list * FoldPred list)

type ScenUnfoldFoldsType = 
    | SUFs of ScenUnfoldFoldType list

type Statement =
    | VarDecl of string * (string list)
    | Label of string
    | Block of int
    | CBlock of int
    | While of Statement List
    | LBlock of Statement List

type MethodDecl =
    | MDecl of string * ((string* string) list)

type Program =
    | Prog of MethodDecl * Statement


let test p str = 
    match run p str with
    | Success(result, _, _) -> printfn "Success %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure %s" errorMsg


let labelIden =
    let isFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isFirstChar isIdentifierChar "Label Name" .>> spaces


let keywords = ["input"; "output"; "scenario"; "while"; "if"; "choice"]
let isKeyword str = List.exists (fun elem -> elem.Equals(str)) keywords


let locParser =  spaces >>. labelIden .>> spaces |>> Loc
let derefParser = (spaces >>. labelIden .>> pstring ".") .>>. (labelIden .>> spaces) |>> Deref
let abstLocParser = (spaces >>. labelIden .>> pstring ":") .>>. (labelIden .>> spaces) |>> AbstLoc
let abstLocDerefParser = pipe3 (spaces >>. labelIden .>> pstring ":") (labelIden .>> spaces .>> pstring ".") (labelIden .>> spaces) (fun loc att sel -> AbstLocDeref(loc, att, sel))
let ValueParser = spaces >>. pint16 .>> spaces |>> Value


let stateExprParser = (attempt abstLocDerefParser) <|> (attempt abstLocParser) <|> (attempt derefParser) <|> (attempt locParser) <|> (attempt ValueParser)

let statePredParser1 = (stateExprParser .>> spaces .>> pstring "->") .>>. (spaces >>. stateExprParser) |>> PointsTo
let statePredParser2 = (stateExprParser .>> spaces .>> pstring "!=") .>>. (spaces >>. stateExprParser) |>> NotPointsTo
let statePredParser3 = (stateExprParser .>> spaces .>> pstring "<") .>>. (spaces >>. stateExprParser) |>> LessThan


let statePredParser = (attempt statePredParser1) <|> (attempt statePredParser2) <|> (attempt statePredParser3)

let inputScenarioParser = (spaces >>. pstring "input" >>. spaces ) >>. (sepBy statePredParser (pstring ",")) |>> InputScenarioDesc
let outputScenarioParser = (spaces >>. pstring "output" >>. spaces ) >>. (sepBy statePredParser (pstring ",")) |>> OutputScenarioDesc
let intermediateScenarioParser = (spaces >>. labelIden .>> spaces ) .>>. (sepBy statePredParser (pstring ",")) |>> IntermediateScenarioDesc

//let scenarioIOParser =  pipe3 (spaces >>. pstring "scenario" >>. spaces >>. labelIden) (inputScenarioParser) (outputScenarioParser) (fun l i o -> ScenarioIODesc(l,i,o))
let scenarioParser = pipe5 (spaces >>. pstring "scenario" >>. spaces >>. labelIden) (inputScenarioParser) (many (notFollowedBy outputScenarioParser >>. intermediateScenarioParser)) (outputScenarioParser) (spaces >>. pstring ";" >>. spaces)(fun l i li o _ -> ScenarioDesc(l,i,o,li))

let scenariosParser = (many scenarioParser) |>> Scenarios



let stateExprTupleParser = pipe5 (spaces >>. pstring "(" >>. spaces) (stateExprParser) (spaces >>. pstring "," >>. spaces) (stateExprParser) (spaces >>. pstring ")" >>. spaces) (fun _ s1 _ s2 _ -> StateTuple(s1,s2))

let incomingSEexpr = pipe3 (spaces >>. pstring"[" >>. spaces >>. pstring "in" >>. spaces) (many stateExprTupleParser) (spaces >>. pstring "]" >>. spaces) (fun _ ts _ -> StateTuples(ts))
let outgoingSEexpr = pipe3 (spaces >>. pstring"[" >>. spaces >>. pstring "out" >>. spaces) (many stateExprTupleParser) (spaces >>. pstring "]" >>. spaces) (fun _ ts _ -> StateTuples(ts))

let constraintsParser = (spaces >>. pstring "(" >>. spaces) >>. (sepBy statePredParser (pstring ",")) .>> (spaces .>> pstring ")" .>> spaces)
let vanishingNodeParser = (spaces >>. pstring "(" >>. spaces) >>. (sepBy stateExprParser (pstring ",")) .>> (spaces .>> pstring ")" .>> spaces)

let pipe7 p1 p2 p3 p4 p5 p6 p7 f =
    pipe4 p1 p2 p3 (tuple4 p4 p5 p6 p7) (fun x1 x2 x3 (x4, x5, x6, x7) -> f x1 x2 x3 x4 x5 x6 x7)

let pipe6 p1 p2 p3 p4 p5 p6 f =
    pipe4 p1 p2 p3 (tuple3 p4 p5 p6) (fun x1 x2 x3 (x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)

let pipe8 p1 p2 p3 p4 p5 p6 p7 p8 f =
    pipe6 p1 p2 p3 p4 p5 (tuple3 p6 p7 p8) (fun x1 x2 x3 x4 x5 (x6, x7, x8) -> f x1 x2 x3 x4 x5 x6 x7 x8)

let unfoldStmtParser = pipe7 (spaces >>. pstring "unfold" >>. spaces) (stateExprParser) (stateExprParser) (incomingSEexpr) (outgoingSEexpr) (constraintsParser) (vanishingNodeParser .>> spaces .>> pstring ";" .>> spaces) (fun _ en rn inedges outedges constraints vn -> Unfold(en, rn, inedges, outedges, constraints, vn))
let foldStmtParser = pipe7 (spaces >>. pstring "fold" >>. spaces) (stateExprParser) (stateExprParser) (incomingSEexpr) (outgoingSEexpr) (constraintsParser) (vanishingNodeParser .>> spaces .>> pstring ";" .>> spaces) (fun _ en rn inedges outedges constraints vn -> Fold(en, rn, inedges, outedges, constraints, vn))

let dataSelParser = pipe3 (spaces >>. pstring "data_selector" >>. spaces) (sepBy labelIden (spaces >>. pstring "," >>. spaces)) (spaces >>. pstring ";" >>. spaces ) (fun _ dataSels _ -> DataSelector(dataSels))
let constantVarsParser = pipe3 (spaces >>. pstring "constant_variables" >>. spaces) (sepBy labelIden (spaces >>. pstring "," >>. spaces)) (spaces >>. pstring ";" >>. spaces ) (fun _ dataSels _ -> ConstantVariable(dataSels))
let metaVarsParser = pipe3 (spaces >>. pstring "meta_variables" >>. spaces) (sepBy labelIden (spaces >>. pstring "," >>. spaces)) (spaces >>. pstring ";" >>. spaces ) (fun _ dataSels _ -> MetaVariable(dataSels))
let choiceSelParser = pipe3 (spaces >>. pstring "choice_selector" >>. spaces) (sepBy labelIden (spaces >>. pstring "," >>. spaces)) (spaces >>. pstring ";" >>. spaces ) (fun _ dataSels _ -> ChoiceSelector(dataSels))
let constantPVarsParser = pipe3 (spaces >>. pstring "constant_pointer_variables" >>. spaces) (sepBy labelIden (spaces >>. pstring "," >>. spaces)) (spaces >>. pstring ";" >>. spaces ) (fun _ dataSels _ -> ConstantPointerVariable(dataSels))



let scenUnfoldFoldParser1 = pipe3 (unfoldStmtParser) (many foldStmtParser) (many scenarioParser) (fun x y z -> SUF(z, [x], y))
let scenUnfoldFoldParser2 = pipe3 (many unfoldStmtParser) (foldStmtParser) (many scenarioParser) (fun x y z -> SUF(z, x, [y]))
let scenUnfoldFoldParser3 = pipe3 (many unfoldStmtParser) (many foldStmtParser) (scenarioParser) (fun x y z -> SUF([z], x, y))
let scenUnfoldFoldsParser = many (scenUnfoldFoldParser1 <|> scenUnfoldFoldParser2 <|> scenUnfoldFoldParser3) |>> SUFs

let rec getSUFList x = 
    match x with
        | suf::restsuf -> 
            match suf with
                | SUF(a,b,c) -> 
                    let (a1, b1, c1) = getSUFList restsuf
                    (List.append a a1, List.append b b1, List.append c c1)
        | [] -> ([],[],[])

let getSUFsLists x = 
    match x with
        | SUFs(suflist) -> getSUFList(suflist)

let storyboardParser = pipe6 (scenUnfoldFoldsParser) (many dataSelParser) (many constantVarsParser) (many metaVarsParser) (many choiceSelParser) (many constantPVarsParser)  (fun scenufs dsels cvars mvars csels cpvars ->
                                                                                                            let (scens, us, fs) = getSUFsLists scenufs
                                                                                                            Storyboard(Scenarios(scens), us, fs, dsels, cvars, mvars, csels, cpvars))
let storyboardParser1 = pipe8 (scenariosParser) (many unfoldStmtParser) (many foldStmtParser) (many dataSelParser) (many constantVarsParser) (many metaVarsParser) (many choiceSelParser) (many constantPVarsParser)  (fun scens us fs dsels cvars mvars csels cpvars -> Storyboard(scens, us, fs, dsels, cvars, mvars, csels, cpvars))


let hole_ws = pstring "??" >>. spaces
let cassign_hole_ws = pstring "?=" >>. spaces
let cond_hole_ws = pstring "**" >>. spaces
let open_paren_ws = pstring "(" >>. spaces
let close_paren_ws = pstring ")" >>. spaces

let unknownStmtBlockParser = hole_ws >>. open_paren_ws >>. pint32 .>> close_paren_ws |>> Block
let unknownCassignStmtBlockParser = cassign_hole_ws >>. open_paren_ws >>. pint32 .>> close_paren_ws |>> CBlock

let whileKeywordParser = pstring "while" >>. spaces >>.  open_paren_ws >>. cond_hole_ws >>.  close_paren_ws

let svalue, svalueRef = createParserForwardedToRef<Statement,unit>()

//let listBlockParser = spaces >>. (sepEndBy (unknownStmtBlockParser <|> whileStmtParser <|> labelNameParser) spaces) |>> LBlock

let listBlockParser = spaces >>. pstring "{" >>. spaces >>. (sepEndBy (svalue) spaces) .>> spaces .>> pstring "}" .>> spaces |>> LBlock
//let listBlockParser = spaces >>. (sepEndBy (svalue) spaces) .>> spaces |>> LBlock

let whileStmtParser = (whileKeywordParser >>. spaces >>. pstring "{" >>. spaces >>.  (sepEndBy (svalue) spaces) .>> spaces .>> pstring "}" .>> spaces) |>> While

let labelIden_ws = spaces >>. labelIden .>> spaces
let mdeclParser = ( (spaces >>. labelIden  .>> pstring "(") .>>. (sepBy (labelIden_ws .>>. labelIden_ws) (pstring "," .>> spaces)) ) .>> pstring ")" .>> spaces |>> MDecl
let vardeclParser = ( (spaces >>. labelIden  .>> spaces) .>>. (sepBy (labelIden_ws) (pstring "," .>> spaces)) ) .>> pstring ";".>> spaces |>> VarDecl
let labelNameParser = labelIden .>> pstring ":" .>> spaces |>> Label

do svalueRef := choice [listBlockParser; unknownStmtBlockParser; unknownCassignStmtBlockParser; whileStmtParser;  (attempt labelNameParser); vardeclParser]

let programParser = (spaces >>. mdeclParser .>> spaces) .>>. (spaces >>. listBlockParser .>> spaces) |>> Prog


// Compute CFG from loop skeleton

let mutable functionCounter = 1

let getProgramAST str = 
    match run programParser str with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg

let rec getMethodParams md =
    match md with
    | MDecl(mname, args) -> [for (argType, argName) in args -> argName]

let rec getMethodBodyParams st =
    match st with
    | LBlock(stmts) -> List.concat (List.map(fun x -> getMethodBodyParams x) stmts)
    | While(stmts) -> List.concat (List.map(fun x -> getMethodBodyParams x) stmts)
    | Block(_) -> []
    | CBlock(_) -> []
    | Label(_) -> []
    | VarDecl(_,vars) -> vars 

let pccount = ref 0
let mutable startPC = 0
let mutable returnPC = 0
let mutable loopLocPCs = []
let mutable labelPCs : (string * int) list = []
let mutable fcount = 0

let newfname () =
    fcount <- fcount + 1
    "f" + string(fcount)

let incrementPC () =
    pccount := !pccount + 1
    !pccount

let getCFGPred ()=
    pccount := !pccount + 1
    let fname = newfname()
    let pcToloc = !pccount + 1 
    (!pccount,fname,pcToloc,"assign")

let getCFGPredCond ()=
    pccount := !pccount + 1
    let fname = newfname()
    let pcToloc = !pccount + 1 
    (!pccount,fname,pcToloc,"cassign")

let rec computeCFG st =
    match st with
    | LBlock(stmts) -> startPC <- (!pccount + 1)
                       let result = List.concat (List.map(fun x -> computeCFG x) stmts)
                       returnPC <- (!pccount + 1)
                       result
    | While(stmts) -> pccount := !pccount + 1
                      let pcFromLoc = !pccount
                      loopLocPCs <- loopLocPCs @ [pcFromLoc]
                      let fname = newfname()
                      let pcToloc = pcFromLoc + 1
                      pccount := !pccount + 1
                      let pcFromLoc1 = !pccount
                      let fname1 = newfname()
                      let fname2 = newfname()
                      let fname3 = newfname()
                      let pcToLoc1 = pcFromLoc1 + 1 
                      [(pcFromLoc,fname,pcToloc,"cond")] @ [(pcFromLoc1,fname1,pcToLoc1,"unfold")] @ List.concat (List.map(fun x -> computeCFG x) stmts) @ [(!pccount+1, fname2, pcFromLoc, "fold")] @ [(pcFromLoc,fname3,incrementPC()+1, "negcond")]
    | Block(n) -> [for i in 1..n -> getCFGPred()] 
    | CBlock(n) -> [for i in 1..n -> getCFGPredCond()] 
    | Label(l) -> labelPCs <- labelPCs @ [(l,!pccount + 1)]
                  []
    | VarDecl(_,vars) -> []


let rec getProgramVars program =
    match program with
    | Prog(md, st) -> 
         let vL1 = (getMethodParams md)
         let vL2 = (getMethodBodyParams st)
         vL1 @ vL2
         
let rec getCFGPreds program =
    match program with
    | Prog(md, st) -> 
        computeCFG st



let srcProg1 = File.ReadAllText(cfgFilename)


let programAST = getProgramAST srcProg1

let printProgramVars varList =
    printf "variables(["
    match varList with
    | firstVar :: restVars ->  
        printf "%s" firstVar
        ignore (List.map(fun x -> printf ", %s" x) restVars)
    | [] -> printf ""
    printfn "])."

let printPCs ()=
    printfn "start_location(%d)." startPC
    printfn "return_location(%d)." returnPC
    List.iter(fun x -> printfn "loop_location(%d)." x) loopLocPCs

let cfgList = (getCFGPreds programAST)

let printCFGs () =
    List.iter( fun (from, fname, topc, stmt) -> printfn "cfg_edge(%d,%s,%d,%s)." from fname topc stmt) cfgList


printProgramVars (getProgramVars programAST)
printPCs()
printCFGs()

let rec findLabelPC label listPCs=
    match listPCs with
    | lpc::lpcs -> 
                    let (lab1,pc1) = lpc
                    if(lab1 = label) then pc1
                    else findLabelPC label lpcs
    | [] -> failwith "No label found"



// Storyboard
let removeDupsList l =
    l |> Set.ofList |> Set.toList

let storyboardSrc = File.ReadAllText(scenarioFilename)

let getStoryboardAST str =
    match run storyboardParser str with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg

//let scenarioSrc = File.ReadAllText(scenarioFilename)

let getScenariosAST sbAST =
    match sbAST with
    | Storyboard(scenAST, unfolds, folds, dsels, cvars, mvars, csels, cpvars) -> scenAST

let getUnfoldsAST sbAST =
    match sbAST with
    | Storyboard(scenAST, unfolds, folds, dsels, cvars, mvars, csels, cpvars) -> unfolds

let getFoldsAST sbAST =
    match sbAST with
    | Storyboard(scenAST, unfolds, folds, dsels, cvars, mvars, csels, cpvars) -> folds

let getDSels sbAST =
    match sbAST with
    | Storyboard(scenAST, unfolds, folds, dsels, cvars, mvars, csels, cpvars) -> dsels

let getCVars sbAST =
    match sbAST with
    | Storyboard(scenAST, unfolds, folds, dsels, cvars, mvars, csels, cpvars) -> cvars

let getMVars sbAST =
    match sbAST with
    | Storyboard(scenAST, unfolds, folds, dsels, cvars, mvars, csels, cpvars) -> mvars

let getCSels sbAST =
    match sbAST with
    | Storyboard(scenAST, unfolds, folds, dsels, cvars, mvars, csels, cpvars) -> csels

let getCPVars sbAST =
    match sbAST with
    | Storyboard(scenAST, unfolds, folds, dsels, cvars, mvars, csels, cpvars) -> cpvars


let mutable UnfoldSummaryNodes : string list = []

let getLocsStateExpr se =
    match se with
    | Deref(loc,sel) -> [loc]
    | AbstLoc(aloc,att) -> [aloc + att]
    | AbstLocDeref(aloc,att,sel) -> [aloc + att]
    | Loc(str) -> []
    | Value(i) -> []

let getSelsStateExpr se =
    match se with
    | Deref(loc,sel) -> [sel]
    | AbstLoc(aloc,att) -> []
    | AbstLocDeref(aloc,att,sel) -> [sel]
    | Loc(str) -> []
    | Value(i) -> []

let getSummaryNodesStateExpr se =
    match se with
    | Deref(loc,sel) -> []
    | AbstLoc(aloc,att) -> [aloc+att]
    | AbstLocDeref(aloc,att,sel) -> []
    | Loc(str) -> []
    | Value(i) -> []

let getLocsStateExpr1 se =
    match se with
    | Deref(loc,sel) -> []
    | AbstLoc(aloc,att) -> [aloc]
    | AbstLocDeref(aloc,att,sel) -> [aloc]
    | Loc(str) -> []
    | Value(i) -> []


let storyboardAST = getStoryboardAST storyboardSrc
let scenariosASTOrig = getScenariosAST storyboardAST
let unfoldsASTorig = getUnfoldsAST storyboardAST
let foldsASTorig = getFoldsAST storyboardAST
let dataSelectorsList = 
    let dSelsList = getDSels storyboardAST
    match dSelsList with
    | dSels::restDSels -> 
        match dSels with
        | DataSelector(dSel) -> dSel
    | [] -> []

let constantVarsList = 
    let cVarsList = getCVars storyboardAST
    match cVarsList with
    | cVars::restDSels -> 
        match cVars with
        | ConstantVariable(cVar) -> cVar
    | [] -> []

let constantPVarsList = 
    let cVarsList = getCPVars storyboardAST
    match cVarsList with
    | cVars::restDSels -> 
        match cVars with
        | ConstantPointerVariable(cVar) -> cVar
    | [] -> []

let metaVarsList = 
    let mVarsList = getMVars storyboardAST
    match mVarsList with
    | mVars::restDSels -> 
        match mVars with
        | MetaVariable(mVar) -> mVar
    | [] -> []

let choiceSelsList = 
    let cSelsList = getCSels storyboardAST
    match cSelsList with
    | mVars::restDSels -> 
        match mVars with
        | ChoiceSelector(mVar) -> mVar
    | [] -> []


let computeAllUnfoldNodes = List.map (fun x -> ( 
                                                 match x with
                                                    | Unfold(en,_,_,_,_,_) -> UnfoldSummaryNodes <- UnfoldSummaryNodes @ (getLocsStateExpr1 en))) unfoldsASTorig

let isUnfoldDefined n =
    let nSet = Set.ofList  (removeDupsList UnfoldSummaryNodes)
    nSet.Contains(n)

// make abstract nodes in the output state primed mid --> mid1

//printfn "unfold defined nodes : %A" UnfoldSummaryNodes

let transformStateExpr se =
    match se with
    | Loc(s) -> Loc(s)
    | Deref(s1, s2) -> Deref(s1, s2)
    | AbstLoc(s1, s2) -> if (isUnfoldDefined (s1)) then AbstLoc(s1+"1",s2) else AbstLoc(s1,s2)
    | AbstLocDeref(s1,s2,s3) -> if (isUnfoldDefined (s1)) then AbstLocDeref(s1+"1",s2,s3) else AbstLocDeref(s1,s2,s3)
    | Value(i) -> Value(i)


let rec transformStatePred sp =
    match sp with
    | PointsTo(se1, se2) -> PointsTo( transformStateExpr se1, transformStateExpr se2)
    | NotPointsTo(se1, se2) -> NotPointsTo( transformStateExpr se1, transformStateExpr se2)
    | LessThan(se1, se2) -> LessThan( transformStateExpr se1, transformStateExpr se2)
    //| Or(splist) -> Or(List.map( fun x -> transformStatePred x) splist)

let transformOutputScen os =
    match os with
    | OutputScenarioDesc(sps) -> OutputScenarioDesc(List.map (fun x-> transformStatePred x) sps)

let transformAST scenAST =
    match scenAST with
    | ScenarioDesc(s,i,o,il) -> ScenarioDesc(s,i, transformOutputScen o, il)

let transformEdges edges =
    match edges with
    | StateTuples(es) -> StateTuples(List.map(fun x -> 
                                                match x with
                                                    | StateTuple(se1, se2) -> StateTuple(transformStateExpr se1, transformStateExpr se2)) es )
        
let transformFoldAST foldAST =
    match foldAST with
    | Fold(en,rn,inedges,outedges,constraints,vn) -> Fold(transformStateExpr en, transformStateExpr rn, (transformEdges inedges), transformEdges outedges, List.map(fun x -> transformStatePred x) constraints, List.map(fun x -> transformStateExpr x) vn)

let transformFoldsAST foldsAST =
    List.map(fun x -> transformFoldAST x) foldsAST
//let scenarioAST = transformAST scenarioASTOrig

let scenariosAST = 
     match scenariosASTOrig with
     | Scenarios(scenList) ->  List.map(fun x -> transformAST x) scenList

let transformedFoldsAST = transformFoldsAST foldsASTorig



let rec traverseStatePred sp f =
    match sp with
    |PointsTo(sp1,sp2) -> (f sp1) @ (f sp2)
    |NotPointsTo(sp1,sp2) -> (f sp1) @ (f sp2)
    |LessThan(sp1,sp2) -> (f sp1) @ (f sp2)
    //|Or(spList) -> List.concat (List.map (fun x-> traverseStatePred x f) spList)

let traverseStatePreds sps f =
    List.concat (List.map(fun x -> traverseStatePred x f) sps)

let traverseInputScenarios iscen f =
    match iscen with
    | InputScenarioDesc(sps) -> traverseStatePreds sps f

let traverseOutputScenarios iscen f =
    match iscen with
    | OutputScenarioDesc(sps) -> traverseStatePreds sps f

let traverseInterScenarios iscen f =
    match iscen with
    | IntermediateScenarioDesc(_,sps) -> traverseStatePreds sps f


let traverseScenAST scen f =
    match scen with
        | ScenarioDesc(name,inputScen, outputScen, interScen) -> removeDupsList ((traverseInputScenarios inputScen f) @ (traverseOutputScenarios outputScen f) @ List.concat(List.map(fun x -> traverseInterScenarios x f) interScen))


let printList locs str=
    match locs with
    | loc::restLocs ->
        printf "%s([%s" str loc 
        let r = List.iter(fun x-> printf ", %s" x) restLocs
        printfn "])."
    | [] -> printf ""

let printProlog f str = printList (removeDupsList (List.concat(List.map(fun x -> traverseScenAST x f) scenariosAST))) str

let rec getLocsConstraints f cs =
    match cs with
    | c :: restcs -> (traverseStatePred c f) @ (getLocsConstraints f restcs)
    | [] -> []

let rec getUnfoldLocs f unfoldsAST =
    match unfoldsAST with
    | Unfold(_,_,_,_,constraints,_):: restunfolds -> (getLocsConstraints f constraints) @ getUnfoldLocs f restunfolds
    | [] -> []

let rec getFoldLocs f unfoldsAST =
    match unfoldsAST with
    | Fold(_,_,_,_,constraints,_):: restunfolds -> (getLocsConstraints f constraints) @ getFoldLocs f restunfolds
    | [] -> []

let unfoldLocs = removeDupsList (getUnfoldLocs getLocsStateExpr unfoldsASTorig) 
let foldLocs = removeDupsList (getFoldLocs getLocsStateExpr transformedFoldsAST) 
let scenLocs = (List.concat(List.map(fun x -> traverseScenAST x getLocsStateExpr) scenariosAST))    

let allLocs = removeDupsList(scenLocs @ unfoldLocs @ foldLocs)

let listDiff l1 l2 =
    let s1 = Set.ofList l1
    let s2 = Set.ofList l2
    let sdiff = Set.difference s1 s2
    Set.toList sdiff

let unfoldSels = removeDupsList (getUnfoldLocs getSelsStateExpr unfoldsASTorig)
let foldSels = removeDupsList (getFoldLocs getSelsStateExpr transformedFoldsAST)

let scenSels = (removeDupsList (List.concat(List.map(fun x -> traverseScenAST x getSelsStateExpr) scenariosAST)))
let allSels = removeDupsList (scenSels @ unfoldSels @ foldSels)
let Sels = listDiff allSels dataSelectorsList

//printProlog getLocsStateExpr "locations"
printList allLocs "locations"
printList Sels "selectors"
printList dataSelectorsList "data_selectors"
printList metaVarsList "meta_variables"
printList constantVarsList "constant_variables"
printList choiceSelsList "choice_selectors"
printList constantPVarsList "constant_pointer_variables"
// printProlog getSelsStateExpr "selectors"
printProlog getSummaryNodesStateExpr "summary_nodes"


let mutable scenarioCount = -1

let getNewScenarioId () =
    scenarioCount <- scenarioCount + 1
    scenarioCount

let getStatePredStateExpr se =
    match se with
    | Deref(loc,sel) -> "("+loc+","+sel+")"
    | AbstLoc(aloc,att) -> "(" + aloc+att + ")"
    | AbstLocDeref(aloc,att,sel) -> "(" + aloc+att + "," + sel + ")"
    | Loc(str) -> str
    | Value(i) -> i.ToString()


let rec getStatePred ast = 
    match ast with
        |PointsTo(sp1,sp2) -> ["(eq, " + getStatePredStateExpr sp1 + ", " + getStatePredStateExpr sp2 + ")"]
        |NotPointsTo(sp1,sp2) -> ["(neq, " + getStatePredStateExpr sp1 + ", " + getStatePredStateExpr sp2 + ")"]
        |LessThan(sp1,sp2) -> ["(lt, " + getStatePredStateExpr sp1 + ", " + getStatePredStateExpr sp2 + ")"]


let getStatePredsInputScenarios k iscen =
    match iscen with
    | InputScenarioDesc(sps) -> [k] @ List.concat( List.map( fun x -> getStatePred x) sps)

let getStatePredsOutputScenarios k iscen =
    match iscen with
    | OutputScenarioDesc(sps) -> [k] @ List.concat( List.map( fun x -> getStatePred x) sps)

let getStatePredsInterScenarios k iscen =
    match iscen with
    | IntermediateScenarioDesc(str, sps) -> [k] @ [(findLabelPC str labelPCs).ToString()] @ List.concat(List.map(fun x -> getStatePred x) sps)


let getStatePredsScenAST scen =
    let scenId = getNewScenarioId()
    match scen with
        | ScenarioDesc(name,inputScen, outputScen, interScen) -> (getStatePredsInputScenarios (scenId.ToString()) inputScen, (getStatePredsOutputScenarios (scenId.ToString()) outputScen, List.map( fun x -> getStatePredsInterScenarios (scenId.ToString()) x) interScen ))


let printVanishingNode vns =
    match vns with
    | vn::restvns -> "[" + (getStatePredStateExpr vn) + "]"
    | [] -> "[]"

let rec printEdges edges str =
    match edges with
        | StateTuple(n1, n2) :: restEdges -> 
            let result = str + "(" + (getStatePredStateExpr n1) + "," + (getStatePredStateExpr n2) + ")"
            match restEdges with
            | [] -> result
            | _ -> result + ", " + (printEdges restEdges str)
        | [] -> ""

let printInOutEdges inedges outedges =
    match inedges with
    | StateTuples(inEdgesList) ->
        match outedges with
            | StateTuples(outEdgesList) ->
                "[" + (printEdges inEdgesList "in") + ", " + (printEdges outEdgesList "out") + "]"

let rec printListConstraints l =
    match l with
    | h::ts -> 
        match h with
        | h1::hs ->
            match ts with
            | [] -> h1 
            | _ -> h1 + ", " + (printListConstraints ts)
        | [] -> ""
    | [] -> ""

let printConstraints constraints =
    let l1 = List.map(fun x -> getStatePred x) constraints
    "[" + printListConstraints l1 + "]"


let writeUnfoldPredicates unfoldStatement =
    match unfoldStatement with
    | Unfold(en, rn, inedges, outedges, constraints, vns) -> printfn "unfoldPred(%s, %s, %s, %s, %s)." (getStatePredStateExpr en) (getStatePredStateExpr rn) (printInOutEdges inedges outedges) (printConstraints constraints) (printVanishingNode vns)


let writeFoldPredicates foldStatement =
    match foldStatement with
    | Fold(en, rn, inedges, outedges, constraints, vns) -> printfn "foldPred(%s, %s, %s, %s, %s)." (getStatePredStateExpr en) (getStatePredStateExpr rn) (printInOutEdges inedges outedges) (printConstraints constraints) (printVanishingNode vns)

 

let printList1 locs str=
    match locs with
    | loc::restLocs ->
       match restLocs with 
       | r1::r1s ->
            printf "%s(%s,[%s" str loc r1 
            let r = List.iter(fun x-> printf ", %s" x) r1s
            printfn "])."
       | [] -> printf ""
    | [] -> printf ""

let printList2 locs str=
    match locs with
    | loc::restLocs ->
       match restLocs with 
       | r1::r1s ->
           match r1s with
               | r2::r2s ->
                    printf "%s(%s,%s,[%s" str loc r1 r2 
                    let r = List.iter(fun x-> printf ", %s" x) r2s
                    printfn "])."
               | [] -> printf ""
        | [] -> printf ""
    | [] -> printf ""


let printIOScen scenTuples = 
    printList1 (fst(scenTuples)) "input_state"
    printList1 (fst(snd(scenTuples))) "output_state"
    List.map( fun x -> printList2 x "intermediate_state") (snd(snd(scenTuples)))

let printIOScens scenarios = List.map(fun x -> printIOScen (getStatePredsScenAST x)) scenarios

let r1 = printIOScens scenariosAST

let numScen = scenarioCount + 1

printfn "num_scenarios(%d)." numScen

printfn "num_unrollings(4)."

printfn "ghost_location([])."

ignore (List.map(fun x -> writeUnfoldPredicates x) unfoldsASTorig)
ignore (List.map(fun x -> writeFoldPredicates x) transformedFoldsAST)