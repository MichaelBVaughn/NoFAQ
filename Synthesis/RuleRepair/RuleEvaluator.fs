module RuleEvaluator

open Parser
open RRUtilities
//TODO: Ensure this handles new Var reasonably
let evalExpr (e:Expr) (env:Map<int,string>) = 
    match e with    
    | ConstStr(str) -> str
    | Var(id,_,_,_) -> env.Item id


// returns the index corresponding to the position expression p
let evalPos (p:Pos) (s:string) : int option = 
    match p with 
    | CPos(i) -> Some(i)
    | SymPos(c,occ,offset) -> 
        let matches = findMatches c (convertToCharSeq s) in        
        if(occ>0) then
            if(matches.Length<occ) then 
                None
            else
                Some((List.nth matches (occ-1))+offset)
        else
           if(matches.Length<(-occ)) then 
                None
           else
                Some((List.nth matches (matches.Length+occ))+offset)
        
let evalSubstr fromInd toInd (str:string)= 
    if(fromInd<0) then
            if(toInd>0) then
                str.Substring(str.Length+fromInd,toInd-str.Length-fromInd)
            else
                str.Substring(str.Length+fromInd,toInd-fromInd)             
        else
            if(toInd>0) then
                str.Substring(fromInd,toInd-fromInd)
            else
                str.Substring(fromInd,str.Length+toInd-fromInd)    
                
let evalSubstrLength fromInd toInd (str:string)= 
    if(fromInd<0) then
            if(toInd>0) then
                toInd-str.Length-fromInd
            else
                toInd-fromInd    
        else
            if(toInd>0) then
                toInd-fromInd
            else
                str.Length+toInd-fromInd                   

let evalFun (f:Fun) (str:string) = 
    match f with
    | SubstrAndAppend(((fromPos,toPos)::_, left,right)::_) -> 
        try
            option{
                let! fromInd = evalPos fromPos str in
                let! toInd = evalPos toPos str in
                let substring = evalSubstr fromInd toInd str in       
                    return left+substring+right
            }
        with 
            | :? System.ArgumentOutOfRangeException -> None
            | :? System.ArgumentException ->  None
    //| AppendLeftRight(left,right) -> Some(left+str+right)
        

let evalFixExpr (e:FixExpr) (env:Map<int,string>)= 
    match e with 
    | FixConstStr(str) -> Some(str)
    | FixFuncApp(l,_) -> match (List.nth l 0) with
                        | (f, varId) -> evalFun f (env.Item varId)


// Pattern matches list of patterns and string in parallel
// Returns variable assignment if matching succeeds and None otherwise
let rec evalCmdParams (exprList: Expr list) (symbStr:SymbString) (env:Map<int,string>) = 
    if(not(exprList.Length = symbStr.Length)) then 
        None
    else    
        match (exprList, symbStr) with 
        | ([],[]) -> Some(env)
        | (e1::t1, e2::t2) -> 
                    match e1 with
                    | ConstStr(str) -> 
                        if(str.Equals(e2)) then
                            evalCmdParams t1 t2 env                            
                        else
                            None
                    | VarEq(_,_) -> evalCmdParams t1 t2 env //Evaluate varEq on a second pass, after we've built up env
                    | Var(varId, prefix, suffix, _)    -> 
                            if(env.ContainsKey(varId)) then
                                None
                            elif (matchPrefixAndSuffix (Seq.toList prefix) (Seq.toList suffix) (Seq.toList e2)) then
                                evalCmdParams t1 t2 (env.Add(varId,e2))
                            else 
                                None
        | _ -> None  


// Pattern matches list of patterns and string in parallel
// Returns variable assignment if matcing succeeds and None otherwise
// This one is for error messages, it only looks at partial match and does not 
// require equal length. It's invoked on some suffix of the error string
let rec evalErrParams (exprList: Expr list) (symbStrSuff:SymbString) (env:Map<int,string>) = 
    if(exprList.Length > symbStrSuff.Length) then 
        None
    else    
        match (exprList, symbStrSuff) with 
        | ([], _) -> Some env
        | (e1:: t1, e2::t2) -> 
                    match e1 with
                    | ConstStr(str) -> 
                        if(str.Equals(e2)) then
                            evalErrParams t1 t2 env                            
                        else
                            None
                    | VarEq(_,_) -> evalCmdParams t1 t2 env
                    | Var(varId, prefix, suffix, _)    -> 
                            if(env.ContainsKey(varId)) then
                                None
                            elif (matchPrefixAndSuffix (Seq.toList prefix) (Seq.toList suffix) (Seq.toList e2)) then
                                evalErrParams t1 t2 (env.Add(varId,e2))
                            else
                                None
        | _ -> None  

let rec evalVarEq exprList symStr env =
     match (exprList,symStr) with
     | ([],[]) -> Some env
     | (e1::t1, s2::t2) -> 
                match e1 with
                | VarEq(i,_) -> match (Map.find i env) = s2 with
                                | true -> evalVarEq t1 t2 env
                                | false -> None
                | _ -> evalVarEq t1 t2 env
     | _ -> None



let rec iterEvalErr exprList symbStr env =
    match (evalErrParams exprList symbStr env) with
    | None -> match symbStr with
                | [] -> None
                | h::t -> iterEvalErr exprList t env
    | Some e -> Some e

let evalCmdMatch (e:CmdMatch) (str:SymbString) (env:Map<int,string>) = 
    match e with 
    | CmdParams(el) -> evalCmdParams el str env
                                                   
let evalErr (e:ErrMatch) str env = 
    match e with 
    | ErrContent(el) -> iterEvalErr el str env        

let rec iterEvalFixCmd exprList env =
    match exprList with
    | [] -> Some([])
    | h::t -> option{
                let! str = (evalFixExpr h env) in
                let! tt = (iterEvalFixCmd t env) in
                return str::tt
              }              
    
let evalFixCmd (e:FixCmd) (env:Map<int,string>) = 
    match e with 
    | FixCmdParams(el) -> iterEvalFixCmd el env

let evalCmdVarEq e cmdstr env =
    match e with
    | CmdParams(e1) -> evalVarEq e1 cmdstr env

let iterEvalErrVarEq exprList errstr env =
    match (evalVarEq exprList errstr env) with
    | None -> match errstr with
                | [] -> None
                | h::t -> iterEvalErr exprList t env
    | Some e -> Some e   

let evalErrVarEq e errstr env =
    match e with
    | ErrContent(e1) -> iterEvalErrVarEq e1 errstr env

let evalTopLevelExpr (e:TopLevelExpr) (cmdstr:SymbString) (errstr:SymbString)  = 
    match e with
    | FixRule(cmd,err,fixcmd,_) -> 
        option{
            let env = Map.empty 
            let! env1 = evalCmdMatch cmd cmdstr env in
            let! env2 = evalErr err errstr env1 in
            let! env3 = evalCmdVarEq cmd cmdstr env2 in
            let! env4 = evalErrVarEq err errstr env3
            let! fixedString = evalFixCmd fixcmd env4 in
            return fixedString
        }

let findAnchors' eList =
    let accFxn acc eExpr =
        let (before, after, accList) = acc in
        let newAccList = match eExpr with
                         | ConstStr s -> (s, before, after) :: accList
                         | _ -> accList in
        (before + 1, after - 1, newAccList) in
    List.fold accFxn (0, List.length eList - 1, []) eList

let third (_,_,c) = c
let findAnchors = findAnchors' >> third

let getAllAnchorMatchesForIdx anchorList msgTok msgBefore msgAfter =
    let isAnchorMatch (cStr, eBefore, eAfter) = 
        cStr = msgTok && eBefore >= msgBefore && eAfter >= msgAfter in
    List.filter isAnchorMatch anchorList

let findAnchorMatchesForError errMsg anchorList =
    let accFxn (msgBefore, msgAfter, accLst) msgTok =
        let matches = getAllAnchorMatchesForIdx anchorList msgTok msgBefore msgAfter in
        let newAccLst = (msgBefore, msgAfter, matches) :: accLst in
        (msgBefore + 1, msgAfter - 1, newAccLst) in
    List.fold accFxn (0, List.length errMsg - 1, []) errMsg

let rec renameVars exprs varMap =
    let accFxn expr (renamed, renamedIDs) =
        match expr with
        | ConstStr s -> ((ConstStr s)::renamed, renamedIDs)
        | VarEq(i, x) -> if Set.contains i renamedIDs then
                            (expr::renamed, renamedIDs)
                         else
                             let v = Map.find i varMap in
                             (v::renamed, Set.add i renamedIDs)
        | Var(i,pref,suf,x) -> let v = Var(i,pref,suf,x) in
                               (v::renamed, Set.add i renamedIDs) in
    List.foldBack accFxn exprs ([], Set.empty) 

let truncateAndRenameVars start len exprs =
    let varMapBuilder vMap expr = 
         match expr with
         | Var(i,pref,suf,x) -> Map.add i (Var(i,pref,suf,x)) vMap
         | _ -> vMap in
    let varMap = List.fold varMapBuilder Map.empty exprs
    let truncated = exprs |> List.skip start |> List.take len in
    renameVars truncated varMap

let appendIfSome v lst =
    match v with
    |Some s -> s :: lst
    | _ -> lst

let getValidFixExpr varIDs fExpr = 
   match fExpr with
   | FixConstStr s -> Some fExpr
   | FixFuncApp (funApps, outputs) ->
            let filterPred (_,id) =
                Set.contains id varIDs in
            match List.filter filterPred funApps with
            | [] -> None
            | l  -> Some (FixFuncApp (l, outputs))

let rec getValidFixCmd varIDs fCmd =
    match fCmd with 
    | [] -> Some []
    | hd::tl -> option{
                  let! rem = getValidFixCmd varIDs tl 
                  let! filteredExpr = getValidFixExpr varIDs hd
                  return filteredExpr :: rem
                }

let makeAnchoredRules substrLen msgBefore msgAfter potentialMatches topLvlRule =
    let (FixRule (cmd, (ErrContent errExprs), (FixCmdParams fixCmd), count)) = topLvlRule in
    let accFxn lst (_, expBefore, expAfter) =
        let start = expBefore - msgBefore in //TO DO: check this
        let res = option{
                    let (renamedErrExpr, varIDs) = truncateAndRenameVars start substrLen errExprs
                    let!  newFix = getValidFixCmd varIDs fixCmd
                    return FixRule(cmd, ErrContent renamedErrExpr, FixCmdParams newFix, count)
                  } in
        appendIfSome res lst in
    List.fold accFxn [] potentialMatches

let getAnchoredRules substrLen topLvlRule anchorMatches =    
    let mapFxn (msgBefore, msgAfter, potentialMatches) =
        makeAnchoredRules substrLen msgBefore msgAfter potentialMatches topLvlRule in
     let anchoredRules = List.map mapFxn anchorMatches in
     List.concat anchoredRules

let evalAnchoredRules cmd err anchoredRules =
    List.map (fun exp -> evalTopLevelExpr exp cmd err) anchoredRules
    |>  List.filter Option.isSome
    |>  List.map Option.get