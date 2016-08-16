module RuleSynthesizer

open RuleEvaluator
open RRUtilities
open Parser

//Utility method to consolidate duplicated code
//Obtain the longest prefix shared by newToken and pref, and longest suffix shared by newToken and suf
let getUpdatedPrefixAndSuffix newToken pref  suf =
    let commonPrefix = (longestCommonPrefix (Seq.toList newToken) (Seq.toList pref)) in
    let commonSuffix = (longestCommonSuffix (Seq.toList newToken) (Seq.toList suf)) in
        (System.String.Concat commonPrefix, System.String.Concat commonSuffix)
//More consolidation of duplicated code - get the longest prefix and suffix shared by tokenA and tokenB
let getSharedPrefAndSuf tokenA tokenB = getUpdatedPrefixAndSuffix tokenA tokenB tokenB

//Get prefix and suffix shared by a list of strings
let getListPrefAndSuf (hd::tl) = List.fold (fun (pref, suf) str -> getUpdatedPrefixAndSuffix str pref suf) (hd,hd) (hd::tl)

//--------Variable Equality--------
//Logic for equal variables in cmd/err

//Given an expression that had previously been a constant, determine whether to promote it to VarEq or Var
//cVal: the value of the constant to be promoted
//newVal: the value just seen
//pos: the position of the constant to be promoted
//newVarMap: a map from (constVal, newVal) to variable index: If a constant further to the right in the expression,
//with the value constVal was promoted to Var ON THIS iteration of synthesys upon seeing newVal, it is mapped to by (constVal, newVal)
//prevTotExamples: the number of examples seen - used to 'reconstitute' varAss
//we can promote a constant in one of two ways. If a former constant (further to the right), having value cVal, was promoted to a Var(i,...) upon seeing newVal on this 
//iteration of synthesis , we promote our constant to a VarEq(i,...)
//If no such former constant exists, we promote this constant to a Var(i,...)
let promoteConst newVal cVal pos newVarMap prevTotExamples= 
    let oldVarAss = nConstList cVal prevTotExamples
    let varAss = newVal :: oldVarAss in
    match Map.tryFind (cVal, newVal) newVarMap with  
    | Some idx -> (VarEq(idx, varAss), varAss, newVarMap, false)
    | None -> let (prefix, suffix) = getSharedPrefAndSuf newVal cVal in
              let promoted = Var(pos, prefix, suffix, varAss) in
              let updatedNewVarMap = Map.add (cVal, newVal) pos newVarMap in
              (promoted, varAss, updatedNewVarMap, true)

//given a varEq(i...), determine whether to keep it unchanged, update it to a varEq(j,...) or a Var(k,...)
//truePos: the actual position in the input of the current expression
//eqPos: the position of the variable to which the current varEq was equivalent to
//(newVal::prevVals): the previous values at the varEq(i,...)
//exprList: the list of all expressions to the right of truePos
//varMap: a map from (oldIndex, currAssignment) to newIndex.
//Don't worry about incomplete pattern match on list destructor in params
//If we have a varEq with only one value in VarAss, we voiolated the invariants somewhere
let promoteVarEq truePos eqPos (newVal::prevVals) exprList varMap =
    let varAss = newVal :: prevVals in
    match Map.tryFind (eqPos, newVal) varMap with
    | Some idx -> (VarEq(idx, varAss), varAss, varMap, false)
    | None -> let prefSufAtPos = (fun pos expr ->
                                      match expr with
                                      | Var(pos, pref, suf, _) -> Some (pref, suf)
                                      | _ -> None) in
              let (pref, suf) = getListPrefAndSuf varAss
              let promoted = Var(truePos, pref, suf, varAss) in
              let updatedVarMap = Map.add (eqPos,newVal) truePos varMap in 
              (promoted, varAss, updatedVarMap, true)                                                      
                 
// --------- PATTERN ALIGNMENT --------------------

// Given a pattern (Expr list) of a rule and a new example return the new pattern that accomodates the new example
// by adding new variables if necessary
// The new added variables have flag True in the environment so that later on we reduce the total recomputation

let rec matchExprListWithSymStr (ruleMatch:Expr list) (cmd:SymbString) (currIndex:int) (prevTotExamples:int) baseNewVarMap baseVarMap: Expr list *  (int * (string list) * bool) list * Map<(string * string), int> * Map<(int * string), int> = 
    match (ruleMatch, cmd) with
    | ([],[]) -> ([], [], baseNewVarMap, baseVarMap)
    | (ConstStr(str1)::t1, str2::t2) ->  
                    let (cmdMatch,env, newVarMap, varMap) = matchExprListWithSymStr t1 t2 (currIndex+1) prevTotExamples baseNewVarMap baseVarMap in
                    if(str1.Equals(str2)) then                        
                        (ConstStr(str1)::cmdMatch,env, newVarMap, varMap)
                    else
                        // create a new variable if string is diff from constant
                        let (promoted, varAss, updatedNewVarMap, isNewVar) = promoteConst str2 str1 currIndex newVarMap prevTotExamples in
                        let updatedEnv = (match isNewVar with
                                                | true -> (currIndex,varAss,true)::env
                                                | false -> env) in
                        (promoted::cmdMatch, updatedEnv, updatedNewVarMap, varMap)  // True indicates that this is a new variable. We'll treat it differently later
    | (VarEq(id, strlist) :: t1, str2::t2) ->
                     let (cmdMatch, env, newVarMap, varMap) = matchExprListWithSymStr t1 t2 (currIndex+1) prevTotExamples baseNewVarMap baseVarMap in
                     let (promoted, varAss, updatedVarMap, isNewVar) = promoteVarEq currIndex id (str2::strlist) t1 varMap in
                     let updatedEnv = (match isNewVar with
                                             | true -> (currIndex, varAss, true)::env
                                             | false -> env ) in
                     (promoted::cmdMatch, updatedEnv , newVarMap, updatedVarMap)
    | (Var(id, prefix, suffix, strlist)::t1, str2::t2) -> 
                        let (cmdMatch,env, newVarMap, varMap) = matchExprListWithSymStr t1 t2 (currIndex+1) prevTotExamples baseNewVarMap baseVarMap in
                        let (commonPrefix, commonSuffix) = getUpdatedPrefixAndSuffix str2 prefix suffix                   
                        let varAss = str2::strlist in
                           (Var(id, commonPrefix, commonSuffix, varAss)::cmdMatch, (id,varAss, false)::env, newVarMap, varMap)
    | _ -> raise (System.ArgumentException("Lists don't have the same length"))

// --------- SYNTHESIS FOR POSITIONS --------------------

// finds all the Sympos that match the index-th character and applies an offset
let syntSymPosWithOffset index (str:char list) offset = 
    if(index<0 || index>=str.Length) then
        []
    else
        let c = List.nth str index in
        let matches = findMatches c str in
        let matchIndex = List.findIndex (fun x -> x=index) matches in
        [SymPos(c,matchIndex+1,offset); SymPos(c,matchIndex-matches.Length,offset)]

// synthesize all the SymPos for the position index by allowing offset 0 -1 +1
let syntSymPositions index str : Pos list = 
    let pos0 = syntSymPosWithOffset index str 0 in // offset 0
    let posp1 = syntSymPosWithOffset (index+1) str (-1) in // offset +1
    let posm1 = syntSymPosWithOffset (index-1) str (+1) in // offset -1    
        pos0 @ posp1 @ posm1 

// all the Pos (both consts and symbolic) expression matching index in str
let posOfIndex (index:int) (str:string) : Pos list = 
    CPos(index) :: CPos(-(str.Length-index)) :: syntSymPositions index (convertToCharSeq str)

// --------- SYNTHESIS FOR SUBSTRINGS --------------------

// all the pairs of positions (i,j) such that orig[i,j]=sub and i>=from
let rec subStringIndices (sub:string) (orig:string) (from:int) : List<Pos*Pos> =
  let index = orig.IndexOf(sub, from)
  if index >= 0 then     
    let rest = subStringIndices sub orig (index + 1) in
    let lPositions = posOfIndex index orig in
    let rPositions = posOfIndex (index+sub.Length) orig in
    List.append (cartesian lPositions rPositions) rest    
  else []

// Projects first 2 strings in context                
let rec projStringMap (env:(int*(string list)*bool) list) (strIndex:int) : Map<int,string> = 
    match env with
    | [] -> Map.empty
    | (i, l ,_) :: t -> let map = projStringMap t strIndex in
                        map.Add(i,List.nth l strIndex)


let rec filterSingleSubstrPositions (l: (Pos * Pos) list) (inpStr:string) (outStr:string) : (Pos * Pos) list= 
    match l with
    | []    -> []
    | (i,j)::t  ->   
                let remainder = filterSingleSubstrPositions t inpStr outStr in
                try
                    let iMatch = evalPos i inpStr in 
                    if(iMatch.IsSome) then
                        let jMatch = evalPos j inpStr in
                        if(jMatch.IsSome) then                            
                            let expSubLength = evalSubstrLength iMatch.Value jMatch.Value inpStr in
                            if(expSubLength = outStr.Length) then
                                 let substringOpt = evalSubstr iMatch.Value jMatch.Value inpStr in
                                    match substringOpt with 
                                    | Some substring -> if(substring.Equals(outStr)) then
                                                            (i,j):: remainder
                                                        else
                                                            remainder
                                    | None -> remainder
                            else
                                remainder
                        else 
                            remainder
                    else 
                        remainder
                with 
                    | :? System.ArgumentOutOfRangeException -> remainder
                    | :? System.ArgumentException ->  remainder



// the list of (Pos * Pos * string * string) in l that when applied to inpStr produce outStr
let rec filterSingleSubstrFuns (l: ((Pos * Pos) list  * string * string) list) (inpStr:string) (outStr:string) : ((Pos * Pos) list * string * string) list= 
    match l with
    | []    -> []
    | (pairPosList,l,r)::t  ->                      
            let remainder = filterSingleSubstrFuns t inpStr outStr in
            if  outStr.StartsWith(l) && outStr.EndsWith(r) then 
                try 
                    let outSubstr = outStr.Substring(l.Length,outStr.Length-l.Length-r.Length) in
                    match filterSingleSubstrPositions pairPosList inpStr outSubstr with
                    | [] -> remainder
                    | filteredList -> (filteredList,l,r):: remainder
                with 
                    | :? System.ArgumentOutOfRangeException -> remainder
                    | :? System.ArgumentException ->  remainder
            else 
                remainder


// the list of (Pos * Pos * string * string) in l that when applied to inpStr produce outStr
let rec filterByApplyingToSingleStr (outString:string) (funs: (Fun * int) list) (env:Map<int,string>) = 
    match funs with
    | [] -> []
    | (SubstrAndAppend(l),varId)::t -> 
                            let filtered = filterSingleSubstrFuns l (env.Item varId) outString in
                            let rest = filterByApplyingToSingleStr outString t env in
                            match filtered with
                            | [] ->  rest
                            | _ -> (SubstrAndAppend(filtered),varId)::rest

// the list of Fun*int that when applied to all input strings produce the correct output strings
let rec filterByApplyingToStrs strs funs (env:(int*(string list)*bool) list) strIndex= 
    match strs with
    | [] -> funs
    | str::t -> let funs1 = filterByApplyingToSingleStr str funs (projStringMap env strIndex) in
                filterByApplyingToStrs t funs1 env (strIndex+1)


// --- USING ONE EXAMPLE---

// all the pairs of positions (i,j) such that orig[i,j]=sub
let syntSubstringIndices1Example (out1:string) (inp1:string) : Set<Pos*Pos> = 
    let out1Indices = subStringIndices out1 inp1 0
    Set.ofList out1Indices

// Projects first string in context                
let rec proj1string (env:(int*(string list)*bool) list) : (int*string) list=
    match env with
    | [] -> []
    | (i,s1 :: _ ,_) :: t -> (i,s1):: proj1string t
    | _  -> raise (System.ArgumentException("Should only be called on lists of length > 2"))


// For each variable find all possible function instantiations
let rec syntSubstrings1Example (str1:string) (env:(int*string) list) prefLength suffLength: (Fun * int) list= 
        // TODO: There needs to be a basic check on the lenghts first to avoid unnecessary enum
        match env with
        | [] -> []
        | (varId,h1)::tail -> 
                let substringExpressions = seq {
                                for begPos in 0 .. prefLength do
                                    let prefix = str1.Substring(0,begPos) in                                    
                                    for endPos in 0 .. suffLength do
                                        let sub1Length = str1.Length-begPos-endPos in
                                        if(sub1Length>0) then
                                            let sub1 = str1.Substring(begPos, str1.Length-begPos-endPos) in                                            
                                            if(sub1.Length>0) then
                                                let indices = syntSubstringIndices1Example sub1 h1 in
                                                if not(indices.IsEmpty) then
                                                    let suffix = str1.Substring(str1.Length-endPos,endPos) in
                                                        yield (List.ofSeq indices ,prefix,suffix)
                            } in
                match List.ofSeq substringExpressions with 
                | [] -> syntSubstrings1Example str1 tail prefLength suffLength 
                | l -> (SubstrAndAppend(l),varId) :: syntSubstrings1Example str1 tail prefLength suffLength      


// --- USING TWO EXAMPLES---

// all the pairs of positions (i,j) such that orig[i,j]=sub
let syntSubstringIndices2Examples (out1:string) (out2:string) (inp1:string) (inp2:string) : Set<Pos*Pos> = 
    let out1Indices = subStringIndices out1 inp1 0
    let out2Indices = subStringIndices out2 inp2 0
    let set1 = Set.ofList out1Indices
    let set2 = Set.ofList out2Indices
    let intersectionSet = Set.intersect set1 set2
    intersectionSet


// For each variable find all possible function instantiations
let rec syntSubstrings2Examples (str1:string) (str2:string) (env:(int*string*string) list) prefLength suffLength: (Fun * int) list= 
        // TODO: There needs to be a basic check on the lenghts first to avoid unnecessary enum
        match env with
        | [] -> []
        | (varId,h1,h2)::tail -> 
                let substringExpressions = seq {
                                for begPos in 0 .. prefLength do
                                    let prefix = str1.Substring(0,begPos) in                                    
                                    for endPos in 0 .. suffLength do
                                        let sub1Length = str1.Length-begPos-endPos in
                                        let sub2Length = str2.Length-begPos-endPos in
                                        if(sub1Length>0 && sub2Length>0) then
                                            let sub1 = str1.Substring(begPos, str1.Length-begPos-endPos) in
                                            let sub2 = str2.Substring(begPos, str2.Length-begPos-endPos) in
                                            if(sub1.Length>0 && sub2.Length>0) then
                                                let indices = syntSubstringIndices2Examples sub1 sub2 h1 h2 in
                                                if not(indices.IsEmpty) then
                                                    let suffix = str1.Substring(str1.Length-endPos,endPos) in                                                    
                                                        yield (List.ofSeq indices,prefix,suffix)
                            } in
                match List.ofSeq substringExpressions with 
                | [] -> syntSubstrings2Examples str1 str2 tail prefLength suffLength 
                | l -> (SubstrAndAppend(l),varId) :: syntSubstrings2Examples str1 str2 tail prefLength suffLength      



// Projects first 2 strings in context                
let rec proj2strings (env:(int*(string list)*bool) list) : (int*string*string) list=
    match env with
    | [] -> []
    | (i,s1 :: s2 :: _ ,_) :: t -> (i,s1,s2):: proj2strings t
    | _  -> raise (System.ArgumentException("Should only be called on lists of length > 2"))

// Returns a FixFuncApp that is consistent with the the two strings
// with respect to some variable in the input
let rec syntFuncApp str1 str2 (env: (int * string * string) list): FixExpr option = 
        // find longest common prefix and longest common suffix
        let charSeq1 = convertToCharSeq str1 in 
        let charSeq2 = convertToCharSeq str2 in
        let prefLength = (longestCommonPrefix charSeq1 charSeq2).Length in
        let suffLength = (longestCommonSuffix charSeq1 charSeq2).Length in
        //Now look for all substrings from (i,j) such that i is before the prefix and j is after the suffix (inclusive)
        match syntSubstrings2Examples str1 str2 env prefLength suffLength with
        | [] -> None
        | l -> Some(FixFuncApp(l,[str1;str2]))


// ----GENERAL METHODS FOR THAT DON'T DEPEND ON NUMBER OF EXAMPLES

// This should only be used when we discover new functions
// the list of funs that are consistent with all the output strings and the environemnts
let syntSubstrsFromMany (strs: string list) (env:(int*(string list)*bool) list) prefLength suffLength: (Fun * int) list = // solve for a given variable
        match strs with
        | s1 :: s2 :: t -> 
                let funsFors1s2 = syntSubstrings2Examples s1 s2 (proj2strings env) prefLength suffLength in
                let funsForStrs = filterByApplyingToStrs t funsFors1s2 env 2 in //Start at index two in the env
                funsForStrs
        | _  -> raise (System.ArgumentException("Should only be called on lists of length > 2"))                   
               

                   
let rec orderedMerge (l1:(Fun*int) list) (l2:(Fun*int) list) = 
    match (l1,l2) with
    | (_,[]) -> l1
    | ([],_) -> l2
    | ((f1,i1)::t1,(f2,i2)::t2) -> 
                if(i1<i2) then 
                    (f1,i1) :: orderedMerge t1 l2
                else
                    (f2,i2) :: orderedMerge l1 t2

// This should only be used when we discover new functions
// the list of funs that are consistent with all the output strings and the environemnts
let rec syntSubstrsFromManyFromStartFuns (strs: string list) (alreadyComputedFuns: (Fun*int) list)
                 (env:(int*(string list)*bool) list) prefLength suffLength: 
                            (Fun * int) list = 
        if(alreadyComputedFuns.IsEmpty) then
            match strs with
            // Un/comment to enable two strings matching
            | s1 :: s2 :: t -> 
                    let funsFors1s2 = syntSubstrings2Examples s1 s2 (proj2strings env) prefLength suffLength in
                    let funsForStrs = filterByApplyingToStrs t funsFors1s2 env 2 in //Start at index two in the env
                    funsForStrs
            
            | s1 :: t -> 
                    let funsFors1 = syntSubstrings1Example s1 (proj1string env) prefLength suffLength in
                    let funsForStrs = filterByApplyingToStrs t funsFors1 env 1 in //Start at index two in the env
                    funsForStrs

            | _  -> raise (System.ArgumentException("Should only be called on lists of length > 2"))    
        else
            let filteredOldFuns = filterByApplyingToStrs [(List.nth strs 0)]  alreadyComputedFuns env 0 in //TODO they should be in better order so that I can avoid a few
            let newFuns = syntSubstrsFromManyFromStartFuns strs [] (List.filter (fun (_,_,b)->b) env) prefLength suffLength in //Compute for all vars we added
              orderedMerge filteredOldFuns newFuns // TODO
                               
// Returns a FixFuncApp that is consistent with the the two strings with respect to some variable in the input
// It avoids recomputing functions that have alredy been computed
let rec syntFuncAppManyStringsFromStartFuns (strs: string list) (alreadyComputedFuns: (Fun*int) list) (env: (int * (string list) * bool) list): FixExpr option = 
        // find longest common prefix and longest common suffix
        let (lpref,lsuf) = findLongestCommonPrefSuf strs in 
        //Now look for all substrings from (i,j) such that i is before the prefix and j is after the suffix (inclusive)
        match syntSubstrsFromManyFromStartFuns strs alreadyComputedFuns env lpref.Length lsuf.Length with //TODO change
        | [] -> None
        | l -> Some(FixFuncApp(l,strs))

// Returns a FixFuncApp that is consistent with the the two strings
// with respect to some variable in the input
let syntFuncAppManyStrings (strs: string list) (env: (int * (string list) * bool) list): FixExpr option = 
        syntFuncAppManyStringsFromStartFuns strs [] env

// ----------- SYNTHESIS OF OUTPUT COMMANDS ------------
                                                          
// returns a FixEpxr list consistent with the two outputs
let rec matchOutputCmds (cmd1:SymbString) (cmd2:SymbString) (env: (int * string * string) list): FixExpr list option = 
    match (cmd1, cmd2) with
    | ([],[]) -> Some []
    | (x1::t1, x2::t2) ->      
            option{                         
                let! head = 
                    if(x1.Equals(x2)) then 
                        Some(FixConstStr(x1))
                    else 
                        syntFuncApp x1 x2 env                   
                in 
                let! cmdMatch = matchOutputCmds t1 t2 env in 
                return head::cmdMatch     
            }           
    | _ -> raise (System.ArgumentException("Lists don't have the same length"))

let rec removeFlag (env: (int * (string list) * bool) list) =
    match env with
    | [] -> []
    | (i,l,b)::t -> (i,l)::(removeFlag t)

// returns a FixEpxr list consistent with the outputs
let rec matchOutputCmdWithNewExample (fix:FixExpr list) (newCmd:SymbString) (env: (int * (string list) * bool) list) (prevTotExamples: int): FixExpr list option = 
    match (fix, newCmd) with
    | ([],[]) -> Some []
    | (FixConstStr(str1)::t1, str2::t2) -> 
              option{   
                    let! head =
                        if(str1.Equals(str2)) then                        
                            Some(FixConstStr(str1))
                        else
                            //We need to the func apps, something in the output is now a variable while before it was supposed to be a constant
                            syntFuncAppManyStrings (str2::(nConstList str1 prevTotExamples)) env
                    in 
                    let! cmdMatch = matchOutputCmdWithNewExample t1 t2 env prevTotExamples in 
                    return head::cmdMatch 
              }
    | (FixFuncApp(funs, strs)::t1, str2::t2) -> 
            let x = "" in
            //let headTest =  syntFuncAppManyStringsFromStartFuns (str2::strs) funs env in
            //let cmdMatchTest = matchOutputCmdWithNewExample t1 t2 env prevTotExamples in 
            option{      
                // TODO pass already computed funs and already computer vars                 
                let! head = syntFuncAppManyStringsFromStartFuns (str2::strs) funs env in // pass funs
                let! cmdMatch = matchOutputCmdWithNewExample t1 t2 env prevTotExamples in 
                return head::cmdMatch     
            }           
    | _ -> raise (System.ArgumentException("Lists don't have the same length"))

// Refines a rule by trying to add one new example (cmd, err, fix). None if the example cannot be added
let addNewExampleToRule (cmd:SymbString) (err:SymbString) (fix:SymbString)
                    (rule: TopLevelExpr)
                      : TopLevelExpr option = 
    match rule with
    FixRule(CmdParams(c),ErrContent(e),FixCmdParams(f), totExamples)->
        //Check first that each part of the rule has the correct length
        if (cmd.Length = c.Length && err.Length=e.Length && f.Length=fix.Length) then
            let (errExpr, env1, errNewVarMap, errVarMap) = matchExprListWithSymStr e err cmd.Length totExamples Map.empty Map.empty in 
            let (cmdExpr, env, _, _) = matchExprListWithSymStr c cmd 0 totExamples errNewVarMap errVarMap in
            option{ 
                let! newFixExpr = matchOutputCmdWithNewExample f fix (env @ env1) totExamples in
                return FixRule(CmdParams(cmdExpr),ErrContent(errExpr),FixCmdParams(newFixExpr), totExamples+1) 
            } 
        else
            None

// --- GENERATES INITIAL CONSTANT RULE FROM ONE EXAMPLE----

// Given a rule, refine it so that it works on the examples in l
let rec addExamplesToRule (l:(SymbString*SymbString*SymbString) list) (r:TopLevelExpr):TopLevelExpr option =
    match l with 
    | [] -> Some(r)
    | (c,e,f)::t -> 
            match addNewExampleToRule c e f r with
            | None -> None
            | Some(r1)-> addExamplesToRule t r1
            
// Rules for single example, always return the constant rule
let rec constRuleExprList symstr: Expr list  = 
    match symstr with
    | [] -> []
    | x1::t1 -> ConstStr(x1) :: constRuleExprList t1

let rec constRuleFixExprList symstr: FixExpr list  = 
    match symstr with
    | [] -> []
    | x1::t1 -> FixConstStr(x1) :: constRuleFixExprList t1

let constRule cmd err fix = FixRule(CmdParams(constRuleExprList cmd), ErrContent(constRuleExprList err), FixCmdParams(constRuleFixExprList fix), 1)

// --- MAIN CALL TO SYNTHESIZER FOR MANY EXAMPLES ----
//Multirule synthesis
type ExampleTuple = SymbString * SymbString * SymbString



// Synthesizes a rule from many examples
let synthesizeRuleFromListOfExamples (l:ExampleTuple list)
                      : TopLevelExpr option = 
    match l with
    | [] -> None
    | (c1,e1,f1)::t -> 
            let r = constRule c1 e1 f1 in
            addExamplesToRule t r

let getLengths (example: ExampleTuple) = match example with (cmd, err, fix) -> (cmd.Length, err.Length, fix.Length)

// A basic implementation - will go back and optimize if necessary.           
let synthesizeMultiRule synthAlg (l: ExampleTuple list) = List.toSeq l |>  (Seq.groupBy getLengths) |> Seq.map (fun (k,v) -> synthAlg synthesizeRuleFromListOfExamples (Seq.toList v)) |> Seq.toList

// Reserves an example to be held back as a test 
let synthesizeMultiRuleWithTest synthAlg (l: ExampleTuple list) = List.toSeq l |>  (Seq.groupBy getLengths) |> Seq.map (fun (k,v) -> (Seq.head v, Seq.skip 1 v, Seq.skip 1 v |> Seq.toList)) |> Seq.map (fun (test,learn,lst) -> (test, synthAlg (Seq.toList learn), lst)) |> Seq.toList

let getTrainingSets (l: ExampleTuple list list) = List.map (List.tail) l

let getTestCases (l: ExampleTuple list list) = List.map (List.head) l

let groupExamplesByLength (l: ExampleTuple list) = List.toSeq l |> (Seq.groupBy getLengths) |> Seq.map snd |> Seq.map Seq.toList |> Seq.toList

let SymbStringToString (l : string list) = String.concat " " l


//Given a partition, attempt to synthesize a rule for each subset. Returns some list of rules
// if a rule is synthesized for each subset. Returns None if at least one subset does not have
//a consistent rule
let partitionSynth synthalg (partition : Map<int, ExampleTuple List>, status: string) : ((TopLevelExpr List) * string) Option =
//    let ignored = printfn "%s" status |> ignore in
    let resultAccFxn acc _ (subset : ExampleTuple List) =
        let resultRule = synthalg subset in
        match resultRule with
        | None -> None
        | Some r -> match acc with
                    | None -> None
                    | Some l -> Some (r :: l) in
    let res = Map.fold resultAccFxn (Some []) partition in
    match res with
          | None -> None
          | Some r -> Some (r, status)

   
//Find the first partition for which a rule is synthesized for each subset.
//This should never throw KeyNotFoundException, since the one-element partitioning should
//yield the constant rule for each element
let findOptPartitionInternal synthalg (partitions : seq<Map<int, ExampleTuple List> * string>) : (TopLevelExpr List * string) =
 //     printfn "example set"
      let (res, str) = Seq.pick (partitionSynth synthalg) partitions in
 //     let ignored = printfn "%s" str |> ignore in
          (res, str)
            

//Find the optimal partitioning of an example list
let findOptPartition synthalg exampleList =
    let partitionSeq = enumerateSetListPartitions exampleList in
    findOptPartitionInternal synthalg partitionSeq

let scaleGroup scaling group =
    let repeatedGroup = List.replicate 6 group |> List.concat
    (Seq.take scaling repeatedGroup) |> Seq.toList |> (List.append group)

let scaleGroups groupedExamples scaling =
    List.map (scaleGroup scaling) groupedExamples
    
//Given an undifferentiated set of examples, find the optimial partition of each set of examples of a given length
let multirulePartitioning synthalg (l: ExampleTuple List) scaling  =
    let groupedExamples = groupExamplesByLength l in  
    let scaledExamples = scaleGroups groupedExamples scaling in
    //let filteredExamples = scaledExamples |> List.filter (fun ((_,_,fix)::_) -> (String.concat " " fix) = "git stash" ) in
    Seq.map (findOptPartition synthalg) scaledExamples
    //Seq.map (findOptPartition synthalg) filteredExamples

//Given a sequence of rules and an example e, attempt to add e to each rule
let addToRulesInList rules cmd err fix =
    List.toSeq rules 
    |> Seq.map (addNewExampleToRule cmd err fix) 
    |> Seq.filter (fun x -> x <> None)
    |> Seq.map (fun (Some r) -> r) //incomplete pattern match. Not a problem. We know it will work.
    |> Seq.toList

let buildRuleList examples singleRules =
    let extendRuleSet ruleList (cmd, err, fix) =
        List.append (addToRulesInList ruleList cmd err fix) ruleList in
    List.fold extendRuleSet singleRules examples

let makeConstRuleList examples =
    List.map (fun (cmd, err, fix) -> constRule cmd err fix) examples

// Just for experiments BAD
let rec envOf symstr: (int*string) list  = 
    match symstr with
    | [] -> []
    | Var(i,_,_,[x1])::t1 -> (i,x1) :: envOf t1
    | _ -> raise (System.ArgumentException("Can't happen"))

// Rules for single example, using only vars
let rec varRuleExprList symstr i: Expr list  = 
    match symstr with
    | [] -> []
    | x1::t1 -> Var(i,x1,x1,[x1]) :: varRuleExprList t1 (i+1)

let rec varRuleFixExprListOld symstr env: FixExpr list  = 
    match symstr with
    | [] -> []
    | x1::t1 -> FixFuncApp(syntSubstrings1Example x1 env (String.length x1) (String.length x1),[x1]) :: (varRuleFixExprListOld t1 env)

let rec varRuleFixExprListNew symstr env: FixExpr list  = 
    match symstr with
    | [] -> []
    | x1::t1 -> let substrFixFxn = syntSubstrings1Example x1 env (String.length x1) (String.length x1) in
                match substrFixFxn with
                | [] -> FixConstStr(x1) :: (varRuleFixExprListNew t1 env)
                | _ -> FixFuncApp(substrFixFxn, [x1]) :: (varRuleFixExprListNew t1 env)

let varRuleOld cmd err fix = 
    let ce = varRuleExprList cmd 0 in
    let ee = varRuleExprList err cmd.Length in
    let env = envOf (List.append ce ee) in
    FixRule(CmdParams(ce), ErrContent(ee), FixCmdParams(varRuleFixExprListOld fix env), 1)

let varRuleNew cmd err fix = 
    let ce = varRuleExprList cmd 0 in
    let ee = varRuleExprList err cmd.Length in
    let env = envOf (List.append ce ee) in
    FixRule(CmdParams(ce), ErrContent(ee), FixCmdParams(varRuleFixExprListNew fix env), 1)

// Synthesizes a rule from many examples
let VARIABLEsynthesizeRuleFromListOfExamples (l:ExampleTuple list)
                      : TopLevelExpr option = 
    match l with
    | [] -> None
    | (c1,e1,f1)::t -> 
            let r = varRuleNew c1 e1 f1 in
            addExamplesToRule t r