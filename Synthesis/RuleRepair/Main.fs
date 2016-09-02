module Main

open System
open FParsec
open Parser
open RuleEvaluator
open RuleSynthesizer
open FSharp.Data
open TestExamples
open System.IO
open RRUtilities
open database
open Nessos.FsPickler.Combinators



// cd {1}, "no such file or directory" -> mkdir {1}; cd {1}
// java {1}, "could not find or locate class" --> java substr(1,-4, {1})
//let r = FixRule(CmdParams([ConstStr("java"); Var(0)]), 
//                ErrContent (strToConstStr "could not find or locate class"), 
//                FixCmdParams([FixConstStr("java"); FixFuncApp([SubstrAndAppend([(CPos(0),CPos(-5),"","")]),0]) ]))


let test p str = 
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


let testEval rule = 
    match (evalTopLevelExpr rule (strToSymbString "java abc.java") 
                (strToSymbString "asdf could not find or locate class asf")) with 
        | Some(s) -> s |> List.iter (fun x-> printf "%s " x)
        | None -> printfn "Didn't match"

//testEval r

// cd {0}, "no such file or directory" -> mkdir {0}; cd {0}
// let testStr = "{cmd: cd {0}, err: no such file or directory, fix: mkdir SubStrFrom(0,{0})}"
// test TopLevelExprParser testStr

// java {1}, "could not find or locate class" --> java substr(0,-5, {0})
let testStr1 = "{cmd: java {0}, \n err: could not find or locate class, fix: java SubStrFromTo(0,-5,{0})}"
// test TopLevelExprParser testStr1

//let getAst str =
//    match run TopLevelExprParser str with
//    | Success(result, _, _) -> result


let addLine (line:string) =     
  use wr = new StreamWriter("results.csv", true)
  wr.WriteLine(line)
  wr.Close()
 
let rec CountConstExpr (exprs: Expr list) c: int =
  match exprs with
  | x::xs ->   
        match x with
        | ConstStr(_) -> CountConstExpr xs (c+1)
        | _ -> CountConstExpr xs c
  | [] -> c

let rec CountVarExpr (exprs: Expr list) c: int =
  match exprs with
  | x::xs ->   
        match x with
        | Var(_,_,_,_) -> CountVarExpr xs (c+1)
        | _ -> CountVarExpr xs c
  | [] -> c
let rec CountFixConstExpr (exprs: FixExpr list) c: int =
  match exprs with
  | x::xs ->   
        match x with
        | FixConstStr(_) -> CountFixConstExpr xs (c+1)
        | _ -> CountFixConstExpr xs c
  | [] -> c

let rec CountFixVarExpr (exprs: FixExpr list) c: int =
  match exprs with
  | x::xs ->   
        match x with
        | FixFuncApp(_,_) -> CountFixVarExpr xs (c+1)
        | _ -> CountFixVarExpr xs c
  | [] -> c

let CountConstTopLevel (fix:TopLevelExpr) =
    match fix with
    | FixRule(CmdParams(cmd), ErrContent(err), FixCmdParams(fix), _) -> (CountConstExpr cmd 0 + (CountConstExpr err 0)).ToString()
    
let CountVarTopLevel (fix:TopLevelExpr) =
    match fix with
    | FixRule(CmdParams(cmd), ErrContent(err), FixCmdParams(fix), _) -> (CountVarExpr cmd 0 + (CountVarExpr err 0)).ToString()

let CountFixVarTopLevel (fix:TopLevelExpr) =
    match fix with
    | FixRule(CmdParams(cmd), ErrContent(err), FixCmdParams(fix), _) -> (CountFixVarExpr fix 0).ToString()

let CountFixConstTopLevel (fix:TopLevelExpr) =
    match fix with
    | FixRule(CmdParams(cmd), ErrContent(err), FixCmdParams(fix), _) -> (CountFixConstExpr fix 0).ToString()


type TestDataJsonRule = JsonProvider<""" [{"ex1": {"cmd": "ab cd", "err" : "slk lsk slk", "fix": "lkl ssiw"},
"ex2": {"cmd": "ab cd", "err" : "slk lsk slk", "fix": "lkl ssiw"},
"ex3": {"cmd": "ab cd", "err" : "slk lsk slk", "fix": "lkl ssiw"}}] """>


type NewTestDataJsonRule = JsonProvider<""" [ [ {"cmd": "ab cd", "err": "slk lsk slk", "fix": "lkl ssiw"}] ] """>
let NewParseTestData = NewTestDataJsonRule.Parse(testdata)
let varEqTestData = NewTestDataJsonRule.Parse(varEqTestData)
let mutable passingTests = 0
let mutable failingTests = 0

let runSingleInstance cmdEx1 errEx1 fixEx1 cmdEx2 errEx2 fixEx2 cmdTest errTest fixTest =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() in
    stopWatch.Restart()
    let exRule = synthesizeRuleFromListOfExamples [(cmdEx1, errEx1, fixEx1); (cmdEx2, errEx2, fixEx2)] in
    for i in 1..10 do
        synthesizeRuleFromListOfExamples [(cmdEx1, errEx1, fixEx1); (cmdEx2, errEx2, fixEx2)]
    let fixTestStr = fixTest |> List.fold (fun acc y -> (acc+" "+y)) "" in
    match exRule with
    | Some(exRuleValue) ->
        stopWatch.Stop()
        let TestRule = evalTopLevelExpr exRuleValue cmdTest errTest in
        match TestRule with
        | Some(testOutput) ->
            let testOutputStr = testOutput |> List.fold (fun acc y -> (acc+" "+y)) "" in
            if (testOutputStr.Equals(fixTestStr)) then 
                passingTests <- passingTests+1 
                let rtime = stopWatch.Elapsed.TotalMilliseconds/11.0 in
                let stats = rtime.ToString() + ", " + cmdEx1.Length.ToString()+ ", " + errEx1.Length.ToString() + ", " + fixEx1.Length.ToString() + ", " + CountConstTopLevel exRuleValue + ", " + CountVarTopLevel exRuleValue + ", " + CountFixConstTopLevel exRuleValue + ", " + CountFixVarTopLevel exRuleValue in
                let stats1 = "time: " + rtime.ToString() + ", sizecmdEx1: " + cmdEx1.Length.ToString()+ ", sizeerrEx1: " + errEx1.Length.ToString() + ", sizefixEx1: " + fixEx1.Length.ToString() + ", sizecmdEx2: " + cmdEx2.Length.ToString() + ", sizeerrEx2: " + errEx2.Length.ToString() + ", sizefixEx2: " + fixEx2.Length.ToString() + ", sizecmdTest: " + cmdTest.Length.ToString() + ", sizeerrEx2: " + errTest.Length.ToString() + ", sizefixEx3: " + fixTest.Length.ToString() + ", constsNum: " + CountConstTopLevel exRuleValue + ", varNum: " + CountVarTopLevel exRuleValue in
                addLine(stats)
                printfn "%s" stats1
                
            else
                failingTests <- failingTests + 1 
                printfn "Failed | Expected Output: %s Generated Output: %s" fixTestStr testOutputStr
        | None ->
            failingTests <- failingTests + 1 
            printfn "Failed | Expected Output: %s Generated Output: None" fixTestStr 
    | None ->
        failingTests <- failingTests + 1  
        printfn "Failed | No Rule synthesized. Expected Output: %s" fixTestStr    
    ;

let runMultiExample l =
    match synthesizeRuleFromListOfExamples l with
    | None -> failingTests <- failingTests+1; printfn "MultiTest Failed"
    | l -> passingTests <- passingTests+1; printfn "MultiTest Passed"
    


let mutable testId=0
let numberOfRepeats = 1 // How many times you want to repeat a test
let mutable numberOfSingleExp = 0

let rec concatenateList l i =
    if(i=0) then
        l
    else
        List.append l (concatenateList l (i-1))

let dumpExampleStats ((cmd,err,fix) : ExampleTuple)  = cmd.Length.ToString() + ", " + err.Length.ToString() + ", " + fix.Length.ToString() + ", "

let dumpRuleStats learnExamples testExample rule =
                  let exampleStats = testExample :: learnExamples |> List.fold (fun acc ex -> acc +  dumpExampleStats ex) "" in
                  exampleStats + "consts: " + CountConstTopLevel rule + ", vars: " + CountVarTopLevel rule


let testRuleOnExample ((cmdTest, errTest, fixTest), exRule, learnExamples : ExampleTuple list) (stopWatch : System.Diagnostics.Stopwatch) =
                        let fixTestStr = fixTest |> List.fold (fun acc y -> (acc+" "+y)) "" in
                        match exRule with
                        | Some(exRuleValue) ->
                              let TestRule = evalTopLevelExpr exRuleValue cmdTest errTest in                              
                              match TestRule with
                              | Some(testOutput) ->
                                    let testOutputStr = testOutput |> List.fold (fun acc y -> (acc+" "+y)) "" in
                                    if (testOutputStr.Equals(fixTestStr)) then
                                        passingTests <- passingTests+1
                                        let rtime = stopWatch.Elapsed.TotalMilliseconds/11.0 in
                                        let (cmdEx1, cmdErr1, cmdFix1) = learnExamples |> List.head
                                        let stats = rtime.ToString() + ", "  + cmdEx1.Length.ToString() + ", " + cmdErr1.Length.ToString() + ", " + cmdFix1.Length.ToString() + ", " + CountConstTopLevel exRuleValue + ", " + CountVarTopLevel exRuleValue + ", " + CountFixConstTopLevel exRuleValue + ", " + CountFixVarTopLevel exRuleValue 
                                        let stats1 = dumpRuleStats learnExamples (cmdTest, errTest, fixTest) exRuleValue       
                                        addLine(stats)
                                        printfn "%s" stats1                
                                    else
                                        failingTests <- failingTests + 1
                                        printfn "Failed | Expected Output: %s Generated Output: %s" fixTestStr testOutputStr
                              | None ->
                                   failingTests <- failingTests + 1 
                                   printfn "Failed | Expected Output: %s Generated Output: None" fixTestStr 
                          | None ->
                            failingTests <- failingTests + 1
                            printfn "Failed | No Rule synthesized. Expected Output: %s" fixTestStr ;  

//Ensure rule is not None before invocation
let testRuleOnExample2 (cmdTest, errTest, fixTest) (matchCount, success, matchFixes)  rule =
    let fixTestStr = SymbStringToString fixTest in
    let testResult = evalTopLevelExpr rule cmdTest errTest in
    match testResult with
    | Some(testOutput) -> 
        let testOutputStr = SymbStringToString testOutput in
        if testOutputStr.Equals(fixTestStr) then
            (matchCount + 1, true, testOutputStr :: matchFixes)
        else
            (matchCount + 1, false || success, testOutputStr :: matchFixes)
    | None -> (matchCount, false || success, matchFixes)

let checkTestAgainstRules ruleList testCase =
    List.fold (testRuleOnExample2 testCase) (0, false, []) ruleList

let checkTestCases testList ruleList =
    List.map (checkTestAgainstRules ruleList) testList

let countSuccesses l =
    List.filter (fun (_,sec,_) -> sec) l |> List.length

let getFailures ruleList testCases =
     List.filter (fun testCase -> checkTestAgainstRules ruleList testCase |> (fun (_,sec,_) -> sec) |> not) testCases

let countMultiMatches l =
    List.map (fun (first,_,_) -> first) l |> List.filter (fun v -> v > 1) |> List.length

let countSingleMatches l =
    List.map (fun (first,_,_) -> first) l |> List.filter (fun v -> v = 1) |> List.length

let testOneGroup alg exampleList = 
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() in
    stopWatch.Restart()
    let exRules = synthesizeMultiRuleWithTest alg exampleList
    for i in 1..10 do
        synthesizeMultiRuleWithTest alg exampleList
    stopWatch.Stop()
    for ruleTuple in exRules do
        testRuleOnExample ruleTuple stopWatch

let printStrList strList =
    printfn "------"
    strList |> List.iter (fun x -> printfn "%s\n" x)

let ruleWithErrSizeAtLeast size rule =
    let (FixRule (_, ErrContent l, _,_)) = rule in
    List.length l >= size

let cmdStartsWith str rule =
    let (FixRule ((CmdParams l),_,_,_)) = rule in
    match l.Head with
    | ConstStr hd -> str = hd
    | _ -> false


(*
//let synthAlg = VARIABLEsynthesizeRuleFromListOfExamples
let synthAlg = synthesizeRuleFromListOfExamples
let filteredSeq = NewParseTestData(* varEqTestData *) |> Array.toList |> List.rev |> List.toSeq |> Seq.skip 1
//List.filter (fun ((cmd,err,fix)::_) -> List.length cmd = 5 && List.length err = 4  && List.length fix = 6 )
let testCases = filteredSeq |> Seq.map Array.toList |> Seq.map (List.map (fun item -> (strToSymbString item.Cmd, strToSymbString item.Err, strToSymbString item.Fix))) |> Seq.map (List.rev) |>  Seq.toList |> (*List.filter (fun ((cmd,err,fix)::_) -> List.length cmd = 1 && List.length err = 6  && List.length fix = 1 )  |>*) getTestCases 
//let testCases = filteredSeq |> Seq.map Array.toList |> Seq.map (List.map (fun item -> (strToSymbString item.Cmd, strToSymbString item.Err, strToSymbString item.Fix))) |>  Seq.toList |> List.filter (fun ((_,_,fix)::_) -> (String.concat " " fix) = "git stash"  ) |> getTestCases 
//let trainingSet = filteredSeq |> Seq.map Array.toList |> Seq.map (List.map (fun item -> (strToSymbString item.Cmd, strToSymbString item.Err, strToSymbString item.Fix))) |> Seq.map (List.rev) |> Seq.toList  |> (*List.filter (fun ((cmd,err,fix)::_) -> List.length cmd = 1 && List.length err = 6  && List.length fix = 1 ) |>*) getTrainingSets |> List.concat
let trainingSet = filteredSeq |> Seq.map Array.toList |> Seq.map (List.map (fun item -> (strToSymbString item.Cmd, strToSymbString item.Err, strToSymbString item.Fix))) |> Seq.map (List.rev) |> Seq.toList  |> (*List.filter (fun ((cmd,err,fix)::_) -> List.length cmd = 1 && List.length err = 6  && List.length fix = 1 ) |>*) getTrainingSets
//let partResults = multirulePartitioning synthAlg trainingSet 3 |> Seq.map fst |> List.concatlet constlist = makeConstRuleList testCases
let rules = trainingSet 
            |> List.map synthesizeRuleFromListOfExamples
            |> List.filter Option.isSome
            |> List.map Option.get

let checkResults = checkTestCases testCases rules
let successes = countSuccesses checkResults
let multiMatches = countMultiMatches checkResults
let singleMatches = countSingleMatches checkResults
let failures = getFailures rules testCases
let cmd1 = strToSymbString "cd someCrapHuh"
let err1 = strToSymbString "no such"
let appropriateRules = rules |> List.filter (cmdStartsWith "cd") |> List.filter (ruleWithErrSizeAtLeast <| List.length err1)
let reslist = listMatches cmd1 err1 appropriateRules

List.iter printStrList reslist


printfn "rules synthesized: %d successes: %d, failures %d, multimatches: %d, single matches: %d" (List.length rules) successes (testCases.Length - successes) multiMatches singleMatches
*)

let caseTupleToStr (cmd :SymbString, err: SymbString, fix: string List) =
   [String.concat " " cmd; String.concat " " err; String.concat " " fix ] |> String.concat "\n"

//List.iter (fun x -> caseTupleToStr x |> printfn "----\n%s") failures

let rulePickler = Pickler.auto<TopLevelExpr>
let pickleRule r = Binary.pickle rulePickler r
let unpickleRule r = Binary.unpickle rulePickler r

let updateRuleInfoSeq cmd err fix rSeq =
    let tmp = rSeq |> Seq.map (fun (rule, exs) -> ((addNewExampleToRule cmd err fix rule), exs)) in
    let tmp2 = tmp |> Seq.filter (fun (r,_) -> r <> None) |> Seq.map (fun (Some r, exs) -> (r, exs)) in
    tmp2

let makeSingleDBRules (exSeq : (uint32 * string * string * string) seq) =
     exSeq 
     |> Seq.map (fun (id, c,e,f) -> (id, c.Split [|' '|] |> Array.toList, e.Split [|' '|] |> Array.toList, f.Split [|' '|] |> Array.toList))
     |> Seq.map (fun (id, cmd, err, fix) -> ((constRule cmd err fix), seq{ yield id }))

let getCmdName (FixRule(CmdParams cMatch, _, _,_) : TopLevelExpr) =
    let extractConstStr expr =
        match expr with
        | ConstStr str -> str
        | _ -> "" in
     match cMatch with
     | headExpr :: _ -> extractConstStr headExpr
     | _ -> "" 

let getKeywords rule =
    let (FixRule(_,ErrContent l,_,_)) = rule in
    let accFxn lst expr =
        match expr with
        | ConstStr s -> s :: lst
        | _ -> lst in
    List.fold accFxn [] l
 
let updateKeywordMapping ruleId rule =
    let keywords = getKeywords rule in
    List.iter (addKeywordMapping ruleId) keywords

let addExample (cmd, err, fix) = 
//    let ignored = printfn "add" in
    let cmdLen = uint32( List.length cmd ) in
    let errLen = uint32( List.length err ) in
    let fixLen = uint32( List.length fix ) in
    let dbSingles = makeSingleDBRules (getExamples |> Seq.toList |> List.toSeq) in
    let exID = addBashExample cmd err fix in
    let dbGroupRulesWithExamples = (getRulesAndExamples |> Seq.toList |> List.toSeq) |> Seq.map (fun (fp, exs) -> (unpickleRule fp, exs)) in
    let dbRulesWithExamples = Seq.concat (seq{ yield dbSingles ; yield dbGroupRulesWithExamples})
    let updatedExamples = dbRulesWithExamples |> updateRuleInfoSeq cmd err fix in 
    updatedExamples
    |> Seq.map (fun (r, exs) -> (addRuleFromIDs (pickleRule r) (getCmdName r) cmdLen errLen fixLen, r, exs ))
    |> Seq.map (fun (id, r, exs) -> (id, r, makeExampleSetFromIDs id exs))
    |> Seq.map (fun (id,r,_) -> (id, updateKeywordMapping id r))
    |> Seq.iter (fun (id,_) -> (addExampleToSet id exID) |> ignore)

let ignoreSingleRule exID exs =
    match exs with 
    | id::[] -> id <> exID
    | _ -> true
    

let addExistingExampleToRules (exID, cmd, err, fix) = 
//    let ignored = printfn "add" in
    let cmdLen = uint32( List.length cmd ) in
    let errLen = uint32( List.length err ) in
    let fixLen = uint32( List.length fix ) in
    let filterCurr (id, _, _, _) = id <> exID in
    let dbSingles = makeSingleDBRules (getExamples |> Seq.filter filterCurr |> Seq.toList |> List.toSeq) in
    let dbGroupRulesWithExamples = (getRulesAndExamples |> Seq.toList |> List.toSeq) |> Seq.map (fun (fp, exs) -> (unpickleRule fp, exs)) in
    let dbRulesWithExamples = Seq.concat (seq{ yield dbSingles; yield dbGroupRulesWithExamples})
    let updatedExamples = dbRulesWithExamples |> updateRuleInfoSeq cmd err fix in 
    updatedExamples
    |> Seq.filter (fun (_,exs) -> ignoreSingleRule exID (Seq.toList exs))
    |> Seq.map (fun (r, exs) -> (addRuleFromIDs (pickleRule r) (getCmdName r) cmdLen errLen fixLen, r, exs ))
    |> Seq.map (fun (id, r, exs) -> (id, r, makeExampleSetFromIDs id exs))
    |> Seq.map (fun (id, r, _) -> (id, updateKeywordMapping id r))
    |> Seq.iter (fun (id,_) -> (addExampleToSet id exID) |> ignore)

let simpleData = """[[
{"cmd": "heroku luck", "err": "!    `luck` is not a heroku command.
 !    Perhaps you meant `lock`.
 !    See `heroku help` for a list of available commands.", "fix": "heroku lock"},
{"cmd": "heroku ad", "err": "!    `ad` is not a heroku command.
 !    Perhaps you meant `da`.
 !    See `heroku help` for a list of available commands.", "fix": "heroku da"},
{"cmd": "heroku abc", "err": "!    `abc` is not a heroku command.
 !    Perhaps you meant `cba`.
 !    See `heroku help` for a list of available commands.", "fix": "heroku cba"}
]]"""

//let simpleTestData = NewTestDataJsonRule.Parse(simpleData)
let synthAlg = synthesizeRuleFromListOfExamples
let filteredSeq = NewParseTestData (* varEqTestData *) |> Array.toList |> List.rev |> List.toSeq //|> Seq.skip 1
//List.filter (fun ((cmd,err,fix)::_) -> List.length cmd = 5 && List.length err = 4  && List.length fix = 6 )
let testCases = filteredSeq |> Seq.map Array.toList |> Seq.map (List.map (fun item -> (strToSymbString item.Cmd, strToSymbString item.Err, strToSymbString item.Fix))) |> Seq.map (List.rev) |>  Seq.toList |> (*List.filter (fun ((cmd,err,fix)::_) -> List.length cmd = 1 && List.length err = 6  && List.length fix = 1 )  |>*) getTestCases 
//let testCases = filteredSeq |> Seq.map Array.toList |> Seq.map (List.map (fun item -> (strToSymbString item.Cmd, strToSymbString item.Err, strToSymbString item.Fix))) |>  Seq.toList |> List.filter (fun ((_,_,fix)::_) -> (String.concat " " fix) = "git stash"  ) |> getTestCases 
let trainingSet = filteredSeq |> Seq.map Array.toList |> Seq.map (List.map (fun item -> (strToSymbString item.Cmd, strToSymbString item.Err, strToSymbString item.Fix))) |> Seq.map (List.rev) |> Seq.toList  |> (*List.filter (fun ((cmd,err,fix)::_) -> List.length cmd = 1 && List.length err = 6  && List.length fix = 1 ) |>*) getTrainingSets |> List.concat
let partResults = multirulePartitioning synthAlg trainingSet 0 |> Seq.map fst |> List.concat
Seq.iter addExample trainingSet

(*
let dbRules = getRulesWithIDs |> Seq.map snd |> Seq.map unpickleRule |> Seq.toList
let dbResults = checkTestCases testCases dbRules
let dbSuccesses = countSuccesses dbResults
let dbFailures = getFailures dbRules testCases
printfn "DB results: nonsingle rules: %d successes: %d failures: %d" (List.length dbRules)  dbSuccesses (List.length dbFailures)
*)

(*
let rec processQueue () =
    let newExamples = getQueuedExamples |> Seq.toList |> List.toSeq in
    newExamples
    |> Seq.map (fun (_, id, c,e,f) -> (id, c.Split [|' '|] |> Array.toList, e.Split [|' '|] |> Array.toList, f.Split [|' '|] |> Array.toList))
    |> Seq.iter addExistingExampleToRules
    newExamples

let rec procLoop () =
    let timer = new System.Timers.Timer(1000.0) //wait 30 seconds
    let timerEvent = Async.AwaitEvent (timer.Elapsed) |> Async.Ignore
    timer.Start()
    Async.RunSynchronously timerEvent
    let queued = processQueue ()
    queued |> Seq.iter (fun (exId,_,_,_,_) -> deleteFromQueue exId)
    procLoop ()

procLoop ()
*)

let deNBSPify str =
    let nbsp = 160 in
    let mapFxn c = if int(c) = nbsp then ' ' else c
    String.map mapFxn str |> (fun s -> s.Trim(' '))

open Newtonsoft.Json

let getFullMatches symCmd symErr =
    let cmdNameMatches = getFixRuleMatchingFirstCmdTok symCmd symErr in
    let varCmdMatches = getFixRulesWithVarCmdName symCmd symErr in
    Seq.concat <| seq{yield cmdNameMatches; yield varCmdMatches}

let getSubstrMatches symCmd symErr =
    let cmdNameMatches = getSubstrRulesMatchingFirstCmdTok symCmd symErr in
    let varCmdMatches = getSubstrRulesWithVarCmdName symCmd symErr in
    Seq.concat <|seq{ yield cmdNameMatches; yield varCmdMatches}

let getEmptyErrMatches symCmd =
    let cmdNameMatches = getEmptyErrRules symCmd in
    let varCmdMatches = getEmptyErrRulesWithVarCmdName symCmd in
    Seq.concat <|seq{ yield cmdNameMatches; yield varCmdMatches}

let trySynthFixInternalOrig cmd err ruleQuery ruleEvaluator =
    let mappedCmd = deNBSPify cmd
    let symCmd = strToSymbString (deNBSPify cmd) in
    let symErr = strToSymbString (deNBSPify err) in
    let evalFxn = (fun rule -> ruleEvaluator rule symCmd symErr)
    let ruleBins = ruleQuery symCmd symErr
    let potentialRules = ruleBins |> Seq.map (sndMap unpickleRule) in
    potentialRules 
    |> Seq.map (sndMap evalFxn) 
    |> Seq.filter  (snd >> Option.isSome) 
    |> Seq.map (sndMap Option.get) 
    |> Seq.map (sndMap <| String.concat " ") 
    |> Seq.map (fun (id, output) -> Map.ofList ["id", id.ToString(); "fix", output]) 
    |> Seq.toArray 

let cmdHasConst rule =
    let (FixRule(CmdParams pList, _, _, _)) = rule in
     not <| matchIsAllVars pList

let trySynthFixInternalPartial cmd err ruleQuery =
    let mappedCmd = deNBSPify cmd
    let symCmd = strToSymbString (deNBSPify cmd) in
    let symErr = strToSymbString (deNBSPify err) in
    let ruleBins = ruleQuery symCmd symErr
    let potentialRules = ruleBins 
                         |> Seq.map (sndMap unpickleRule)
                         |> Seq.filter (snd >> cmdHasConst) in
    potentialRules  
    |> Seq.map (sndMap (substringMatch symCmd symErr)) 
    |> Seq.map (fun (f, s) -> Seq.map 
                                 (fun x -> (f,x)) 
                                 s) 
    |> Seq.concat
    |> Seq.map (sndMap <| String.concat " ") 
    |> Seq.map (fun (id, output) -> Map.ofList ["id", id.ToString(); "fix", output]) 
    |> Seq.toArray
    
let trySynthFixFull cmd err =
    trySynthFixInternalOrig cmd err getFullMatches evalTopLevelExpr

let trySynthFixSubstr cmd err =
    trySynthFixInternalPartial cmd err getSubstrMatches
    

let trySynthFixEmpty cmd err =
    let mappedCmd = deNBSPify cmd
    let symCmd = strToSymbString (deNBSPify cmd) in
    let symErr = strToSymbString (deNBSPify err) in
    let evalFxn = (fun rule -> evalTopLevelExpr rule symCmd symErr) 
    let ruleBins =  getEmptyErrMatches symCmd 
    let potentialRules = ruleBins 
                         |> Seq.map (sndMap unpickleRule) 
                         |> Seq.filter (snd >> cmdHasConst) in
    potentialRules
    |> Seq.map (sndMap tryMakeRuleWithEmptyErr) 
    |> Seq.filter (snd >> Option.isSome)
    |> Seq.map (sndMap Option.get)
    |> Seq.map (sndMap evalFxn) 
    |> Seq.filter  (snd >> Option.isSome) 
    |> Seq.map (sndMap Option.get) 
    |> Seq.map (sndMap <| String.concat " ") 
    |> Seq.map (fun (id, output) -> Map.ofList ["id", id.ToString(); "fix", output]) 
    |> Seq.toArray 

let errIsEmptyStr err =
    match err with
    | hd::[] -> hd = ""
    | _ -> false

let trySynthFix cmd err =
    let fullMatches = trySynthFixFull cmd err in
    if Array.isEmpty fullMatches then
       let symErr = strToSymbString (deNBSPify err) in
       if errIsEmptyStr symErr then
          trySynthFixEmpty cmd err
          |> JsonConvert.SerializeObject
       else
          trySynthFixSubstr cmd err
          |> JsonConvert.SerializeObject 
    else
       JsonConvert.SerializeObject fullMatches

let tryAddEx id cmd err fix =
    let exID = System.UInt32.Parse id in
    let symCmd = strToSymbString (deNBSPify cmd) in
    let symErr = strToSymbString (deNBSPify err) in
    let symFix = strToSymbString (deNBSPify fix) in
    addExistingExampleToRules (exID, symCmd, symErr, symFix)
    "Ok"

let tryGetUnfixed () =
    let invPair = getPresentableExample() in
    let invAssoc = Map.ofList ["cmd", fst invPair; "err", snd invPair] in
    JsonConvert.SerializeObject invAssoc

let tryGetRandom () =
    let invPair = getRandomExample() in
    let invAssoc = Map.ofList ["cmd", fst invPair; "err", snd invPair] in
    JsonConvert.SerializeObject invAssoc


    

open Suave
open Suave.Web
open Suave.Filters
open Suave.Operators
open Suave.Writers
open Suave.Successful

let setCORSHeaders =
    setHeader "Access-Control-Allow-Origin" "*"
    >=> setHeader "Access-Control-Allow-Headers" "content-type"

let allow_cors : WebPart =
    choose [OPTIONS >=> 
                    fun context ->
                        context |> (
                            setCORSHeaders
                            >=> OK "CORS approved")
    ]
let cfg = { defaultConfig with bindings = [ HttpBinding.mkSimple HTTP "0.0.0.0" 8083] }

let simpleMethod (r : HttpRequest) = 
    let v1 = r.queryParam "cmd" in
    let v2 = r.queryParam "err" in
    "an error occurred at routing"
    

let reqFixResponder : WebPart = request(fun r -> setHeader "Access-Control-Allow-Origin" "*" >=>  OK (match (r.formData "cmd", r.formData "err" ) with
                                                                                                        | (Choice1Of2 cmd, Choice1Of2 err) -> trySynthFix cmd err
                                                                                                        | _ -> simpleMethod r))

let exSynthResponder = request(fun r -> setHeader "Access-Control-Allow-Origin" "*" >=>  OK (match (r.queryParam "id", r.queryParam "cmd", r.queryParam "err", r.queryParam "fix" ) with
                                                                                                        | (Choice1Of2 id, Choice1Of2 cmd, Choice1Of2 err, Choice1Of2 fix) -> tryAddEx id cmd err fix 
                                                                                                        | _ -> "an error occurred at routing"))


let voteResponder = request(fun r -> setHeader "Access-Control-Allow-Origin" "*" >=>  OK (match (r.queryParam "id") with
                                                                                          | Choice1Of2 id -> upvoteRule (System.UInt32.Parse id) |> ignore |> fun _ -> "voted"
                                                                                          | _ -> "an error occurred at routing"))

let randUnfixedResponder = request(fun r -> setHeader "Access-Control-Allow-Origin" "*" >=>  OK (tryGetUnfixed()))

let randExampleResponder = request(fun r -> setHeader "Access-Control-Allow-Origin" "*" >=>  OK (tryGetRandom()))

let testResponder = request(fun r-> setHeader "Access-Control-Allow-Origin" "*" >=> OK (getPartialErrCandidateRulesByKeyword 1 2 "" ("found" :: []) |> Seq.map fst |> Seq.toArray |> JsonConvert.SerializeObject))

let responder  = choose [ GET >=> choose
                                  [path "/exSynth.ajax" >=> exSynthResponder
                                   path "/upvote.ajax" >=> voteResponder
                                   path "/getOneUnfixed.ajax" >=> randUnfixedResponder
                                   path "/getRandEx.ajax" >=> randExampleResponder
                                   path "/test.ajax" >=> testResponder]
                          POST >=> choose
                                  [path "/reqFix.ajax" >=> reqFixResponder]]



//TO DO: uncomment
[<EntryPoint>]
let main argv = 
    startWebServer cfg responder
    0


    

//checkResults |> List.filter (fun (first,_,_) -> first > 1) |> List.map (fun (_,_,last) -> last) |> List.iter printStrList


(*
let flatList = NewParseTestData |> Array.toList |> List.rev |> List.toSeq |> Seq.skip 1 |> Seq.map Array.toList |> List.concat |> List.map (fun item -> (strToSymbString item.Cmd, strToSymbString item.Err, strToSymbString item.Fix))
for i in 0..4 do
    printfn "iter %d" i
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let constlist = makeConstRuleList flatList
    let resList = buildRuleList flatList constlist
    //let results = multirulePartitioning synthAlg flatList i
    //let resList = Seq.toList results //Force the sequence computation
    timer.Stop()
    printfn "Time %d" timer.ElapsedMilliseconds
printfn "done"*)
(*
// for this experiment, just dump everything into one bucket.
let singleBucket = true
let synthAlg = synthesizeRuleFromListOfExamples

if singleBucket then
    let flatList = NewParseTestData |> Array.toList |> List.rev |> List.toSeq |> Seq.skip 1 |> Seq.map Array.toList |> List.concat |> List.map (fun item -> (strToSymbString item.Cmd, strToSymbString item.Err, strToSymbString item.Fix))
    let timer = System.Diagnostics.Stopwatch.StartNew()
    testOneGroup synthAlg flatList
    let totalTests = failingTests + passingTests
    timer.Stop()
    printfn "Groups: %d, Synthesized: %d, Failed %d, Time %d" totalTests passingTests failingTests timer.ElapsedMilliseconds
else
    for exArr in NewParseTestData do
        let exList = Array.toList exArr |> List.map (fun item -> (strToSymbString item.Cmd, strToSymbString item.Err, strToSymbString item.Fix))
        testOneGroup synthAlg exList 
        let totalTests = failingTests + passingTests
        printfn "Groups: %d, Synthesized: %d, Failed %d" totalTests passingTests failingTests*)
     
    
//let s = Console.ReadLine() 
(*
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
for item in ParseTestData do
    let (cmdEx1, errEx1, fixEx1) = (strToSymbString item.Ex1.Cmd, strToSymbString item.Ex1.Err, strToSymbString item.Ex1.Fix) in
    let (cmdEx2, errEx2, fixEx2) = (strToSymbString item.Ex2.Cmd, strToSymbString item.Ex2.Err, strToSymbString item.Ex2.Fix) in
    let (cmdEx3, errEx3, fixEx3) = (strToSymbString item.Ex3.Cmd, strToSymbString item.Ex3.Err, strToSymbString item.Ex3.Fix) in
    testId <- testId+1;
    printf "Example %d:" testId;    
    for i in 1..numberOfRepeats do
        numberOfSingleExp <- numberOfSingleExp+7
        //runSingleInstance (concatenateList cmdEx1 i) (concatenateList errEx1 i) (concatenateList fixEx1 i) (concatenateList cmdEx2 i) (concatenateList errEx2 i) (concatenateList fixEx2 i) (concatenateList cmdEx3 i) (concatenateList errEx3 i) (concatenateList fixEx3 i)
        runSingleInstance cmdEx1 errEx1 fixEx1 cmdEx2 errEx2 fixEx2 cmdEx3 errEx3 fixEx3
        //runSingleInstance cmdEx1 errEx1 fixEx1 cmdEx3 errEx3 fixEx3 cmdEx2 errEx2 fixEx2
        //runSingleInstance cmdEx2 errEx2 fixEx2 cmdEx3 errEx3 fixEx3 cmdEx1 errEx1 fixEx1
        //let l1 = [(cmdEx1, errEx1, fixEx1); (cmdEx2, errEx2, fixEx2);(cmdEx3, errEx3, fixEx3)] in
        //let l2 = [(cmdEx1, errEx1, fixEx1); (cmdEx2, errEx2, fixEx2);(cmdEx3, errEx3, fixEx3)] in
        //let l3 = [(cmdEx1, errEx1, fixEx1); (cmdEx2, errEx2, fixEx2);(cmdEx3, errEx3, fixEx3)] in
        //let llong = l1 @ l2 @ l3 in
        //runMultiExample l1
        //runMultiExample l2
        //runMultiExample l3
        //runMultiExample llong
        
stopWatch.Stop()
printfn ""
printfn "Repeat each test %f times" (float numberOfRepeats)
printfn "Finished in %f milliseconds" (stopWatch.Elapsed.TotalMilliseconds)
printfn "Avg time per rule: %f milliseconds" (stopWatch.Elapsed.TotalMilliseconds/(float numberOfSingleExp))
printfn "TEST CASES PASSED: %d" (passingTests/numberOfRepeats)
printfn "TEST CASES FAILED: %d" (failingTests/numberOfRepeats)
printfn "Total number of tests: %d" (numberOfSingleExp/numberOfRepeats)
*)

//let s = Console.ReadLine()