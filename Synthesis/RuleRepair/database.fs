module database
open System.Security.Cryptography
open System.Text
open System.Data
open FSharp.Data.Sql

[<Literal>]
let connString = "Server=localhost;Database=synthdb;User=root;Password=o26baf!OMGPuppies"


[<Literal>]
let dbVendor = Common.DatabaseProviderTypes.MYSQL

[<Literal>]
let resPath = __SOURCE_DIRECTORY__ + @"/../Packages/MySql.Data.6.9.8/lib/net45"

[<Literal>]
let indivAmount = 1000

[<Literal>]
let useOptTypes = true

type sql = SqlDataProvider<dbVendor,connString,ResolutionPath=resPath,IndividualsAmount=indivAmount,UseOptionTypes=useOptTypes, Owner = "synthdb">

let ctx = sql.GetDataContext()

let bashID = uint32(1)

let md5 (str : string) =
   use hashAlg = MD5.Create()
   str 
   |> Encoding.ASCII.GetBytes
   |> hashAlg.ComputeHash
   |> Seq.map (fun c -> c.ToString("X2"))
   |> Seq.reduce (+)

//These early add/get functions are only for initializing the database from the JSON example set.
//Since this is single-threaded with input controlled by us, we do the obvious thing, since it'll be correct.
//Don't use these for anything else.

let nonEmptyHead res =
   match not <| Seq.isEmpty res with
   | true -> Some <| Seq.head res
   | false -> None

let getCmd cmd =
   let cmdText = String.concat " " cmd
   let res = 
       query{
           for cmdRow in ctx.Synthdb.Command do
           where (cmdRow.Text = cmdText)
           select (cmdRow.Id)
       } |> Seq.map id in
   nonEmptyHead res

let getErr err = 
    let errText = String.concat " " err
    let res = 
        query{
            for errRow in ctx.Synthdb.Output do
            where (errRow.Text = errText)
            select (errRow.Id)
        } |> Seq.map id in
    nonEmptyHead res

let getFix fix =
    let fixText = String.concat " " fix
    let res =
        query{
            for fixRow in ctx.Synthdb.Fix do
            where (fixRow.Text = fixText)
            select (fixRow.Id)
        } |> Seq.map id in
    nonEmptyHead res

let getInv shellId cmdId errId =
    let res = 
        query{
            for invRow in ctx.Synthdb.Invocation do
            where (invRow.ShellId = shellId && invRow.CmdId = cmdId && invRow.OutId = errId)
            select (invRow.Id)
        } |> Seq.map id in
    nonEmptyHead res

let getExample invId fixId =
    let res = 
        query{
            for exRow in ctx.Synthdb.Repairexample do
            where (exRow.InvocationId = invId && exRow.FixId = fixId)
            select (exRow.Id)
        } |> Seq.map id in
    nonEmptyHead res
      
   
let addCmd cmd =
    let row = ctx.Synthdb.Command.Create()
    let cmdLen = cmd |> List.length
    let cmdText = String.concat " " cmd
    row.Text <- cmdText
    row.FirstWord <- List.head cmd
    row.TxtHash <- md5 cmdText
    row.WordCount <- uint32(cmdLen)
    ctx.SubmitUpdates()
    row.Id
    
let addErr err =
    let row = ctx.Synthdb.Output.Create()
    let errLen = err |> List.length
    let errText = String.concat " " err
    row.Text <- errText
    row.FirstWord <- List.head err
    row.TxtHash <- md5 errText
    row.WordCount <- uint32(errLen)
    ctx.SubmitUpdates()
    row.Id

let addFix fix =
    let row = ctx.Synthdb.Fix.Create()
    let fixLen = fix |> List.length
    let fixText = String.concat " " fix
    row.Text <- fixText
    row.FirstWord <- List.head fix
    row.TxtHash <- md5 fixText
    row.WordCount <- uint32(fixLen)
    ctx.SubmitUpdates()
    row.Id

let updateOrCreate getFxn createFxn toAdd =
    let existingId = getFxn toAdd in
    match existingId with
    | Some id -> id
    | None -> createFxn toAdd

let upsertCmd cmd = updateOrCreate getCmd addCmd cmd
    
let upsertErr err = updateOrCreate getErr addErr err

let upsertFix fix = updateOrCreate getFix addFix fix

let addInvocationFromIDs shellID cmdId errId =
    let row = ctx.Synthdb.Invocation.Create()
    row.ShellId <- shellID
    row.CmdId <- cmdId
    row.OutId <- errId
    ctx.SubmitUpdates()
    row.Id

let upsertInvocationFromIDs shellId cmdId errId = 
    updateOrCreate (fun (shellId, cmdId, errId) -> getInv shellId cmdId errId) 
                   (fun (shellId, cmdId, errId) -> addInvocationFromIDs shellId cmdId errId)
                   (shellId, cmdId, errId)

let addInvocation shellID cmd err =
    let cmdId = upsertCmd cmd in
    let errId = upsertErr err in
    upsertInvocationFromIDs shellID cmdId errId

let addExampleFromIds invId fixId =
    let row = ctx.Synthdb.Repairexample.Create() 
    row.InvocationId <- invId
    row.FixId <- fixId
    row.SubmitCount <- uint32(3)
    ctx.SubmitUpdates()
    row.Id

let upsertExample invId fixId =
    updateOrCreate (fun (invId, fixId) -> getExample invId fixId)
                   (fun (invId, fixId) -> addExampleFromIds invId fixId)
                   (invId, fixId)

let addBashExample cmd err fix =
    let invocationID = addInvocation bashID cmd err in
    let fixID = upsertFix fix in
    upsertExample invocationID fixID
    

let addRuleFromIDs ruleBin cmdName cmdlen errlen fixlen =
   let row = ctx.Synthdb.Fixrule.Create()
   row.FixProg <- ruleBin
   row.CmdName <- cmdName
   row.CmdLen <- cmdlen
   row.OutLen <- errlen
   row.FixLen <- fixlen
   row.Votes <- uint32 0
   ctx.SubmitUpdates()
   row.Id

let addExampleToSet ruleId cmdId  =
    let row = ctx.Synthdb.Exampleset.Create()
    row.RuleId <- ruleId
    row.CmdId <- cmdId
    ctx.SubmitUpdates()
    row.Id

let addKeywordMapping ruleId keyword =
    let dbId = uint32(ruleId)
    ctx.Procedures.AddKeywordMapping.Invoke(keyword, ruleId) |> ignore

let deleteFromQueue entryId =
    let row = ctx.Synthdb.Examplequeue.Create()
    row.Id <- entryId
    row.Delete()
    ctx.SubmitUpdates()

let makeExampleSetFromIDs ruleId idList =
   Seq.iter (fun x -> addExampleToSet ruleId x |> ignore) idList

let getExamples =
    query {for rEx in ctx.Synthdb.Repairexample do
               where (rEx.SubmitCount >= uint32(2)
                      && rEx.FlagCount < uint32(6))
               join invocation in ctx.Synthdb.Invocation on (rEx.InvocationId = invocation.Id)
               join cmd in ctx.Synthdb.Command on (invocation.CmdId = cmd.Id)
               join err in ctx.Synthdb.Output on (invocation.OutId = err.Id)
               join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id)
               select (rEx, invocation, cmd, err, fix)} |> Seq.map (fun (rEx, invocation, cmd,err,fix) -> (rEx.Id, cmd.Text, err.Text, fix.Text))

let testGetExamples () =
    query {for rEx in ctx.Synthdb.Repairexample do
               //where (rEx.Id = uint32(5229) || rEx.Id = uint32(5230))
               where (rEx.FlagCount < uint32(6) && rEx.SubmitCount >= uint32(2))
               where (rEx.FlagCount < uint32(6))
               join invocation in ctx.Synthdb.Invocation on (rEx.InvocationId = invocation.Id)
               join cmd in ctx.Synthdb.Command on (invocation.CmdId = cmd.Id)
               join err in ctx.Synthdb.Output on (invocation.OutId = err.Id)
               join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id)
               select (rEx, invocation, cmd, err, fix)} |> Seq.map (fun (rEx, invocation, cmd,err,fix) -> (rEx.Id, cmd.Text, err.Text, fix.Text))
let testGetExample id =
    query {for rEx in ctx.Synthdb.Repairexample do
               where (rEx.FlagCount < uint32(6) && rEx.Id = id)
               join invocation in ctx.Synthdb.Invocation on (rEx.InvocationId = invocation.Id)
               join cmd in ctx.Synthdb.Command on (invocation.CmdId = cmd.Id)
               join err in ctx.Synthdb.Output on (invocation.OutId = err.Id)
               join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id) 
               select (rEx, invocation, cmd, err, fix)} |> Seq.map (fun (rEx, invocation, cmd,err,fix) -> (rEx.Id, cmd.Text, err.Text, fix.Text)) |> Seq.head

let getRulesWithIDs =
    query {for ruleInfo in ctx.Synthdb.Fixrule do
               select ruleInfo} |> Seq.map (fun ruleInfo -> (ruleInfo.Id, ruleInfo.FixProg)) 


let getExampleSetIDs ruleID =
    query{for exampleInfo in ctx.Synthdb.Exampleset do
              where (exampleInfo.RuleId = ruleID) 
              select (exampleInfo.CmdId)} |> Seq.map id 

let getRulesAndExamples () =
    getRulesWithIDs |> Seq.map (fun (ruleID, fixProg) -> (fixProg, getExampleSetIDs ruleID))   

let getFixRuleMatchingFirstCmdTok cmd err = 
    let cmdLen = List.length cmd in
    let errLen = List.length err in
    let firstTok = match cmdLen with
                  | 0 -> ""
                  | _ -> List.head cmd in
    query {for ruleInfo in ctx.Synthdb.Fixrule do
           where (ruleInfo.CmdLen = uint32(cmdLen) && 
                  ruleInfo.OutLen = uint32(errLen) && 
                  ruleInfo.CmdName = firstTok &&
                  ruleInfo.FlagCount < uint32(6))
           select (ruleInfo.Id, ruleInfo.FixProg) }


let getFixRulesWithVarCmdName cmd err =
    let cmdLen = List.length cmd in
    let errLen = List.length err in
    query {for ruleInfo in ctx.Synthdb.Fixrule do
           where (ruleInfo.CmdLen = uint32(cmdLen) && 
                  ruleInfo.OutLen = uint32(errLen) && 
                  ruleInfo.CmdName = "" &&
                  ruleInfo.FlagCount < uint32(6))
           select (ruleInfo.Id, ruleInfo.FixProg)}

let upvoteRule ruleID = ctx.Procedures.UpvoteRule.Invoke(ruleID)

let randomDbIdx (max : uint32) =
    let randGen = System.Random() in
    let maxInt = int(max)
    uint32(randGen.Next(maxInt))

let getRandomExample () = 
    let count = ctx.Procedures.GetNumRepairExamples.Invoke().numEx  in
    let idx = randomDbIdx count in
    let res = ctx.Procedures.GetExample.Invoke(idx) in
    (res.invCmd, res.invErr)

let getUnfixedExample () =
    let count = ctx.Procedures.GetNumInvocationsWithNoFix.Invoke().numInv in
    let idx = randomDbIdx count in
    let res = ctx.Procedures.GetUnfixedInvocation.Invoke(idx) in
    (res.invCmd, res.invErr)

let getLowFixInv idx =
    let res = ctx.Procedures.GetRandLowFixInvocation.Invoke(idx) in
    (res.cmd, res.err, res.invId)

let getRandInv () =
    let count = ctx.Procedures.GetNumInvocations.Invoke().numInv in
    let idx = randomDbIdx count in
    let res = ctx.Procedures.GetInvocation.Invoke(idx) in
    (res.cmd, res.err, res.invId)

let getPresentableExample () =
    let lowFixCount = ctx.Procedures.GetTotalNumLowFixInvocations.Invoke().numInv in
    let bound = lowFixCount in
    if (int bound) <> 0 then
        let idx = randomDbIdx bound in
            getLowFixInv idx    
    else 
        getRandInv()

open System.Linq
let getPartialErrCandidateRulesByKeyword cmdLen substrLen firstTok (keywords : string List) =
    query{for ruleInfo in ctx.Synthdb.Fixrule do
          join mapping in ctx.Synthdb.Keywordstofix 
                       on (ruleInfo.Id = mapping.RuleId)
          where (ruleInfo.CmdLen = uint32(cmdLen) 
                 && ruleInfo.OutLen >= uint32(substrLen) 
                 && ruleInfo.CmdName = firstTok
                 && (keywords.Contains(mapping.Keyword))
                 && ruleInfo.FlagCount < uint32(6))
          select (ruleInfo.Id, ruleInfo.FixProg)} |> Seq.map id

let mkCanonical lst = String.concat " " lst

let computeHashDigest lst = md5 <| mkCanonical lst

let getPartialErrCandidateExamplesByKeyword cmd substrLen (keywords : string List) =
   let cmdLen = List.length cmd
   let cmdHash = computeHashDigest cmd in
   let firstTok = List.head cmd
   query{for cmd in ctx.Synthdb.Command do
         join inv in ctx.Synthdb.Invocation on (cmd.Id = inv.CmdId)
         join err in ctx.Synthdb.Output on (inv.OutId = err.Id)
         join rEx in ctx.Synthdb.Repairexample on (inv.Id = rEx.InvocationId)
         join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id)
         join mapping in ctx.Synthdb.Keywordstoexamples on (rEx.Id = mapping.ExampleId)
         where (cmd.WordCount = uint32(cmdLen) 
                && cmd.TxtHash = cmdHash
                && err.WordCount >= uint32(substrLen)
                && cmd.FirstWord = firstTok
                && rEx.SubmitCount >= uint32(2)
                && (keywords.Contains(mapping.Keyword))
                && rEx.FlagCount < uint32(6))
         select (rEx.Id, cmd.Text, err.Text, fix.Text) 
        } |> Seq.map id

let getCandidatePartialExamples cmd err =
    getPartialErrCandidateExamplesByKeyword cmd (List.length err) err

let getFullErrCandidateExamples cmd err =
    let cmdHash = computeHashDigest cmd in
    let errHash = computeHashDigest err in
    let cmdLen = List.length cmd in
    let errLen = match err with
                 | [""] -> 0
                 | _ -> List.length err in
    query{for cmd in ctx.Synthdb.Command do
          join inv in ctx.Synthdb.Invocation on (cmd.Id = inv.CmdId)
          join err in ctx.Synthdb.Output on (inv.OutId = err.Id)
          join rEx in ctx.Synthdb.Repairexample on (inv.Id = rEx.InvocationId)
          join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id)
          where (cmd.TxtHash = cmdHash
                 && err.TxtHash = errHash
                 && cmd.WordCount = uint32(cmdLen)
                 && err.WordCount = uint32(errLen) //TODO: add submit count check
                 && rEx.FlagCount < uint32(6))
          select (rEx.Id, cmd.Text, err.Text, fix.Text)} |> Seq.map id

let getSimilarExamples cmd err =
   let cmdLen = List.length cmd
   let cmdHash = computeHashDigest cmd in
   let substrLen = List.length err
   let firstTok = List.head cmd
   query{for cmd in ctx.Synthdb.Command do
         join inv in ctx.Synthdb.Invocation on (cmd.Id = inv.CmdId)
         join err in ctx.Synthdb.Output on (inv.OutId = err.Id)
         join rEx in ctx.Synthdb.Repairexample on (inv.Id = rEx.InvocationId)
         join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id)
         where (err.WordCount >= uint32(substrLen)
                && cmd.FirstWord = firstTok
                && rEx.SubmitCount >= uint32(2)
                && rEx.FlagCount < uint32(6))
         sortByDescending rEx.SubmitCount
         take 20 
         select (rEx.Id, cmd.Text, err.Text, fix.Text)
        } |> Seq.map id


let getSubstrRulesMatchingFirstCmdTok cmd err = 
    let cmdLen = List.length cmd in
    let errLen = List.length err in
    let firstTok = match cmdLen with
                   | 0 -> ""
                   | _ -> List.head cmd in
    getPartialErrCandidateRulesByKeyword cmdLen errLen firstTok err

let getSubstrRulesWithVarCmdName cmd err =
    let cmdLen = List.length cmd in
    let errLen = List.length err in
    getPartialErrCandidateRulesByKeyword cmdLen errLen "" err


let getEmptyErrRules cmd =
    let cmdLen = List.length cmd in
    let firstTok = match cmdLen with
                   | 0 -> ""
                   | _ -> List.head cmd in
    query {for ruleInfo in ctx.Synthdb.Fixrule do
           where (ruleInfo.CmdLen = uint32(cmdLen) 
                  && ruleInfo.CmdName = firstTok
                  && ruleInfo.FlagCount < uint32(6))
           select (ruleInfo.Id, ruleInfo.FixProg)}

let getEmptyErrRulesWithVarCmdName cmd =
    let cmdLen = List.length cmd in
    query {for ruleInfo in ctx.Synthdb.Fixrule do
           where (ruleInfo.CmdLen = uint32(cmdLen)
                  && ruleInfo.CmdName = ""
                  && ruleInfo.FlagCount < uint32(6))
           select (ruleInfo.Id, ruleInfo.FixProg)}

let flagExample rexID =
    ctx.Procedures.FlagExample.Invoke(rexID)

let backPropToExamples ruleID =
    let exampleIDs = query{for ruleInfo in ctx.Synthdb.Fixrule do
                           join esInfo in ctx.Synthdb.Exampleset on (ruleInfo.Id = esInfo.RuleId)
                           join rexInfo in ctx.Synthdb.Repairexample on (esInfo.CmdId = rexInfo.Id)
                           where (ruleInfo.Id = uint32(ruleID))
                           select (rexInfo.Id)} |> Seq.map id in
    exampleIDs 
    |> Seq.iter (flagExample >> ignore)

let flagRule rID =
    ctx.Procedures.FlagRule.Invoke(rID) |> ignore
    backPropToExamples rID

let flagInvocation invID =
    ctx.Procedures.FlagInvocation.Invoke(invID)

let upvoteExample rexID = 
    ctx.Procedures.UpvoteExample.Invoke(rexID)

let dumpInvocations () =
    query {for inv in ctx.Synthdb.Invocation do
           join cmd in ctx.Synthdb.Command on (inv.CmdId = cmd.Id)
           join err in ctx.Synthdb.Output on (inv.OutId = err.Id)
           select (inv.Id, cmd.Text, err.Text, inv)} |> Seq.map id

let dumpExamples () = 
    query {for rEx in ctx.Synthdb.Repairexample do
           join inv in ctx.Synthdb.Invocation on (rEx.InvocationId = inv.Id)
           join cmd in ctx.Synthdb.Command on (inv.CmdId = cmd.Id)
           join err in ctx.Synthdb.Output on (inv.OutId = err.Id)
           join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id)
           select (rEx.Id, cmd.Text, err.Text, fix.Text, fix)}

let dumpRules () = 
    query {for rule in ctx.Synthdb.Fixrule do
           join sMap in ctx.Synthdb.Exampleset on (rule.Id = sMap.RuleId)
           join rEx in ctx.Synthdb.Repairexample on (sMap.CmdId = rEx.Id)
           join inv in ctx.Synthdb.Invocation on (rEx.InvocationId = inv.Id)
           join cmd in ctx.Synthdb.Command on (inv.CmdId = cmd.Id)
           join err in ctx.Synthdb.Output on (inv.OutId = err.Id)
           join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id)
           select (rule.Id, cmd.Text, err.Text, fix.Text)}

let getRuleWithID id =
    query{ for rule in ctx.Synthdb.Fixrule do
            where (rule.Id = id)
            select rule.FixProg}

let incrMatchCountSingleton rexID =
    ctx.Procedures.IncrMatchCountSingleton.Invoke(rexID) |> ignore


let incrMatchCountRule ruleID =
    ctx.Procedures.IncrMatchCountRule.Invoke(ruleID) |> ignore

let incrMatchCountSingletonList ids =
    ids |> Seq.map (fun x -> uint32(x)) |> Seq.map incrMatchCountSingleton |> ignore

let incrMatchCountRuleList ids =
    ids |> Seq.map (fun x -> uint32(x)) |> Seq.map incrMatchCountRule |> ignore

let updateRequestRecord cmd err =
    let cmdhash = md5 cmd in
    let errhash = md5 err in
    ctx.Procedures.UpdateRequestRecord.Invoke(cmdhash, errhash, cmd, err)


let debugGetExample idx =
    let results = query{for rEx in ctx.Synthdb.Repairexample do
          join inv in ctx.Synthdb.Invocation on (rEx.InvocationId = inv.Id)
          join cmd in ctx.Synthdb.Command on (inv.CmdId = cmd.Id)
          join err in ctx.Synthdb.Output on (inv.OutId = err.Id)
          join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id)
          where (rEx.Id = idx)
          select (cmd.Text, err.Text, fix.Text)} in 
     results |> Seq.map( (fun (a,b,c) -> a + " " + b + " " +  c ) ) 