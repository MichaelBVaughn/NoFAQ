module database
open System.Security.Cryptography
open System.Text
open System.Data
open FSharp.Data.Sql

[<Literal>]
let connString = ""


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

let addInvocationFromIDs shellID cmdId errId =
    let row = ctx.Synthdb.Invocation.Create()
    row.ShellId <- shellID
    row.CmdId <- cmdId
    row.OutId <- errId
    ctx.SubmitUpdates()
    row.Id

let addInvocation shellID cmd err =
    let cmdId = addCmd cmd in
    let errId = addErr err in
    addInvocationFromIDs shellID cmdId errId

let addBashExample cmd err fix =
    let invocationID = addInvocation bashID cmd err
    let fixID = addFix fix
    let row = ctx.Synthdb.Repairexample.Create() 
    row.InvocationId <- invocationID
    row.FixId <- fixID
    ctx.SubmitUpdates()
    row.Id

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
    row.Id

let deleteFromQueue entryId =
    let row = ctx.Synthdb.Examplequeue.Create()
    row.Id <- entryId
    row.Delete()
    ctx.SubmitUpdates()

let makeExampleSetFromIDs ruleId idList =
   Seq.iter (fun x -> addExampleToSet ruleId x |> ignore) idList

let getExamples =
    query {for rEx in ctx.Synthdb.Repairexample do
               join invocation in ctx.Synthdb.Invocation on (rEx.InvocationId = invocation.Id)
               join cmd in ctx.Synthdb.Command on (invocation.CmdId = cmd.Id)
               join err in ctx.Synthdb.Output on (invocation.OutId = err.Id)
               join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id)
               select (rEx, invocation, cmd, err, fix)} |> Seq.map (fun (rEx, invocation, cmd,err,fix) -> (rEx.Id, cmd.Text, err.Text, fix.Text))

let getQueuedExamples =
    query {for qEx in ctx.Synthdb.Examplequeue do
           join rEx in ctx.Synthdb.Repairexample on (qEx.ExampleId = rEx.Id)
           join invocation in ctx.Synthdb.Invocation on (rEx.InvocationId = invocation.Id)
           join cmd in ctx.Synthdb.Command on (invocation.CmdId = cmd.Id)
           join err in ctx.Synthdb.Output on (invocation.OutId = err.Id)
           join fix in ctx.Synthdb.Fix on (rEx.FixId = fix.Id)
           select (qEx, invocation, cmd, err, fix)} |> Seq.map (fun (qEx, invocation, cmd,err,fix) -> (qEx.Id, invocation.Id, cmd.Text, err.Text, fix.Text))

let getRulesWithIDs =
    query {for ruleInfo in ctx.Synthdb.Fixrule do
               select ruleInfo} |> Seq.map (fun ruleInfo -> (ruleInfo.Id, ruleInfo.FixProg)) 


let getExampleSetIDs ruleID =
    query{for exampleInfo in ctx.Synthdb.Exampleset do
              where (exampleInfo.RuleId = ruleID) 
              select (exampleInfo.CmdId)} |> Seq.map id 

let getRulesAndExamples =
    getRulesWithIDs |> Seq.map (fun (ruleID, fixProg) -> (fixProg, getExampleSetIDs ruleID))   

let getFixRuleMatchingFirstCmdTok cmd err = 
    let cmdLen = List.length cmd in
    let errLen = List.length err in
    let firstTok = match cmdLen with
                  | 0 -> ""
                  | _ -> List.head cmd in
    query {for ruleInfo in ctx.Synthdb.Fixrule do
           where (ruleInfo.CmdLen = uint32(cmdLen) && ruleInfo.OutLen = uint32(errLen) && ruleInfo.CmdName = firstTok)
           select (ruleInfo.Id, ruleInfo.FixProg) }


let getFixRulesWithVarCmdName cmd err =
    let cmdLen = List.length cmd in
    let errLen = List.length err in
    query {for ruleInfo in ctx.Synthdb.Fixrule do
           where (ruleInfo.CmdLen = uint32(cmdLen) && ruleInfo.OutLen = uint32(errLen) && ruleInfo.CmdName = "")
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

let getRandLowFixInv() =
    let count = ctx.Procedures.GetTotalNumInvocations.Invoke().numInv in
    let bound = Operators.min count (uint32 100) in
    let idx = randomDbIdx bound in
    let res = ctx.Procedures.GetRandLowFixInvocation.Invoke(idx) in
    (res.cmd, res.err)

let getPresentableExample () =
    let unfixedCount = ctx.Procedures.GetNumInvocationsWithNoFix.Invoke().numInv in
    if (int unfixedCount) <> 0 then
        let idx = randomDbIdx unfixedCount in
        let res = ctx.Procedures.GetUnfixedInvocation.Invoke(idx) in
        (res.invCmd, res.invErr)
    else
        getRandLowFixInv()
   
