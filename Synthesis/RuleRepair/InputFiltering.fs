module InputFiltering
open Parser
open RuleEvaluator
open RuleSynthesizer
open System.Text.RegularExpressions

//Filter profanity and obviously malicious code.
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

//[whitespace]rm([whitespace] [flag])*[whitespace](/ | /*)[whitespace* anchor | whitespace ;]
//The resulting match group contains one or more Unix-style flag groups.
let rmDestructive = "(?:.*;\s*|^\s*)(?:(?:sudo)(?:sdo)\s+)?(?:rm|\/bin/rm)((?:(?:\s+-\w+)|(?:\s+--(?:\w|-)+))*)\s+(?:\/|\/[\.\*]*)(?:$|\s*;|\s+$)"

//Possibly over-restrictive and simultaneously nowhere near exhaustive, but should suffice for dissuading low-creativity trolls
//With apologies/thanks to George Carlin.
let profanity = "shit|piss|fuck|cunt|cock|tit|twat|penis"

let isRecursiveFlag flag =
    match flag with 
    | "--recursive" -> true
    | _ -> String.exists (fun c -> c = 'r' || c = 'R') flag

let isForceFlag flag =
    match flag with
    | "--force" -> true
    | _ -> String.exists (fun c -> c = 'f') flag

let checkFlag (recurseFound, forceFound) flag =
    (recurseFound || isRecursiveFlag flag, forceFound || isForceFlag flag)
    
let analyzeFlagList flagList =
    match List.fold checkFlag (false, false) flagList with
    | (true, true) -> true
    | _ -> false

let analyzeFlags flagParams =
    let flags = strToSymbString flagParams in
    analyzeFlagList flags

let alertMatch str =
    match str with
    | Regex rmDestructive [flags] -> analyzeFlags flags
    | Regex profanity _ -> true
    | _ -> false

let rmMatch str =
    match str with
    | Regex rmDestructive [flags] -> analyzeFlags flags
    | _ -> false

let profanityMatch str =
   match str with
   | Regex profanity _ -> true
   | _ -> false
