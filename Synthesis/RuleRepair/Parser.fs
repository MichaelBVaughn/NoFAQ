module Parser

open FParsec
open System

type SymbString = string list 

type Expr = ConstStr of string
          | Var of int * string * string * (string list)  // int Denotes the position in the string cmd+err (this avoids renaming problems)
                                                          // the next two string parameters denote the expected prefix and suffix of matching strings, respectively
                                        // the string list denotes the enviroment (the inputs bound to this var)
          | VarEq of int * (string list)     //String denotes the current binding to the varEq expression - this is used for an intermediate step in the synthesis

type Pos = CPos of int 
         | SymPos of char * int * int // Sympos('a',3,-1) is position before (-1) the third occurrence of 'a'

//Given a string s, SubstrAndAppend(i,j,left,right) outputs the string (left s[i,j] right)
type Fun = SubstrAndAppend of ((Pos * Pos) list * string * string) list

type FixExpr = FixConstStr of string
             | FixFuncApp of ((Fun * int) list) * (string list)  
                                    // is a pair (f,l) where f is the set of possible functions that wen applied
                                    // produce the results in l. Need this to piggyback the strings for when you
                                    // add more examples
                                    // Each pair (l,v) in f is all the functions in l you can apply to one var v
                                    // New convetion on var naming, var=5 means 5th chunk in symbstring                                              

type CmdMatch = CmdParams of Expr list

type ErrMatch = ErrContent of Expr list

type FixCmd = FixCmdParams of FixExpr list

type TopLevelExpr = FixRule of CmdMatch * ErrMatch * FixCmd * int //The last parameter is the number of examples

let strToSymbString (str:string) = str.Split( (null : char []), StringSplitOptions.RemoveEmptyEntries) 
                                   |> Array.toList
                                   |> (fun l -> match l with 
                                                      | [] -> []
                                                      | _ -> l)
let strToConstStr(str:string) = List.map (fun s -> ConstStr(s)) (strToSymbString str)

let str s = pstring s

let py_indent = "    "
let indent_block = List.map (fun s -> py_indent + s)
let makeMatchPred matchElem =
    let get_tok = "cur_list.pop()"
    match matchElem with
    | ConstStr s -> let pred = sprintf "if tok != \"%s\":" s in
                    let block = py_indent + "return None" in
                    [pred; block]
    | Var (i, pre, suf, _) -> let pred = sprintf "if tok.startswith(\"%s\") and tok.endswith(\"%s\"):" pre suf in
                              let true_block =  py_indent  + sprintf "var_dict[%i] = tok" i in
                              let else_ln = "else:" in
                              let false_block = py_indent + sprintf "return None" in
                              [pred; true_block; else_ln; false_block]
    | VarEq (i, _) -> let pred = sprintf "if tok != var_dict[%i]:" i in
                      let block = py_indent + "return None" in
                      [pred; block]

let makeMatchLines cMatch eMatch =
    let matchHead = "def match(command):" in
    let setupErr1 = py_indent + "err_list = command.stderr.split()"
    let setupCmd1 = py_indent + "cmd_list = list(command.script_parts)"
    let checkELen = py_indent + sprintf "if len(err_list) != %i:"  (List.length eMatch)
    let ELenBlock = py_indent + py_indent + "return False"
    let checkCLen = py_indent + sprintf "if len(cmd_list) != %i:" (List.length cMatch)
    let CLenBlock = py_indent + py_indent + "return False"
    let setupList1 = py_indent + "cur_list = err_list"
    let errMatch = List.rev eMatch |> List.map makeMatchPred |> List.concat |> indent_block in
    let setupList2 = py_indent + "cur_list = cmd_list"
    let cmdMatch = List.rev cMatch |> List.map makeMatchPred |> List.concat |> indent_block in
    [matchHead; setupErr1; setupCmd1; checkELen; ELenBlock; checkCLen; CLenBlock; setupList1] @ errMatch @ [setupList2] @ cmdMatch

let makeMatchFxn cMatch eMatch =
    makeMatchLines cMatch eMatch


let nthFromEnd ="""
def nthFromEnd(tok, target_c, n):
    count = 0
    for idx in range(-1,-(len(s) + 1),-1):
        if tok[idx] == target_c:
            if count == n:
                return (idx, True)
            count += 1
    return (0,False)
"""

let nthFromStart ="""
def nthFromStart(tok, target_c, n):
   count = 1
   for idx in range(0,len(s)):
       if tok[idx] == target_c:
           if count == n:
               return (len(s) + idx - 1, True)
           count += 1
   return (0,False)
"""

let getPosLine posVar pExpr = 
   match pExpr with
   | CPos(i) -> sprintf "%s = %i" posVar i
   | SymPos (c, instance, offset) ->
       if offset > 0 then
           sprintf "%s = nthFromStart(tok, %c, %i) + %i" posVar c instance offset
       else
           sprintf "%s = nthFromEnd(tok, %c, %i) + %i" posVar c instance offset
let makeStrOp (SubstrAndAppend ((((i,j) :: _), left, right) :: _)) idx =
    let lPosExp = getPosLine "lPos" i in
    let rPosExp = getPosLine "rPos" j in
    let repairExp = sprintf "repairedTok = %s + var_dict[%i][lPos:rPos] + %s" left idx right in
    let updateLine = "repairedToks.append(repairedTok)" in
    [lPosExp; rPosExp; repairExp; updateLine]
    

let makeFixTerm fExpr = 
    match fExpr with
    | (FixConstStr s) -> [sprintf "repairedToks.append(\"%s\")" s] 
    | (FixFuncApp (((fxn, idx) :: _), _)) -> makeStrOp fxn idx

let makeFixFxn fCmd =
    let fixHead = "def get_new_command(command):" in
    let fixDecl = py_indent + "repairedCmd = []"
    let fixLines = List.map makeFixTerm fCmd |> List.concat |> indent_block in 
    let returnFix = py_indent + "return \" \".join(fixLines)" in
    (fixHead  :: fixDecl :: (fixLines @ [returnFix]))
//Make thefuck rules from FixIt rules.
let makeTheFRuleLines (FixRule (CmdParams cMatch, ErrContent eMatch, FixCmdParams fCmd, _)) =
    (makeMatchFxn cMatch eMatch, makeFixFxn fCmd)

let makeTheFRule expr =
    let dict_decl = "var_dict = {}" in
    let enable = "enabled_by_default = True" in
    let priority = "priority = 1000" in
    let req_out = "requires_output = True" in
    let (matchLines, fixLines) = makeTheFRuleLines expr in
    let ruleText = [dict_decl; enable; priority; req_out] @ (matchLines @ fixLines) in
    String.concat "\n" ruleText

//let pipe7 p1 p2 p3 p4 p5 p6 p7 f =
//    pipe4 p1 p2 p3 (tuple4 p4 p5 p6 p7)
//          (fun x1 x2 x3 (x4, x5, x6, x7) -> f x1 x2 x3 x4 x5 x6 x7)
//
//let disallowedConstStrChars = [' '; '{'; '}'; '('; ')'; ',';':']
//let StrParser = many1Satisfy (fun c -> not (List.exists (fun elem -> c.Equals(elem)) disallowedConstStrChars))
//
//let WithSpaces p = spaces >>. p .>> spaces
//
//let VarExprParser = str "{" >>. (WithSpaces pint32) .>> str "}" |>> Var
//let ConstStrExprParser = StrParser |>> ConstStr
//
//let FixFuncAppVarParser = str "{" >>. (WithSpaces pint32) .>> str "}"
//let SubStrFunParser = pipe5 (str "SubstrAndAppend(") (WithSpaces pint32) (str ",") (WithSpaces pint32) (str ",")  (fun _ i _ j _ -> SubstrAndAppend([(i,j,"","")]))
//let IdFunParser = pipe2 (str "Id(") (spaces) (fun _ _-> SubstrAndAppend([(0,0,"","")]))
//let FixFuncAppFunParser = SubStrFunParser <|> IdFunParser
//
//let FixConstStrParser = StrParser |>> FixConstStr
////let FixFuncAppParser = pipe3 FixFuncAppFunParser FixFuncAppVarParser (str ")") (fun f v _ -> FixFuncApp([f],v))
////let FixExprParser = FixFuncAppParser <|> FixConstStrParser
//
//let CmdMatchParser = str "cmd" >>. spaces >>. str ":" >>. spaces >>. (sepBy (ConstStrExprParser <|> VarExprParser) (str " "))  |>> CmdParams
//let ErrMatchParser = str "err" >>. spaces >>. str ":" >>. spaces >>. (sepBy (ConstStrExprParser <|> VarExprParser) (str " "))  |>> ErrContent
////let FixCmdParser = str "fix" >>. spaces >>. str ":" >>. spaces >>. (sepBy FixExprParser (str " "))  |>> FixCmdParams

//let TopLevelExprParser: Parser<TopLevelExpr,unit> = pipe7 (str "{") CmdMatchParser (WithSpaces (str ",")) ErrMatchParser (WithSpaces (str ",")) FixCmdParser (str "}") (fun _ cmd _ err _ fix _ -> FixRule(cmd, err, fix))
