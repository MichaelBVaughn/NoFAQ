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
