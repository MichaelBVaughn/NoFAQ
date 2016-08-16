module RRUtilities

type OptionBuilder() =
    member x.Bind(v,f) = Option.bind f v
    member x.Return v = Some v
    member x.ReturnFrom o = o

let option = OptionBuilder()

let fstMap f (a,b) = (f a, b)
let sndMap f (a,b) = (a, f b)

let convertToCharSeq (s:string) =  [for c in s -> c]

let rec cartesian l1 l2= 
    match (l1,l2) with
     | ([],[]) -> []
     | (xs,[]) -> []
     | ([],ys) -> []
     | (x::xs, ys) -> List.append (List.map(fun y -> x,y) ys) (cartesian xs ys)

// Finds all indices for char c in str
let findMatches (c:char) (str:char list) =
  let rec findMatchesR index clist =
    match clist with
    | h :: t ->
      if c=h
      then index :: findMatchesR(index + 1) t
      else findMatchesR (index + 1) t
    | [] -> []
  findMatchesR 0 str


let rec nConstList a (n:int) = if n=0 then [] else a:: (nConstList a (n - 1))

let rec isPrefix  (prefix :char list) (other :char list)= 
    match (prefix, other) with
    | ([],_) -> true
    | (_,[]) -> false
    | (h1::t1,h2::t2) -> if h1 = h2 then isPrefix t1 t2 else false

let isSuffix suffix other = isPrefix (List.rev suffix) (List.rev other)

let matchPrefixAndSuffix (prefix :char list ) (suffix :char list) (other :char list) =
    if prefix.Length + suffix.Length <= other.Length then
       (isPrefix prefix other) && (isSuffix suffix other)
    else
       false

let rec longestCommonPrefix (str1: char list) (str2:char list): char list =
    match (str1, str2) with
    | ([],_) -> []
    | (_,[]) -> []
    | (h1::t1,h2::t2) -> if h1 = h2 then h1::(longestCommonPrefix t1 t2) else []

let longestCommonSuffix (str1: char list) (str2:char list) : char list  = List.rev (longestCommonPrefix (List.rev str1) (List.rev str2))

let rec findLongestCommonPrefSufRec (strs: string list) (prefSoFar: char list) (suffSoFar: char list) : (char list * char list) =
    match strs with 
        | []  -> (prefSoFar,suffSoFar)
        | str::t -> 
            let charSeq = convertToCharSeq str in 
            let pref = longestCommonPrefix charSeq prefSoFar in 
            let suf = longestCommonSuffix charSeq suffSoFar in 
            findLongestCommonPrefSufRec t pref suf

let findLongestCommonPrefSuf (strs: string list) : (char list * char list) =
        match strs with         
        | str::t -> 
            let charSeq = convertToCharSeq str in 
            findLongestCommonPrefSufRec t charSeq charSeq
        | []  -> raise (System.ArgumentException("This should not be called on empty list"))

//Generates all "growth-restricted" strings in lexicographic order - an ancillary algorithm for enumerating all partitions of a set
//There is a 1-1 correspondence between partitions of a set and growth-restricted strings
//This is algorithm H from Knuth's The Art of Computer Programming Volume 4a Fascicle 3B, page 26
let enumerateGrowthRestrictedStrings (len: int) = 
    let a = [| for i in 1 .. len -> 0 |]
    let b = [| for i in 1 .. len ->  1 |] 
    let mutable isDone = false 
    let mutable j = len - 1
    seq{while not isDone do
            yield a
            if a.[len - 1] <> b.[len - 1] then
                a.[len - 1] <- a.[len - 1] + 1
            else 
                j <- len - 1
                while a.[j] = b.[j] do
                    j <- j - 1
                if j = 0 then
                    isDone <- true
                else
                    a.[j] <- a.[j] + 1
                    if a.[j] = b.[j] then
                        b.[len - 1] <- b.[j] + 1
                    else
                        b.[len - 1] <- b.[j]
                    j <- j + 1
                    a.[len - 1] <- 0
                    while j < len - 1 do
                        a.[j] <- 0
                        b.[j] <- b.[len - 1]
                        j <- j + 1            
               }

//extend the list at set_idx in the mapping with item
let updateSetMap mapping (item, set_idx) =
    let key_found = Map.containsKey set_idx mapping in
    let old_lst = match key_found with
                  | false -> []
                  | true -> mapping.[set_idx] in                  
    Map.add set_idx (item :: old_lst) mapping

//A utility function which converts a partition to a string representation.
//note that the list being partitioned must contain elements of a type which 
//has access to ToString()
let rec setPartitionToStr (partition: Map<int,'b List>) idx =
    let listToStr setList = (List.map (fun i -> i.ToString()) setList) |> String.concat ""  in
    let rest = match idx with 
               | 0 -> ""
               | _ -> setPartitionToStr partition (idx - 1) in
    let subsetStr = match Map.containsKey idx partition with
                    | false -> ""
                    | true ->  "|" + (listToStr partition.[idx]) in
    rest + subsetStr
//Given a growth-restricted string and a list of the same cardinality,
//partition the list 
//The resulting partition is a map from subset index to a list of the subset's contents
let GRstrToPartition gr_str (set_list : 'a List) =
    let tmp = [1 .. set_list.Length] in
    let tmp_zipped = List.zip tmp gr_str in
    let tmp_part = List.fold (updateSetMap) Map.empty tmp_zipped in
    let part_str = setPartitionToStr tmp_part set_list.Length  in
    let zipped = List.zip set_list gr_str in
    (List.fold (updateSetMap) Map.empty zipped, part_str)
        

//Given a list, enumerate all partitions of the list.
let enumerateSetListPartitions (setList: 'a List)=
    let grStrings = enumerateGrowthRestrictedStrings setList.Length in
    Seq.map (fun grStr -> GRstrToPartition (Seq.toList grStr) setList) grStrings



let setToList s = Set.fold (fun l se -> se::l) [] s