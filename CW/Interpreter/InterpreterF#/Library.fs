namespace InterpreterFSharp

module LexerParser =
    open System

    type Number =
        Int of int | Float of float

    type terminal = 
        Add | Sub | Mul | Div | Rem | Pow | Lpar | Rpar | Equ | Vid of string | Num of Number | Neg | Plus | Err of char

    type 'a result = 
        Success of 'a | Failure of string

    let str2lst s = [for c in s -> c]
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c = System.Char.IsDigit c
    let ischar c = System.Char.IsLetter c
    let lexError = System.Exception("Lexer error")
    let intVal (c:char) = (int)((int)c - (int)'0')
    let floatVal (c:char) = (float)((int)c - (int)'0')
    let symError = System.Exception("No value associated to variable name")
    let parseError = System.Exception("Parser error")


    let rec scFloat(iStr, iVal) =
        let v = iVal
        match iStr with
        c :: tail when isdigit c -> scFloat(tail, v + floatVal c / 10.0)
        | _ -> (iStr, v)

    let rec scInt(iStr, iVal) =
        match iStr with
        c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
        |'.'::tail ->
            let (b, n) = scFloat(tail, float iVal)
            (b, Float n)
        | _ -> (iStr, Int iVal)

    let rec scChar(iStr, vName:string) =
        match iStr with
        | c :: tail when ischar c -> scChar(tail,(vName + c.ToString()))
        | _ -> (iStr, vName)

    let isNeg (num: Number) =
        match num with
        | Number.Int n -> n < 0
        | Number.Float f -> f < 0.0

    let rec check4vid sList vName value =  // added to update symbol table list if already existing vName is overwritten
        match sList with
        | head :: tail -> if (fst head) = vName then [(vName,value)]@(check4vid tail vName value) // replace original value
                          else [head]@(check4vid tail vName value) // copy original value
        | _ -> []


    //============================= lexer ========================================
    let lexer input = 
        let rec isUnary prevChar =
            match prevChar with
            | [] -> true  // Previous character is empty, indicating the start of the input.
            | '=':: _ -> true
            | '^':: _ -> true
            | '(' :: _ -> true  // Previous character is '(', indicating unary minus.
            | '+' :: _ -> true
            | '-' :: _ -> true
            | _ -> false

        let rec scan prevChar input =
         
            match input with
            | [] -> []
            | '+'::tail ->
                if isUnary prevChar then
                    Plus :: scan ['+'] tail
                else
                    Add :: scan ['+'] tail
            | '-'::tail ->
                if isUnary prevChar then
                    Neg :: scan ['-'] tail
                else
                    Sub :: scan ['-'] tail
            | '*'::tail -> Mul :: scan ['*'] tail
            | '/'::tail -> Div :: scan ['/'] tail
            | '%'::tail -> Rem :: scan ['%'] tail
            | '^'::tail -> Pow :: scan ['^'] tail
            | '('::tail -> Lpar:: scan ['('] tail
            | ')'::tail -> Rpar:: scan [')'] tail
            | '='::tail -> Equ :: scan ['=']tail
            | c :: tail when isblank c -> scan [c] tail
            | c :: tail when isdigit c -> 
                let (iStr, iVal) = scInt(tail, intVal c) 
                Num iVal :: scan [c] iStr
            | c :: tail when ischar c -> let (iStr, vName) = scChar(tail, c.ToString())
                                         Vid vName :: scan [c] iStr
            | c :: tail -> Err c :: scan [c] tail
            
        
        scan [] (str2lst input)



    //============================ Lexer =========================================  

    let getInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()
    
    // Grammar in (E)BNF:
    //<VA> ::= <varID> "=" <E>
    //<E> ::= <T> <Eopt>
    //<Eopt> ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    //<T> ::= <P> <Topt>
    //<Topt> ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
    //<P> ::= <NR> <Popt>
    //<Popt> ::= "^" <NR> <Popt> | <empty>
    //<NR> ::= ["Num" | "varVal" ] <value> | "(" <E> ")" | "-" <NR> | "+" <NR>
    //<varID> ::= [a-z,A-Z]+

    ////============================= Parser ======================================
    let parser tList = 
        let rec E (tList: result<terminal list>) = 
            match (T >> Eopt) tList with
            | Success res -> Success res
            | Failure error -> Failure (error)
        and Eopt (tList: result<terminal list>) =             
            match tList with
            | Success (Add :: tail) -> (T >> Eopt) (Success tail)
            | Success (Sub :: tail) -> (T >> Eopt) (Success tail)
            | Failure error -> Failure error
            | _ -> tList
        and T (tList: result<terminal list>) = 
            match (P >> Topt) tList with
            | Success res -> Success res
            | Failure error -> Failure (error)
        and Topt (tList: result<terminal list>) =
            match tList with
            | Success (Mul :: tail) -> (P >> Topt) (Success tail)
            | Success (Div :: tail) -> (P >> Topt) (Success tail)
            | Success (Rem :: tail) -> (P >> Topt) (Success tail)
            | Failure error -> Failure error
            | _ -> tList
        and P (tList: result<terminal list>) =   
            match (NR >> Popt) tList with
            | Success res -> Success res
            | Failure error -> Failure (error)
        and Popt (tList: result<terminal list>) =
            match tList with
            | Success (Pow :: tail) -> (NR >> Popt) (Success tail)
            | Failure error -> Failure error
            | _ -> tList
        and NR (tList: result<terminal list>) =
            match tList with 
            | Success (Neg :: Num value :: tail) -> Success tail
            | Success (Neg :: Lpar :: tail) -> match E (Success tail) with
                                               | Success (Rpar :: tail) -> Success tail
                                               | Success _ -> Failure "Missing right parenthesis"
                                               | Failure error -> Failure error
            | Success (Plus :: Num value :: tail) -> Success tail
            | Success (Num value :: tail) -> Success tail
            | Success (Vid vName :: tail) -> Success tail
            | Success (Lpar :: tail) -> match E (Success tail) with 
                                        | Success (Rpar :: tail) -> Success tail
                                        | Success _ -> Failure "Missing right parenthesis"
                                        | Failure error -> Failure error
            | Failure error -> Failure error
            | c -> Failure ("Unexpected token \"" + c.ToString() + "\"")
        let VA tList =  
            match tList with
            | Vid vName :: tail -> match tail with
                                   | Equ :: tail -> E (Success tail)
                                   | _ -> Failure "Missing equal sign after variable name"
            | _ -> E (Success tList)
        VA tList 

    let rec searchVName vName (symList:List<string*Number>) =
        match symList with
        | head :: tail -> if (fst head) = vName then (true, (snd head))
                          else searchVName vName tail
        | _ -> (false, Number.Int 0)

    let parseNeval tList (symList:List<string*Number>) =
        let rec E tList = (T >> Eopt) tList
        and Eopt (tList, (vID, value)) =
            match tList with
            | Add :: tail -> 
                let (tLst, (vID, tval)) = T tail
                Eopt (tLst, (vID,
                    match value, tval with
                    | Number.Int a, Number.Int b -> Number.Int (a + b)
                    | Number.Float a, Number.Float b -> Number.Float (a + b)
                    | Number.Int a, Number.Float b -> Number.Float (float a + b)
                    | Number.Float a, Number.Int b -> Number.Float (a + float b)
                ))
            | Sub :: tail -> 
                let (tLst, (vID, tval)) = T tail
                Eopt (tLst, (vID,
                    match value, tval with
                    | Number.Int a, Number.Int b -> Number.Int (a - b)
                    | Number.Float a, Number.Float b -> Number.Float (a - b)
                    | Number.Int a, Number.Float b -> Number.Float (float a - b)
                    | Number.Float a, Number.Int b -> Number.Float (a - float b)
                ))
            | _ -> (tList, ("", value))
        and T tList = (P >> Topt) tList
        and Topt (tList, (vID, value)) =
            match tList with
            | Mul :: tail -> 
                let (tLst, (vID, tval)) = P tail
                Topt (tLst, (vID,
                    match value, tval with
                    | Number.Int a, Number.Int b -> Number.Int (a * b)
                    | Number.Float a, Number.Float b -> Number.Float (a * b)
                    | Number.Int a, Number.Float b -> Number.Float (float a * b)
                    | Number.Float a, Number.Int b -> Number.Float (a * float b)
                ))
            | Div :: tail -> 
                let (tLst, (vID, tval)) = P tail
                Topt (tLst, (vID, 
                    match value, tval with
                    | Number.Int a, Number.Int b -> Number.Int (a / b)
                    | Number.Float a, Number.Float b -> Number.Float (a / b)
                    | Number.Int a, Number.Float b -> Number.Float (float a / b)
                    | Number.Float a, Number.Int b -> Number.Float (a / float b)
                ))
            | Rem :: tail -> 
                let (tLst, (vID, tval)) = P tail
                Topt (tLst, (vID, 
                    match value, tval with
                    | Number.Int a, Number.Int b -> Number.Int (a % b)
                    | Number.Float a, Number.Float b -> Number.Float (a % b)
                    | Number.Int a, Number.Float b -> Number.Float (float a % b)
                    | Number.Float a, Number.Int b -> Number.Float (a % float b)
                ))
            | _ -> (tList, ("", value))
        and P tList = (NR >> Popt) tList
        and Popt (tList, (vID, value)) = 
            match tList with
            | Pow :: tail -> 
                let (tLst, (vID, tval)) = NR tail
                Popt (tLst, (vID, 
                    match value, tval with
                    | Number.Int a, Number.Int b ->
                        if isNeg tval then
                            Number.Float (float a ** float b)
                        else
                            Number.Int (pown a (int b))
                    | Number.Float a, Number.Float b ->
                        if isNeg tval then
                            Number.Float (float a ** float b)
                        else
                            Number.Float (pown a (int b))
                    | Number.Float a, Number.Int b ->
                        if isNeg tval then
                            Number.Float (float a ** float b)
                        else
                            Number.Float (float(pown (int a) b))
                    | Number.Int a, Number.Float b ->
                        if isNeg tval then
                            Number.Float (float a ** float b)
                        else
                            Number.Int (pown a (int b))
                ))
            | _ -> (tList, ("", value))
        and NR tList =
            match tList with 
            | Neg :: Num value :: tail -> 
                (tail, ("",
                    match value with
                    | Int a -> Int (-a)
                    | Float a -> Float (-a)))
            |Neg :: Lpar :: tail ->
                let (tLst, (vID, tval)) = E tail
                match tLst with 
                | Rpar :: tail ->
                    (tail, ("",
                        match tval with
                        | Int a -> Int (-a)
                        | Float a -> Float (-a)))
                | _ -> raise parseError
            | Plus :: Num value :: tail -> (tail, ("", value))
            | Vid vName :: tail -> let res = searchVName vName symList
                                   if (fst res) then (tail, ("", (snd res)))
                                   else raise symError
            | Num value :: tail -> (tail, ("", value))
            | Lpar :: tail -> 
                let (tLst, (vID, tval)) = E tail
                match tLst with 
                | Rpar :: tail -> (tail, ("", tval))
                | _ -> raise parseError
            | _ -> raise parseError
        let VA tList = 
            match tList with 
            | Vid vName :: tail -> match tail with 
                                   | Equ :: tail -> let (tLst, (vID, tval)) = E tail
                                                    (tLst, (vName, tval))
                                   | _ -> E tList
            | _ -> E tList
        VA tList

    let parseNevalNsym tList (symList:List<string*Number>) =
        let pNe = parseNeval tList symList
        match fst (snd pNe) with
          | "" -> (pNe, symList) // if there is no vID just return the symList
          | _ -> match symList with
                 | [] -> (pNe, symList @ [(fst (snd pNe)), (snd (snd pNe))])
                 | _ -> let res = check4vid symList (fst (snd pNe)) (snd (snd pNe)) // if the vID is already in symbol table replace its value
                        (pNe, res)
                 

    ////============================= Parser ======================================

    let rec printTList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTList tail
                  
        | [] -> Console.Write("EOL\n")
                []

    let rec printSymTList (sList:List<string*Number>)  =
        match sList with
        | head :: [] -> Console.Write("{0}", head)
                        printSymTList []
        | head :: tail -> Console.Write("{0};", head)
                          printSymTList tail
        | [] -> Console.WriteLine("]")

    let rec inpLoop (symTList:List<string*Number>) = 
        Console.Write("Symbol Table = [")
        let outSym = printSymTList symTList
        let input = getInputString()
        if input <> "" then
            let oList = lexer input
            let sList = printTList oList
            //let pList = parser oList  // pList is the remaining token list and should be empty
            //if not pList.IsEmpty then raise parseError // NOTE this update to avoid expressions like 3(2+3) that would return a value of 3 and have a nonempty token list ([Lpar Num 2 Add Num 3 Rpar], 3)
            let Out = parseNeval oList symTList
            let tempID = fst (snd Out)
            let tempVal = snd (snd Out)
            Console.WriteLine("Variable name = {0}", tempID)    // UPDATE
            Console.WriteLine("Result = {0}", tempVal)          // UPDATED
            // Check whether variable name was already in symTList and if so replace with new value
            if tempID.Length > 0 then // update symbol table
                if symTList.IsEmpty then 
                    inpLoop (symTList@[tempID, tempVal])  // append new value if symbol table is empty
                else 
                    let res = check4vid symTList tempID tempVal // if tempID is already in symbol table replace its value
                    let check = res.Equals(symTList)      // Check whether res is equal to the original (means no replacing was done)
                    if check then inpLoop (symTList@[tempID, tempVal])  // if true pass old list with appended new tuple                 
                    else inpLoop res   // if false pass updated res list with updated tuple
            else inpLoop symTList 
        else symTList


    [<EntryPoint>]
    let main argv  =
        Console.WriteLine("Simple Interpreter")
        let res = inpLoop [] 
        Console.WriteLine("Symbol table is {0}", res )
        0


    //[<EntryPoint>]
    //let main argv  =
    //    Console.WriteLine("Simple Interpreter")
    //    while true do
    //        let input:string = getInputString()
    //        let oList = lexer input
    //        let sList = printTList oList;
    //        Console.WriteLine("")
    //        //let pList = printTList (parser oList)
    //        let Out = parseNeval oList
    //        Console.WriteLine("Result = {0}", snd Out)
    //    0