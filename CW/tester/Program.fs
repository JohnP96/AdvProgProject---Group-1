open System

    type Number =
        Int of int | Float of float

    type terminal = 
        Add | Sub | Mul | Div | Rem | Pow | Lpar | Rpar | Equ | Vid of string | Num of Number| Neg | Plus

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


    //============================= lexer ========================================
    let lexer input = 
        let rec isBlankOrOpenParen prevChar =
            match prevChar with
            | [] -> true  // Previous character is empty, indicating the start of the input.
            | '=':: _ -> true
            | '^':: _ -> true
            | '(' :: _ -> true  // Previous character is '(', indicating unary minus.
            | _ -> false

        let rec scan prevChar input =
         
            match input with
            | [] -> []
            | '+'::tail ->
                if isBlankOrOpenParen prevChar then
                    Plus :: scan ['+'] tail
                else
                    Add :: scan ['+'] tail
            | '-'::tail ->
                if isBlankOrOpenParen prevChar then
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
            | _ -> raise lexError
            
        
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
    //let parser tList = 
    //    let rec E tList = (T >> Eopt) tList     // NOTE: >> is (forward) function composition
    //    and Eopt tList =                        // 'and' allows for mutual recursion
    //        match tList with
    //        | Add :: tail -> (T >> Eopt) tail
    //        | Sub :: tail -> (T >> Eopt) tail
    //        | _ -> tList
    //    and T tList = (P >> Topt) tList
    //    and Topt tList =
    //        match tList with
    //        | Mul :: tail -> (P >> Topt) tail
    //        | Div :: tail -> (P >> Topt) tail
    //        | Rem :: tail -> (P >> Topt) tail
    //        | _ -> tList
    //    and P tList = (NR >> Popt) tList
    //    and Popt tList =
    //        match tList with
    //        | Pow :: tail -> (NR >> Popt) tail
    //        | _ -> tList
    //    and NR tList =
    //        match tList with 
    //        | Num value :: tail -> tail
    //        | Lpar :: tail -> match E tail with 
    //                          | Rpar :: tail -> tail
    //                          | _ -> raise parseError
    //        | _ -> raise parseError
    //    E tList

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

    ////============================= Parser ======================================

    let rec printTList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTList tail
                  
        | [] -> Console.Write("EOL\n")
                []

    let rec check4vid sList vName value =  // added to update symbol table list if already existing vName is overwritten
        match sList with
        | head :: tail -> if (fst head) = vName then [(vName,value)]@(check4vid tail vName value) // replace original value
                          else [head]@(check4vid tail vName value) // copy original value
        | _ -> []

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


















































    //Working Code


//open System

//type Number =
//    Int of int | Float of float

//type terminal = 
//    Add | Sub | Mul | Div | Rem | Pow | Lpar | Rpar | Num of Number| Neg | Plus

//let str2lst s = [for c in s -> c]
//let isblank c = System.Char.IsWhiteSpace c
//let isdigit c = System.Char.IsDigit c
//let lexError = System.Exception("Lexer error")
//let intVal (c:char) = (int)((int)c - (int)'0')
//let floatVal (c:char) = (float)((int)c - (int)'0')

//let parseError = System.Exception("Parser error")


//let rec scFloat(iStr, iVal) =
//    let v = iVal
//    match iStr with
//    c :: tail when isdigit c -> scFloat(tail, v + floatVal c / 10.0)
//    | _ -> (iStr, v)

//let rec scInt(iStr, iVal) =
//    match iStr with
//    c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
//    |'.'::tail ->
//        let (b, n) = scFloat(tail, float iVal)
//        (b, Float n)
//    | _ -> (iStr, Int iVal)




////============================= lexer ========================================
//let lexer input = 
//    let rec isBlankOrOpenParen prevChar =
//        match prevChar with
//        | [] -> true  // Previous character is empty, indicating the start of the input.
//        | '(' :: _ -> true  // Previous character is '(', indicating unary minus.
//        | _ -> false

//    let rec scan prevChar input =
//        match input with
//        | [] -> []
//        | '+'::tail ->
//            if isBlankOrOpenParen prevChar then
//                Plus :: scan ['+'] tail
//            else
//                Add :: scan ['+'] tail
//        | '-'::tail ->
//            if isBlankOrOpenParen prevChar then
//                Neg :: scan ['-'] tail
//            else
//                Sub :: scan ['-'] tail
//        | '*'::tail -> Mul :: scan ['*'] tail
//        | '/'::tail -> Div :: scan ['/'] tail
//        | '%'::tail -> Rem :: scan ['%'] tail
//        | '^'::tail -> Pow :: scan ['^'] tail
//        | '('::tail -> Lpar:: scan ['('] tail
//        | ')'::tail -> Rpar:: scan [')'] tail
//        | c :: tail when isblank c -> scan [c] tail
//        | c :: tail when isdigit c -> 
//            let (iStr, iVal) = scInt(tail, intVal c) 
//            Num iVal :: scan [c] iStr
//        | _ -> raise lexError
//    scan [] (str2lst input)



////============================ Lexer =========================================  

//let getInputString() : string = 
//    Console.Write("Enter an expression: ")
//    Console.ReadLine()
    
//// Grammar in BNF:
////<E> ::= <T> <Eopt>
////<Eopt> ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
////<T> ::= <P> <Topt>
////<Topt> ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
////<P> ::= <NR> <Popt>
////<Popt> ::= "^" <NR> <Popt> | <empty>
////<NR> ::= "Num" <value> | "(" <E> ")" | "-" <NR> | "+" <NR>

//////============================= Parser ======================================
////let parser tList = 
////    let rec E tList = (T >> Eopt) tList     // NOTE: >> is (forward) function composition
////    and Eopt tList =                        // 'and' allows for mutual recursion
////        match tList with
////        | Add :: tail -> (T >> Eopt) tail
////        | Sub :: tail -> (T >> Eopt) tail
////        | _ -> tList
////    and T tList = (P >> Topt) tList
////    and Topt tList =
////        match tList with
////        | Mul :: tail -> (P >> Topt) tail
////        | Div :: tail -> (P >> Topt) tail
////        | Rem :: tail -> (P >> Topt) tail
////        | _ -> tList
////    and P tList = (NR >> Popt) tList
////    and Popt tList =
////        match tList with
////        | Pow :: tail -> (NR >> Popt) tail
////        | _ -> tList
////    and NR tList =
////        match tList with 
////        | Num value :: tail -> tail
////        | Lpar :: tail -> match E tail with 
////                          | Rpar :: tail -> tail
////                          | _ -> raise parseError
////        | _ -> raise parseError
////    E tList

//let parseNeval tList =
//    let rec E tList = (T >> Eopt) tList
//    and Eopt (tList, value) =
//        match tList with
//        | Add :: tail -> 
//            let (tLst, tval) = T tail
//            Eopt (tLst, 
//                match value, tval with
//                | Number.Int a, Number.Int b -> Number.Int (a + b)
//                | Number.Float a, Number.Float b -> Number.Float (a + b)
//                | Number.Int a, Number.Float b -> Number.Float (float a + b)
//                | Number.Float a, Number.Int b -> Number.Float (a + float b)
//            )
//        | Sub :: tail -> 
//            let (tLst, tval) = T tail
//            Eopt (tLst, 
//                match value, tval with
//                | Number.Int a, Number.Int b -> Number.Int (a - b)
//                | Number.Float a, Number.Float b -> Number.Float (a - b)
//                | Number.Int a, Number.Float b -> Number.Float (float a - b)
//                | Number.Float a, Number.Int b -> Number.Float (a - float b)
//            )
//        | _ -> (tList, value)
//    and T tList = (P >> Topt) tList
//    and Topt (tList, value) =
//        match tList with
//        | Mul :: tail -> 
//            let (tLst, tval) = P tail
//            Topt (tLst, 
//                match value, tval with
//                | Number.Int a, Number.Int b -> Number.Int (a * b)
//                | Number.Float a, Number.Float b -> Number.Float (a * b)
//                | Number.Int a, Number.Float b -> Number.Float (float a * b)
//                | Number.Float a, Number.Int b -> Number.Float (a * float b)
//            )
//        | Div :: tail -> 
//            let (tLst, tval) = P tail
//            Topt (tLst, 
//                match value, tval with
//                | Number.Int a, Number.Int b -> Number.Int (a / b)
//                | Number.Float a, Number.Float b -> Number.Float (a / b)
//                | Number.Int a, Number.Float b -> Number.Float (float a / b)
//                | Number.Float a, Number.Int b -> Number.Float (a / float b)
//            )
//        | Rem :: tail -> 
//            let (tLst, tval) = P tail
//            Topt (tLst, 
//                match value, tval with
//                | Number.Int a, Number.Int b -> Number.Int (a % b)
//                | Number.Float a, Number.Float b -> Number.Float (a % b)
//                | Number.Int a, Number.Float b -> Number.Float (float a % b)
//                | Number.Float a, Number.Int b -> Number.Float (a % float b)
//            )
//        | _ -> (tList, value)
//    and P tList = (NR >> Popt) tList
//    and Popt (tList, value) = 
//        match tList with
//        | Pow :: tail -> 
//            let (tLst, tval) = NR tail
//            Popt (tLst, 
//                match value, tval with
//                | Number.Int a, Number.Int b -> Number.Int (pown a b)
//                | Number.Float a, Number.Float b -> Number.Float (pown a (int b))
//                | Number.Float a, Number.Int b -> Number.Float (float(pown (int a) b))
//                | Number.Int a, Number.Float b -> Number.Int (pown  a (int b))
//            )
//        | _ -> (tList, value)
//    and NR tList =
//        match tList with 
//        | Neg :: Num value :: tail -> 
//            (tail, 
//                match value with
//                | Int a -> Int (-a)
//                | Float a -> Float (-a))
//        | Plus :: Num value :: tail -> (tail, value)
//        | Num value :: tail -> (tail, value)
//        | Lpar :: tail -> 
//            let (tLst, tval) = E tail
//            match tLst with 
//            | Rpar :: tail -> (tail, tval)
//            | _ -> raise parseError
//        | _ -> raise parseError
//    E tList

//////============================= Parser ======================================

//let rec printTList (lst:list<terminal>) : list<string> = 
//    match lst with
//    head::tail -> Console.Write("{0} ",head.ToString())
//                  printTList tail
                  
//    | [] -> Console.Write("EOL\n")
//            []


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





























    //OLd Old Code



















//open System

//type terminal = 
//    Add | Sub | Mul | Div | Rem | Pow | Lpar | Rpar | Num of int | Neg | Plus

//let str2lst s = [for c in s -> c]
//let isblank c = System.Char.IsWhiteSpace c
//let isdigit c = System.Char.IsDigit c
//let lexError = System.Exception("Lexer error")
//let intVal (c:char) = (int)((int)c - (int)'0')
//let parseError = System.Exception("Parser error")

//let rec scInt(iStr, iVal) = 
//    match iStr with
//    c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
//    | _ -> (iStr, iVal)


////============================= lexer ========================================
//let lexer input = 
//    let rec isBlankOrOpenParen prevChar =
//        match prevChar with
//        | [] -> true  // Previous character is empty, indicating the start of the input.
//        | '(' :: _ -> true  // Previous character is '(', indicating unary minus.
//        | _ -> false

//    let rec scan prevChar input =
//        match input with
//        | [] -> []
//        | '+'::tail ->
//            if isBlankOrOpenParen prevChar then
//                Plus :: scan ['+'] tail
//            else
//                Add :: scan ['+'] tail
//        | '-'::tail ->
//            if isBlankOrOpenParen prevChar then
//                Neg :: scan ['-'] tail
//            else
//                Sub :: scan ['-'] tail
//        | '*'::tail -> Mul :: scan ['*'] tail
//        | '/'::tail -> Div :: scan ['/'] tail
//        | '%'::tail -> Rem :: scan ['%'] tail
//        | '^'::tail -> Pow :: scan ['^'] tail
//        | '('::tail -> Lpar:: scan ['('] tail
//        | ')'::tail -> Rpar:: scan [')'] tail
//        | c :: tail when isblank c -> scan [c] tail
//        | c :: tail when isdigit c -> 
//            let (iStr, iVal) = scInt(tail, intVal c) 
//            Num iVal :: scan [c] iStr
//        | _ -> raise lexError
//    scan [] (str2lst input)



////============================ Lexer =========================================  

//let getInputString() : string = 
//    Console.Write("Enter an expression: ")
//    Console.ReadLine()
    
//// Grammar in BNF:
////<E> ::= <T> <Eopt>
////<Eopt> ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
////<T> ::= <P> <Topt>
////<Topt> ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
////<P> ::= <NR> <Popt>
////<Popt> ::= "^" <NR> <Popt> | <empty>
////<NR> ::= "Num" <value> | "(" <E> ")" | "-" <NR> | "+" <NR>

////============================= Parser ======================================
//let parser tList = 
//    let rec E tList = (T >> Eopt) tList     // NOTE: >> is (forward) function composition
//    and Eopt tList =                        // 'and' allows for mutual recursion
//        match tList with
//        | Add :: tail -> (T >> Eopt) tail
//        | Sub :: tail -> (T >> Eopt) tail
//        | _ -> tList
//    and T tList = (P >> Topt) tList
//    and Topt tList =
//        match tList with
//        | Mul :: tail -> (P >> Topt) tail
//        | Div :: tail -> (P >> Topt) tail
//        | Rem :: tail -> (P >> Topt) tail
//        | _ -> tList
//    and P tList = (NR >> Popt) tList
//    and Popt tList =
//        match tList with
//        | Pow :: tail -> (NR >> Popt) tail
//        | _ -> tList
//    and NR tList =
//        match tList with 
//        | Num value :: tail -> tail
//        | Lpar :: tail -> match E tail with 
//                          | Rpar :: tail -> tail
//                          | _ -> raise parseError
//        | _ -> raise parseError
//    E tList

//let parseNeval tList = 
//    let rec E tList = (T >> Eopt) tList
//    and Eopt (tList, value) = 
//        match tList with
//        | Add :: tail -> let (tLst, tval) = T tail
//                         Eopt (tLst, value + tval)
//        | Sub :: tail -> let (tLst, tval) = T tail
//                         Eopt (tLst, value - tval)
//        | _ -> (tList, value)
//    and T tList = (P >> Topt) tList
//    and Topt (tList, value) =
//        match tList with
//        | Mul :: tail -> let (tLst, tval) = P tail
//                         Topt (tLst, value * tval)
//        | Div :: tail -> let (tLst, tval) = P tail
//                         Topt (tLst, value / tval)
//        | Rem :: tail -> let (tLst, tval) = P tail
//                         Topt (tLst, value % tval)
//        | _ -> (tList, value)
//    and P tList = (NR >> Popt) tList
//    and Popt (tList, value)= 
//        match tList with
//        | Pow :: tail -> let (tLst, tval) = NR tail
//                         Popt (tLst, pown value tval)
//        | _ -> (tList, value)
//    and NR tList =
//        match tList with 
//        | Neg :: Num value :: tail -> (tail, value * -1)
//        | Plus :: Num value :: tail -> (tail, value * 1)
//        | Num value :: tail -> (tail, value)
//        | Lpar :: tail -> let (tLst, tval) = E tail
//                          match tLst with 
//                          | Rpar :: tail -> (tail, tval)
//                          | _ -> raise parseError
//        | _ -> raise parseError
//    E tList
////============================= Parser ======================================

//let rec printTList (lst:list<terminal>) : list<string> = 
//    match lst with
//    head::tail -> Console.Write("{0} ",head.ToString())
//                  printTList tail
                  
//    | [] -> Console.Write("EOL\n")
//            []


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
