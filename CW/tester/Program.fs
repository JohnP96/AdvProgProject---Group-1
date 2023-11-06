open System

type Number =
    Int of int | Float of float

type terminal = 
    Add | Sub | Mul | Div | Rem | Pow | Lpar | Rpar | Num of Number| Neg | Plus
    | Asn of string | Eql

let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let ischar c = System.Char.IsLetter c // UPDATE
let lexError = System.Exception("Lexer error")
let intVal (c:char) = (int)((int)c - (int)'0')
let floatVal (c:char) = (float)((int)c - (int)'0')

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
        | '='::tail -> Eql::scan ['='] tail 
        | c :: tail when isblank c -> scan [c] tail
        | c :: tail when isdigit c -> 
            let (iStr, iVal) = scInt(tail, intVal c) 
            Num iVal :: scan [c] iStr
        | c :: tail when ischar c -> let (iStr, vName) = scChar(tail, c.ToString() ) // UPDATE
                                     Asn vName :: scan [c] iStr
        | _ -> raise lexError
    scan [] (str2lst input)



//============================ Lexer =========================================  

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()
    
// Grammar in BNF:
//<E> ::= <T> <Eopt>
//<Eopt> ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
//<T> ::= <P> <Topt>
//<Topt> ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
//<P> ::= <NR> <Popt>
//<Popt> ::= "^" <NR> <Popt> | <empty>
//<NR> ::= "Num" <value> | "(" <E> ")" | "-" <NR> | "+" <NR>

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

let parseNeval tList =
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) =
        match tList with
        | Add :: tail -> 
            let (tLst, tval) = T tail
            Eopt (tLst, 
                match value, tval with
                | Number.Int a, Number.Int b -> Number.Int (a + b)
                | Number.Float a, Number.Float b -> Number.Float (a + b)
                | Number.Int a, Number.Float b -> Number.Float (float a + b)
                | Number.Float a, Number.Int b -> Number.Float (a + float b)
            )
        | Sub :: tail -> 
            let (tLst, tval) = T tail
            Eopt (tLst, 
                match value, tval with
                | Number.Int a, Number.Int b -> Number.Int (a - b)
                | Number.Float a, Number.Float b -> Number.Float (a - b)
                | Number.Int a, Number.Float b -> Number.Float (float a - b)
                | Number.Float a, Number.Int b -> Number.Float (a - float b)
            )
        | _ -> (tList, value)
    and T tList = (P >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Mul :: tail -> 
            let (tLst, tval) = P tail
            Topt (tLst, 
                match value, tval with
                | Number.Int a, Number.Int b -> Number.Int (a * b)
                | Number.Float a, Number.Float b -> Number.Float (a * b)
                | Number.Int a, Number.Float b -> Number.Float (float a * b)
                | Number.Float a, Number.Int b -> Number.Float (a * float b)
            )
        | Div :: tail -> 
            let (tLst, tval) = P tail
            Topt (tLst, 
                match value, tval with
                | Number.Int a, Number.Int b -> Number.Int (a / b)
                | Number.Float a, Number.Float b -> Number.Float (a / b)
                | Number.Int a, Number.Float b -> Number.Float (float a / b)
                | Number.Float a, Number.Int b -> Number.Float (a / float b)
            )
        | Rem :: tail -> 
            let (tLst, tval) = P tail
            Topt (tLst, 
                match value, tval with
                | Number.Int a, Number.Int b -> Number.Int (a % b)
                | Number.Float a, Number.Float b -> Number.Float (a % b)
                | Number.Int a, Number.Float b -> Number.Float (float a % b)
                | Number.Float a, Number.Int b -> Number.Float (a % float b)
            )
        | _ -> (tList, value)
    and P tList = (NR >> Popt) tList
    and Popt (tList, value) = 
        match tList with
        | Pow :: tail -> 
            let (tLst, tval) = NR tail
            Popt (tLst, 
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
            )
        | _ -> (tList, value)

    and NR tList =
        match tList with 
        | Neg :: Num value :: tail -> 
            (tail, 
                match value with
                | Int a -> Int (-a)
                | Float a -> Float (-a))
        |Neg :: Lpar :: tail ->
            let (tLst, tval) = E tail
            match tLst with 
            | Rpar :: tail ->
                (tail,
                    match tval with
                    | Int a -> Int (-a)
                    | Float a -> Float (-a))
                    | _ -> raise parseError

        | Plus :: Num value :: tail -> (tail, value)
        | Num value :: tail -> (tail, value)
        //| Asn vName :: tail -> (tail, vName)
        | Lpar :: tail -> 
            let (tLst, tval) = E tail
            match tLst with 
            | Rpar :: tail -> (tail, tval)
            | _ -> raise parseError
        | _ -> raise parseError
    E tList

////============================= Parser ======================================

let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []


[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter")
    while true do
        let input:string = getInputString()
        let oList = lexer input
        let sList = printTList oList;
        Console.WriteLine("")
        //let pList = printTList (parser oList)
        let Out = parseNeval oList
        Console.WriteLine("Result = {0}", snd Out)
    0


















































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
