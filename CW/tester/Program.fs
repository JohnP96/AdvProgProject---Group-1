// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

type terminal = 
    Add | Sub | Mul | Div | Rem | Pow | Lpar | Rpar | Num of int

let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let lexError = System.Exception("Lexer error")
let intVal (c:char) = (int)((int)c - (int)'0')
let parseError = System.Exception("Parser error")

let rec scInt(iStr, iVal) = 
    match iStr with
    c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
    | _ -> (iStr, iVal)

let lexer input = 
    let rec scan input =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail
        | '-'::tail -> Sub :: scan tail
        | '*'::tail -> Mul :: scan tail
        | '/'::tail -> Div :: scan tail
        | '%'::tail -> Rem :: scan tail
        | '^'::tail -> Pow :: scan tail
        | '('::tail -> Lpar:: scan tail
        | ')'::tail -> Rpar:: scan tail
        | c :: tail when isblank c -> scan tail
        | c :: tail when isdigit c -> let (iStr, iVal) = scInt(tail, intVal c) 
                                      Num iVal :: scan iStr
        | _ -> raise lexError
    scan (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()
    
// Grammar in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <P> <Topt>
// <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>        ::= <NR> <Popt>
// <Popt>     ::= "^" <NR> <Popt> | <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")"

//!!!!!IMPLEMENT UNARY MINUS!!!!!
let parser tList = 
    let rec E tList = (T >> Eopt) tList     // NOTE: >> is (forward) function composition
    and Eopt tList =                        // 'and' allows for mutual recursion
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList
    and T tList = (P >> Topt) tList
    and Topt tList =
        match tList with
        | Mul :: tail -> (P >> Topt) tail
        | Div :: tail -> (P >> Topt) tail
        | Rem :: tail -> (P >> Topt) tail
        | _ -> tList
    and P tList = (NR >> Popt) tList
    and Popt tList =
        match tList with
        | Pow :: tail -> (NR >> Popt) tail
        | _ -> tList
    and NR tList =
        match tList with 
        | Num value :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | _ -> raise parseError
    E tList

let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value + tval)
        | Sub :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value - tval)
        | _ -> (tList, value)
    and T tList = (P >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Mul :: tail -> let (tLst, tval) = P tail
                         Topt (tLst, value * tval)
        | Div :: tail -> let (tLst, tval) = P tail
                         Topt (tLst, value / tval)
        | Rem :: tail -> let (tLst, tval) = P tail
                         Topt (tLst, value % tval)
        | _ -> (tList, value)
    and P tList = (NR >> Popt) tList
    and Popt (tList, value)= 
        match tList with
        | Pow :: tail -> let (tLst, tval) = NR tail
                         Popt (tLst, pown value tval)
        | _ -> (tList, value)
    and NR tList =
        match tList with 
        | Num value :: tail -> (tail, value)
        | Lpar :: tail -> let (tLst, tval) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> raise parseError
        | _ -> raise parseError
    E tList

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
        let pList = printTList (parser oList)
        let Out = parseNeval oList
        Console.WriteLine("Result = {0}", snd Out)
    0