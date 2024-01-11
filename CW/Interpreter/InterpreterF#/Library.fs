namespace InterpreterFSharp

module LexerParser =
    open System

    type Number =
        Int of int | Float of float

    type terminal = 
        Add | Sub | Mul | Div | Rem | Pow | Lpar | Rpar | Equ | Plt | Vid of string | Num of Number | Neg | Plus | Integrate| Comma | Err of char

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

    let getInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()

    let rec searchVName vName (symList:List<string*Number>) =
        match symList with
        | head :: tail -> if (fst head) = vName then (true, (snd head))
                          else searchVName vName tail
        | _ -> (false, Number.Int 0)

    //===================== Insert token Mul btw a Num and a Vid ================
    let insertMulBetweenNumAndVid tokens =
        let rec insertMul acc = function
            | Num n :: Vid v :: rest ->
                insertMul (acc @ [Num n; Mul; Vid v]) rest
            | token :: rest ->
                insertMul (acc @ [token]) rest
            | [] -> acc

        insertMul [] tokens
    //===================== Insert token Mul btw a Num and a Vid ================

    //============================= Turn a Number to type float =================
    let getNumeric number = 
        match number with
        | Float value -> float value
        | Int value -> float value
    //============================= Turn a Number to type float =================

    //============================= Simplify Token List =========================
    let simplifyTokens tList = 
        let rec simplify token = 
            match token with
            | Num(Number.Int n) :: Mul :: Num(Number.Int m) :: tail -> Num(Number.Int(n*m)):: simplify(tail)
            | Num(Number.Int n) :: Add :: Num(Number.Int m) :: tail -> Num(Number.Int(n+m)):: simplify(tail)
            | Num(Number.Int n) :: Sub :: Num(Number.Int m) :: tail -> Num(Number.Int(n-m)):: simplify(tail)
            | Add::Num(Number.Int 0) :: tail -> simplify(tail)
            | Sub::Num(Number.Int 0) :: tail -> simplify(tail)
            | head::tail-> head::simplify(tail)
            | [] -> []

        simplify tList
    //============================= Simplify Token List =========================

    //============================= Token List to String ========================
    let tokenToString tList = 
        let rec tToString token = 
            match token with
            | Num (Int n) -> string n
            | Vid v -> string v
            | Add -> "+"
            | Sub -> "-"
            | Mul -> "*"
            | Div -> "/"
            | Pow -> "^"
            | Lpar -> "("
            | Rpar -> ")"

        String.concat "" (List.map tToString tList)
    //============================= Token List to String =========================

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
            | ',' :: _ -> true
            | _ -> false

        let rec scan prevChar negCount input =
            match input with
            | [] -> []
            | '+'::tail ->
                if isUnary prevChar then
                    Plus :: scan ['+'] 0 tail
                else
                    Add :: scan ['+'] 0 tail
            | '-'::tail ->
                if isUnary prevChar then
                    scanNegs (negCount + 1) tail
                else
                    Sub :: scan ['-'] 0 tail
            | '*'::tail -> Mul :: scan ['*'] 0 tail
            | '/'::tail -> Div :: scan ['/'] 0 tail
            | '%'::tail -> Rem :: scan ['%'] 0 tail
            | '^'::tail -> Pow :: scan ['^'] 0 tail
            | '('::tail -> Lpar:: scan ['('] 0 tail
            | ')'::tail -> Rpar:: scan [')'] 0 tail
            | '='::tail -> Equ :: scan ['='] 0 tail
            | ','::tail -> Comma:: scan[','] 0 tail
            | c :: tail when isblank c -> scan [c] 0 tail
            | c :: tail when isdigit c -> 
                let (iStr, iVal) = scInt(tail, intVal c) 
                Num iVal :: scan [c] 0 iStr
            | c :: tail when ischar c -> 
                let (iStr, vName) = scChar(tail, c.ToString())
                match vName with
                | "plot" -> Plt :: scan [] 0 iStr
                | "integrate" -> Integrate:: scan [] 0 iStr
                | _ -> Vid vName :: scan [c] 0 iStr
            | c :: tail -> Err c :: scan [c] 0 tail

        and scanNegs negCount input =
            match input with
            | '-'::tail -> scanNegs (negCount + 1) tail
            | _ -> 
                if negCount % 2 = 0 then
                    Plus :: scan [] 0 input
                else
                    Neg :: scan [] 0 input

        scan [] 0 (str2lst input)
    //============================ Lexer =========================================  
    
    // Grammar in (E)BNF:
    //<Plot> ::= <Plt> "(" <VA> ")"
    //<VA> ::= <varID> "=" <E>
    //<E> ::= <T> <Eopt>
    //<Eopt> ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    //<T> ::= <P> <Topt>
    //<Topt> ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
    //<P> ::= <NR> <Popt>
    //<Popt> ::= "^" <NR> <Popt> | <empty>
    //<NR> ::= <Num> | "varVal" value | "(" <E> ")" | "-" <NR> | "+" <NR>
    //<Num> ::= ["int" | "float"] value
    //<varID> ::= [a-z,A-Z]+
    //<Plt> ::= "plot"

    ////============================= Parser ======================================
    let parser tList symList = 
        let rec E ((tList: result<terminal list>), plot:bool) = 
            match fst ((T >> Eopt) (tList, plot)) with
            | Success res -> (Success res, plot)
            | Failure error -> (Failure (error), plot)
        and Eopt ((tList: result<terminal list>), plot:bool) =             
            match tList with
            | Success (Add :: tail) -> (T >> Eopt) ((Success tail), plot)
            | Success (Sub :: tail) -> (T >> Eopt) ((Success tail), plot)
            | Failure error -> (Failure (error), plot)
            | _ -> (tList, plot)    
        and T ((tList: result<terminal list>), plot:bool) = 
            match fst ((P >> Topt) (tList, plot)) with
            | Success res -> (Success res, plot)
            | Failure error -> (Failure (error), plot)
        and Topt ((tList: result<terminal list>), plot:bool) =
            match tList with
            | Success (Div :: (Num (Number.Int 0)) :: tail) -> ((Failure "Cannot divide by zero."), plot)
            | Success (Div :: (Num (Number.Float 0.0)) :: tail) -> ((Failure "Cannot divide by zero."), plot)
            | Success (Mul :: tail) -> (P >> Topt) ((Success tail), plot)
            | Success (Div :: tail) -> (P >> Topt) ((Success tail), plot)
            | Success (Rem :: tail) -> (P >> Topt) ((Success tail), plot)
            | Failure error -> (Failure error, plot)
            | _ -> (tList, plot)
        and P ((tList: result<terminal list>), plot:bool) =   
            match fst ((NR >> Popt) (tList, plot)) with
            | Success res -> (Success res, plot)
            | Failure error -> (Failure (error), plot)
        and Popt ((tList: result<terminal list>), plot:bool) =
            match tList with
            | Success (Pow :: tail) -> (NR >> Popt) ((Success tail), plot)
            | Failure error -> ((Failure error), plot)
            | _ -> // Handle consecutive NR elements by checking the next token
                 match tList with
                 | Success (Vid _ :: _) -> (NR >> Popt) (tList, plot)
                 | _ -> (tList, plot)
                

        and NR ((tList: result<terminal list>), plot:bool) =
            match tList with 
            | Success (Num v1:: Comma :: Num v2 :: Comma:: tail) -> (Success tail, plot)
            | Success (Neg :: Num value :: tail) -> (Success tail, plot)
            | Success (Neg :: Lpar :: tail) -> match fst (E ((Success tail), plot)) with
                                               | Success (Rpar :: tail) -> (Success tail, plot)
                                               | Success _ -> (Failure "Missing right parenthesis", plot)
                                               | Failure error -> (Failure error, plot)
            | Success (Plus :: Num value :: tail) -> (Success tail, plot)

            | Success (Num value :: tail) -> (Success tail, plot)

            | Success (Vid vName :: tail) -> if plot then (Success tail, plot)
                                             else
                                                 let res = searchVName vName symList
                                                 if (fst res) then ((Success tail), plot)
                                                 else (Failure ("The variable \"" + vName + "\" is not assigned."), plot)
            | Success (Lpar :: tail) -> match fst (E ((Success tail), plot)) with 
                                        | Success (Rpar :: tail) -> (Success tail, plot)
                                        | Success _ -> (Failure "Missing right parenthesis", plot)
                                        | Failure error -> (Failure error, plot)
            | Failure error -> (Failure error, plot)
            | c -> (Failure ("Unexpected token \"" + c.ToString() + "\""), plot)
        let VA ((tList: result<terminal list>), plot:bool) =  
            match tList with
            | Success (Vid vName :: tail) -> match tail with
                                             | Equ :: tail -> E ((Success tail), plot)
                                             | Num value :: tail -> (Failure "Missing equal sign after variable name", plot)
                                             | _ -> E (tList, plot)
            | _ -> E (tList, plot)
        let Plot tList = 
            match tList with 
            | Plt :: tail -> E ((Success tail), true)
            | Integrate :: tail ->  E ((Success tail), true)
            | _ -> VA ((Success tList), false)

        Plot tList 

    let parseNeval tList (symList:List<string*Number>) : (bool * (terminal list * (string * Number))) =
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
                    | Number.Float a, Number.Int b -> Number.Float (float a ** float b)
                    | Number.Int a, Number.Float b -> Number.Float (float a ** float b)
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
        let Plot tList = // Adds a boolean for if this is a plot function and returns the token list
            match tList with
                | Plt :: tail -> (true, (tail, ("", Number.Int 0)))
                | Integrate :: Lpar :: Num v1 tail -> (true, (tail, ("", Number.Int 0)))
                | _ -> (false, VA tList)
        Plot tList

    let parseNevalNsym tList (symList:List<string*Number>) =
        let pNe = parseNeval tList symList
        let vID = fst (snd (snd pNe))
        let tval = snd (snd (snd pNe))
        let symCopy = symList
        match vID with
          | "" -> (pNe, symList) // if there is no vID just return the symList
          | _ -> match symList with
                 | [] -> (pNe, symList @ [vID, tval])
                 | _ -> let res = check4vid symList vID tval // if the vID is already in symbol table replace its value
                        if res.Equals(symList) then (pNe, symList @ [vID, tval])
                        else (pNe, res)
                 

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


    // Evaluates the equation given in the form of a token list using the value of x given
    let rec evalPoly (tlist:list<terminal>) (x:double) = 
        let rec E tList = (T >> Eopt) tList
        and Eopt (tList, (vID, value)) =
            match tList with
            | Add :: tail -> 
                let (tLst, (vID, tval)) = T tail
                Eopt (tLst, (vID,
                    match value, tval with
                    | Number.Int a, Number.Int b -> Number.Float (float a + float b)
                    | Number.Float a, Number.Float b -> Number.Float (a + b)
                    | Number.Int a, Number.Float b -> Number.Float (float a + b)
                    | Number.Float a, Number.Int b -> Number.Float (a + float b)
                ))
            | Sub :: tail -> 
                let (tLst, (vID, tval)) = T tail
                Eopt (tLst, (vID,
                    match value, tval with
                    | Number.Int a, Number.Int b -> Number.Float (float a - float b)
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
                    | Number.Int a, Number.Int b -> Number.Float (float a * float b)
                    | Number.Float a, Number.Float b -> Number.Float (a * b)
                    | Number.Int a, Number.Float b -> Number.Float (float a * b)
                    | Number.Float a, Number.Int b -> Number.Float (a * float b)
                ))
            | Div :: tail -> 
                let (tLst, (vID, tval)) = P tail
                Topt (tLst, (vID, 
                    match value, tval with
                    | Number.Int a, Number.Int b -> Number.Float (float a / float b)
                    | Number.Float a, Number.Float b -> Number.Float (a / b)
                    | Number.Int a, Number.Float b -> Number.Float (float a / b)
                    | Number.Float a, Number.Int b -> Number.Float (a / float b)
                ))
            | Rem :: tail -> 
                let (tLst, (vID, tval)) = P tail
                Topt (tLst, (vID, 
                    match value, tval with
                    | Number.Int a, Number.Int b -> Number.Float (float a % float b)
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
                            Number.Float (pown a (int b))
                    | Number.Float a, Number.Float b ->
                        if isNeg tval then
                            Number.Float (float a ** float b)
                        else
                            Number.Float (pown a (int b))
                    | Number.Float a, Number.Int b -> Number.Float (float a ** float b)
                    | Number.Int a, Number.Float b -> Number.Float (float a ** float b)
                ))
            | _ -> 
                match tList with
                | Vid _ :: _ -> (NR >> Popt) tList
                | _ -> (tList, ("", value))
        and NR tList =
            match tList with 
            | Neg :: Num value :: tail -> 
                (tail, ("",
                    match value with
                    | Int a -> Float (-a)
                    | Float a -> Float (-a)))
            |Neg :: Lpar :: tail ->
                let (tLst, (vID, tval)) = E tail
                match tLst with 
                | Rpar :: tail ->
                    (tail, ("",
                        match tval with
                        | Int a -> Float (-a)
                        | Float a -> Float (-a)))
                | _ -> raise parseError
            | Plus :: Num value :: tail -> (tail, ("", value))
            | Vid vname :: tail -> (tail, ("", Number.Float x))
            | Num value :: tail -> (tail, ("", value))
            | Lpar :: tail ->  let (tLst, (vID, tval)) = E tail
                               match tLst with 
                               | Rpar :: tail -> (tail, ("", tval))
                               | _ -> raise parseError
            | _ -> raise parseError
        snd (snd (E tlist))

    let initPlotTokens =
        [Num (Number.Float 0)]

    //================================== DERIVATIVE OF FUNCITONS /==================================
    let rec findDerivative(tokens:list<terminal>): list<terminal> =
        let rec derivativeOfTerm term =
            match term with
            | Num (Int n) :: Mul :: Vid v :: Pow :: Num (Int p) :: tail -> [Num(Number.Int n); Mul; Num(Number.Int p); Mul; Vid v; Pow; Lpar; Num (Number.Int p); Sub; Num(Number.Int 1); Rpar] @ derivativeOfTerm tail // dy/dx 2x^3 = 6x^2
            | Num (Int n) :: Mul :: Vid v :: tail -> Num (Number.Int n) :: derivativeOfTerm tail// dy/dx of 2x = 2
            | Vid v :: Pow :: Num(Int p):: tail -> [Num (Number.Int p) ; Mul; Vid v; Pow; Lpar; Num (Number.Int p); Sub; Num(Number.Int 1); Rpar] @ derivativeOfTerm tail //x^2 = 2x
            | Num n :: tail -> Num (Number.Int 0) :: derivativeOfTerm tail  // Constant numbers e.g dy/dx of 5 = 0
            | Vid c :: tail -> Num (Number.Int 1) :: derivativeOfTerm tail // stand alone Variables e.g dy/dx of x = 1
            | Add :: tail -> Add:: derivativeOfTerm tail
            | Sub :: tail -> Sub :: derivativeOfTerm tail
            | Neg:: tail -> Neg :: derivativeOfTerm tail
            | Plus::tail -> Plus :: derivativeOfTerm tail
            | Mul::tail -> Mul :: derivativeOfTerm tail
            |_ -> []


        match tokens with
        | [] -> []
        | Lpar::tail -> 
            // If the expression is enclosed in "()" then find the derivative of the enclosed expression 
            let innerExpression, rest = findInnerExpression tail 1
            derivativeOfTerm innerExpression @ findDerivative rest
        | head :: tail -> 
            // Find the derivative of the first term and continue with the rest of the expression
            derivativeOfTerm [head] @ findDerivative tail

    and findInnerExpression (tokens: list<terminal>) (parenCount: int) =
        match tokens with
        | Rpar :: tail when parenCount = 1 -> [], tail // End of all inner expressions 
        | Lpar :: tail -> 
            // There is more nested expressions; increment how deep we are by +1
            let inner, rest = findInnerExpression tail (parenCount + 1)
            Lpar :: inner, rest // Include Lpar in the inner expression
        | Rpar :: tail -> 
            // We've completed one inner exression but thers is more; decrement by -1
            let inner, rest = findInnerExpression tail (parenCount - 1)
            Rpar :: inner, rest // Include Rpar in the inner expression
        | head :: tail -> 
            let inner, rest = findInnerExpression tail parenCount
            head :: inner, rest
        | [] -> [], []
    //================================== DERIVATIVE OF FUNCITONS /==================================


    //================================== FINDING ROOTS OF FUNCTIONS /===============================
    let bisectionMethod (tokens:list<terminal>) (lower:double) (upper:double) = 
        let rec bisection start stop count =
            if count = 0 then
                (true, (start+stop)/2.0)
            else
                let midpoint = (start + stop) / 2.0

                let x0 = getNumeric (evalPoly tokens start)
                let mid = getNumeric (evalPoly tokens midpoint)
                let x1 = getNumeric (evalPoly tokens stop)

                if x0 * mid <= 0.0 then
                    bisection start midpoint (count - 1)
                elif mid * x1 <= 0.0 then
                    bisection midpoint stop (count - 1) 
                else 
                    (false, 0.0)


        bisection lower upper 1000

    let newtonMethod (tList:list<terminal>) (dList:list<terminal>) (startValue:list<double>) (maxIteration:double) (accuracy:double)=
        let rec iterate currentGuess iterations =
            let tGuess = getNumeric(evalPoly tList currentGuess)
            let dGuess = getNumeric(evalPoly dList currentGuess)

            let nextGuess = currentGuess - (tGuess / dGuess)

            let difference = abs(nextGuess - currentGuess)

            if difference <= accuracy || iterations >= maxIteration then
                nextGuess
            else
                iterate nextGuess (iterations + 1.0)

        let leftRoot = iterate startValue[1] 0.0
        let rightRoot = iterate startValue[0] 0.0

        let result = []
        if abs(leftRoot-rightRoot) <= accuracy then
           leftRoot::result 
        else
            leftRoot::rightRoot::result

    //================================== FINDING ROOTS OF FUNCTIONS /=============================

    //================================== INTEGRAL OF FUNCITONS /==================================
    let rec findIntegral(tokens: list<terminal>): list<terminal> =
        let rec integralOfTerm term =
            match term with
            | Num (Int n) :: Mul :: Vid v :: Pow :: Num (Int p) :: tail -> // e.g 2x^2 = (2/2+1)*x^2+1
                [Lpar; Num(Number.Int n); Div; Lpar; Num(Number.Int p); Add; Num(Number.Int 1); Rpar; Rpar; Mul; Vid v; Pow; Num(Number.Int p); Add; Num(Number.Int 1)] @ integralOfTerm tail
            | Num (Int n) :: Mul :: Vid v :: tail -> // e.g 2x = (2/2)*x^2
                [Lpar; Num(Number.Int n); Div; Num(Number.Int 2); Rpar; Mul; Vid v; Pow; Num(Number.Int 2)] @ integralOfTerm tail
            | Vid v :: Pow :: Num(Int p):: tail -> // e.g x^2 = (1/2+1)*x^2+1
                [Lpar; Num(Number.Int 1); Div; Lpar; Num(Number.Int p); Add; Num(Number.Int 1); Rpar; Rpar; Mul; Vid v; Pow; Num(Number.Int p); Add; Num(Number.Int 1)] @ integralOfTerm tail
            | Num (Int n) :: tail -> // e.g 3 = (3/2)*x
                [Lpar; Num(Number.Int n); Div; Num(Number.Int 2); Rpar; Mul; Vid "x"] @ integralOfTerm tail // Integration of constant term
            | Vid c :: tail -> // e.g x = (1/2)*x^2
                [Lpar; Num(Number.Int 1); Div; Num(Number.Int 2); Rpar; Mul; Vid c; Pow; Num(Number.Int 2)] @ integralOfTerm tail // Integration of a variable term
            | Add :: tail -> Add :: integralOfTerm tail
            | Sub :: tail -> Sub :: integralOfTerm tail
            | Neg:: tail -> Neg :: integralOfTerm tail
            | Plus::tail -> Plus :: integralOfTerm tail
            | Mul::tail -> Mul :: integralOfTerm tail
            |_ -> []

        match tokens with
        | [] -> []
        | Lpar::tail -> 
            let innerExpression, rest = findInnerExpression tail 1
            integralOfTerm innerExpression @ findIntegral rest
        | head :: tail -> 
            integralOfTerm [head] @ findIntegral tail
    //================================== iNTEGRAL OF FUNCITONS /==================================

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
            let tempID = fst (snd (snd Out))
            let tempVal = snd (snd (snd Out))
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
