module App exposing (main)

import Html exposing (Html, div, nav, button, text, p, input, label)
import Html.Attributes exposing (class, type_, min, max, value, placeholder, for, id)
import Html.Events exposing (onClick, onInput)
import Browser

type alias State = {
        inputFunc: String,
        result: String,
        inputValue: Float
    }

initialState : State
initialState =
    {
        inputFunc = "",
        result = "",
        inputValue = 0
    }

type alias Term = { --//e.g. 3 * cos(x)
    factor: Float, 
    f: InnerFunction
    }

type  InnerFunction  = X_POW Float | COS  | SIN | EXP

type alias Function = List Term

type Action = READ_FUNC String | READ_X String | DIFFERENTIATE | SHOW | NONE

calc: Term -> Float -> Float
calc term x  = case term.f of 
    X_POW n -> (x ^ n)
    COS -> Basics.cos x
    SIN -> Basics.sin x
    EXP -> (Basics.e ^ x)

calcDiff: Term -> Float -> Float
calcDiff term x  = case term.f of 
    X_POW n -> (n * term.factor )* x ^ (n-1)
    COS -> term.factor * Basics.sin x * -1
    SIN -> term.factor * Basics.cos x
    EXP -> (term.factor * Basics.e ^ x)

parseTerm: Term -> Float -> Float
parseTerm term x = 
       (term.factor * (calc term x))

processTermList: Function -> Float -> Float -> Float -> (Float, Float)
processTermList function x result diffResult = case function of 
    [] -> (result, diffResult)
    _ ->
        let
            firstArg = List.head function
            newResult =  parseTerm (Maybe.withDefault { factor = 0, f = (X_POW 1)} firstArg) x
            newDiffResult = calcDiff (Maybe.withDefault { factor = 0, f = (X_POW 1)} firstArg) x
        in
            processTermList (List.drop 1 function) x (result + newResult) (diffResult + newDiffResult)
    
--todo: create List of Terms from List of Strings -> put into processTermList 
--  [3x^3, 2x] -> [{factor = 3, f = X_POW 3}, {factor = 2, f = X_POW 1}]
createFunction list = List.map createTerm list

-- find x -> if chars behind == ^... -> chars before = factor, f = pow

getExpAndFac: List String -> (String, String)
getExpAndFac list =
    case list of
        [a] -> ("1" ,a)
        [a, b] -> (a,b)
        _ -> ("0", "0")

-- cases:  ncosx  cosx  nsinx sinx nx x ne^x e^x 
-- special case: nx - cosx
createTerm str = if String.contains "cosx" str then
        let
            fac = Maybe.withDefault 1 (String.toFloat(String.dropRight 4 str))
        in
           {factor = fac, f = COS}
    else if String.contains "sinx" str then
        let
            fac = Maybe.withDefault 1 (String.toFloat(String.dropRight 4 str))
        in
            {factor = fac, f = SIN}
    else if String.contains "e^x" str then
        let
            fac = Maybe.withDefault 1 (String.toFloat(String.dropRight 3 str))
        in
            {factor = fac, f = EXP}
    else if String.contains "-x^" str then
        let
        -- 23x^3 -> [23,3]; x^2 -> [2]; 
            factorAndExp = String.split "x^" str
        -- (23, 3); (1,2)
            (fac, exp) = getExpAndFac factorAndExp
        in
            {factor = -1 , f = X_POW (Maybe.withDefault 1 (String.toFloat exp))}
    else if String.contains "x^" str then
        let
        -- 23x^3 -> [23,3]; x^2 -> [2]; 
            factorAndExp = String.split "x^" str
        -- (23, 3); (1,2)
            (fac, exp) = getExpAndFac factorAndExp
        in
            {factor = Maybe.withDefault 1 (String.toFloat fac) , f = X_POW (Maybe.withDefault 1 (String.toFloat exp))}
    --  nx
    else if String.contains "-x" str then
        {factor = -1, f = X_POW 1}
    else if String.contains "x" str then
        let
            fac = Maybe.withDefault 1 (String.toFloat(String.dropRight 1 str))
        in
            {factor = fac, f = X_POW 1}
    -- n
    else {factor =  Maybe.withDefault 0 (String.toFloat(str)), f = X_POW 0}
    

parseResult: String -> Float -> String
parseResult func x = 
    let
        (result, diffResult) = (processTermList (createFunction (createList func)) x 0 0)
    in 
        String.fromFloat result

parseDiffResult: String -> Float -> String
parseDiffResult func x =
    let
        (result, diffResult) = (processTermList (createFunction (createList func)) x 0 0)
    in
        String.fromFloat diffResult
    
        


-- 3x + 2x^2 - x -> 3x + 2z^2 +-x -> [3x, 2z^2, -x]
createList: String -> List String
createList str = String.split "+" (replace "-" "+-" (removeWhitespaces str))

removeWhitespaces: String -> String
removeWhitespaces func = (replace " " "" func)

search : String -> String -> List Int
search char str = 
    String.indexes char str 

replace : String -> String -> String -> String
replace from to str =
    String.split from str
        |> String.join to

reducer : Action -> State -> State
reducer action state = case action of
    READ_FUNC s -> { state | inputFunc = s }
    READ_X n -> { state | inputValue = (String.toFloat n) |> Maybe.withDefault 0 }
    SHOW -> {state | result = (parseResult state.inputFunc state.inputValue) }
    DIFFERENTIATE -> {state | result = (parseDiffResult state.inputFunc state.inputValue)}
    _ -> state


render : State -> Html Action
render state =
    div[class "row"][
        nav [class "blue z-depth-0"][
            div [class "brand-logo center"][text "Parser"]
        ],
        div[class "container row center-align"][
            p [][
                div [class "col s5"][
                    label [for "inputFunc"] [text "Function" ],
                    input [placeholder "function", id "inputFunc", onInput READ_FUNC, value (state.inputFunc)][],
                    label [for "inputFunc"] [text "X" ],
                    input [placeholder "x", id "x", onInput READ_X, value (String.fromFloat state.inputValue)][]
                ]
            ],
            p [][
                div [class "col s5"][
                    div[class "row s5"] [
                        button [class "blue btn-large", onClick SHOW][text "evaluate"],
                        text "   ",
                        button [class "blue btn-large", onClick DIFFERENTIATE][text "differentiate"]
                    ],
                    div[class "row s2"][text "  "],
                    div[class "row s3"] [
                        div[class "chip"][
                            text state.result
                        ]
                    ]
                ]
            ]
        ]
    ]

main = Browser.sandbox {
        init = initialState,
        update = reducer,
        view = render
        }
