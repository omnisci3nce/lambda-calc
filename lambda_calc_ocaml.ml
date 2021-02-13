(*
Examples:
  \x.x                          --> x.x
  x                             --> error
  (\x.x \y.y)                   --> \y.y
  ((\x.\y.x   \a.(a a)) \b.b)   --> \a.(a a)    Do as an exercise
*)

type token =
  | LParen
  | RParen
  | Lambda
  | Dot
  | Variable of char

let alphabet = String.to_seq "abcdefghijklmnopqrstuvwxyz" |> List.of_seq

let rec tokenise (text: char list) =
  match text with
  | [] -> []
  | '('::rest -> LParen::tokenise rest
  | ')'::rest -> RParen::tokenise rest
  | '.'::rest -> Dot::tokenise rest
  | '\\'::rest -> Lambda::tokenise rest
  | c::rest ->
      (if List.mem c alphabet
      then [Variable c]
      else []) @ tokenise rest


type term =
  | VariableT     of char
  | LambdaT       of char * term
  | ApplicationT  of term * term

let rec parseSingle (tokens: token list): (term * token list) =
  match tokens with
  | (Variable name::rest) -> VariableT name, rest
  | (Lambda::Variable arg::Dot::bodyCode) ->
      let body, rest = parseSingle bodyCode in
      LambdaT (arg, body), rest
  | LParen::code ->
      let fn, afterFirst = parseSingle code in
      let value, afterValue = parseSingle afterFirst in

      match afterValue with
      | RParen::rest ->
          ApplicationT (fn, value), rest
      | _ -> failwith "expected a right paren lol"
  | _ -> failwith "bad parse"


let parse (tokens: token list) =
  fst (parseSingle tokens)

type Environment = (char * term) list

let evalInEnv env term =
  match term with
  | VariableT name ->
      match List.find_opt (fun a -> a = name) env with
        | Some term -> term
        | None -> failwith "couldnt find a term by name"
  | LambdaT (arg, body) ->
      ClosureT (arg, body, env)
  | ApplicationT (fn, value) ->

let eval (term: term): term =
  evalInEnv [] term

