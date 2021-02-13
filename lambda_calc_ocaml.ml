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
  | ClosureT      of char * term * environment
  | ApplicationT  of term * term
and environment = (char * term) list

let rec parseSingle (tokens: token list): (term * token list) =
  match tokens with
  | (Variable name::rest) -> VariableT name, rest
  | (Lambda::Variable arg::Dot::bodyCode) ->
      let body, rest = parseSingle bodyCode in
      LambdaT (arg, body), rest
  | LParen::code ->
      let fn, afterFirst = parseSingle code in
      let value, afterValue = parseSingle afterFirst in
      begin
      match afterValue with
      | RParen::rest ->
          ApplicationT (fn, value), rest
      | _ -> failwith "expected a right paren lol"
      end
  | _ -> failwith "bad parse"


let parse (tokens: token list) =
  fst (parseSingle tokens)


let rec evalInEnv env (term: term) : term =
  match term with
  | VariableT name ->
      begin
      match List.find_opt (fun (c, t) -> c = name) env with
        | Some (_, term) -> term
        | None -> failwith "couldnt find a term by name"
      end
  | LambdaT (arg, body) -> ClosureT (arg, body, env)
  | ApplicationT (fn, value) ->
      begin
      match evalInEnv env fn with
        | ClosureT (arg, body, closedEnv) ->
            let evaluatedValue = evalInEnv env value in
            let newEnv = (arg, evaluatedValue)::closedEnv @ env in
            evalInEnv newEnv body
        | _ ->
            failwith "cannot apply what was given"
      end
  | closure -> closure

let eval (term: term): term =
  evalInEnv [] term

let rec pretty (term: term): char list =
  match term with
  | VariableT name -> [name]
  | LambdaT (arg, body) -> ['\\'; arg; '.'] @ pretty body
  | ClosureT (arg, body, _) -> ['\\'; arg; '.'] @ pretty body
  | ApplicationT (fn, value) -> '(' :: pretty fn @ [' '] @ pretty value @ [')']

let interpret (characters: char list) : char list =
  characters
    |> tokenise
    |> parse
    |> eval
    |> pretty

let interpretString s =
  s |> String.to_seq |> List.of_seq |> interpret |> List.to_seq |> String.of_seq

