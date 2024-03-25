module String = struct
  let is_prefix ~prefix str =
    let pl = String.length prefix in
    if String.length str < pl then
      false
    else
      String.sub str 0 (String.length prefix) = prefix

  let cut sep str =
    try
      let idx = String.index str sep
      and l = String.length str
      in
      let sidx = succ idx in
      Some (String.sub str 0 idx, String.sub str sidx (l - sidx))
    with
      Not_found -> None

  let cuts sep str =
    let rec doit acc s =
      match cut sep s with
      | None -> List.rev (s :: acc)
      | Some (a, b) -> doit (a :: acc) b
    in
    doit [] str

  let slice ?(start = 0) ?stop str =
    let stop = match stop with
      | None -> String.length str
      | Some x -> x
    in
    let len = stop - start in
    String.sub str start len

  let trim = String.trim

  let get = String.get

  let concat = String.concat

  let length = String.length

  let equal = String.equal

  let index_from = String.index_from

  let sub = String.sub
end

type hunk = {
  mine_start : int ;
  mine_len : int ;
  mine : string list ;
  their_start : int ;
  their_len : int ;
  their : string list ;
}

let unified_diff ~mine_no_nl ~their_no_nl hunk =
  let no_nl_str = ["\\ No newline at end of file"] in
  (* TODO *)
  String.concat "\n" (List.map (fun line -> "-" ^ line) hunk.mine @
                      (if mine_no_nl then no_nl_str else []) @
                      List.map (fun line -> "+" ^ line) hunk.their @
                      (if their_no_nl then no_nl_str else []))

let pp_hunk ~mine_no_nl ~their_no_nl ppf hunk =
  Format.fprintf ppf "@@@@ -%d,%d +%d,%d @@@@\n%s"
    hunk.mine_start hunk.mine_len hunk.their_start hunk.their_len
    (unified_diff ~mine_no_nl ~their_no_nl hunk)

let take data num =
  let rec take0 num data acc =
    match num, data with
    | 0, _ -> List.rev acc
    | n, x::xs -> take0 (pred n) xs (x :: acc)
    | _ -> invalid_arg "take0 broken"
  in
  take0 num data []

let drop data num =
  let rec drop data num =
    match num, data with
    | 0, _ -> data
    | n, _::xs -> drop xs (pred n)
    | _ -> invalid_arg "drop broken"
  in
  try drop data num with
  | Invalid_argument _ -> invalid_arg ("drop " ^ string_of_int num ^ " on " ^ string_of_int (List.length data))

(* TODO verify that it applies cleanly *)
let apply_hunk old (index, to_build) hunk =
  try
    let prefix = take (drop old index) (hunk.mine_start - index) in
    (hunk.mine_start + hunk.mine_len, to_build @ prefix @ hunk.their)
  with
  | Invalid_argument _ -> invalid_arg ("apply_hunk " ^ string_of_int index ^ " old len " ^ string_of_int (List.length old) ^
                                       " hunk start " ^ string_of_int hunk.mine_start ^ " hunk len " ^ string_of_int hunk.mine_len)

let to_start_len data =
  (* input being "?19,23" *)
  match String.cut ',' (String.slice ~start:1 data) with
  | None when data = "+1" || data = "-1" -> (0, 1)
  | None -> invalid_arg ("start_len broken in " ^ data)
  | Some (start, len) ->
     let len = int_of_string len
     and start = int_of_string start
     in
     let st = if len = 0 || start = 0 then start else pred start in
     (st, len)

let count_to_sl_sl data =
  if String.is_prefix ~prefix:"@@" data then
    (* input: "@@ -19,23 +19,12 @@ bla" *)
    (* output: ((19,23), (19, 12)) *)
    match List.filter (function "" -> false | _ -> true) (String.cuts '@' data) with
    | numbers::_ ->
       let nums = String.trim numbers in
       (match String.cut ' ' nums with
        | None -> invalid_arg "couldn't find space in count"
        | Some (mine, theirs) -> Some (to_start_len mine, to_start_len theirs))
    | _ -> invalid_arg "broken line!"
  else
    None

let sort_into_bags ~counter:(mine_len, their_len) dir mine their m_nl t_nl str =
  if String.length str = 0 then
    failwith "invalid patch (empty line)"
  else if mine_len = 0 && their_len = 0 && String.get str 0 <> '\\' then
    None
  else match String.get str 0, String.slice ~start:1 str with
    | ' ', data ->
        if m_nl || t_nl then
          failwith "\"no newline at the end of file\" is not at the end of the file";
        if mine_len = 0 || their_len = 0 then
          failwith "invalid patch (both size exhausted)";
        let counter = (mine_len - 1, their_len - 1) in
        Some (counter, `Both, (data :: mine), (data :: their), m_nl, t_nl)
    | '+', data ->
        if t_nl then
          failwith "\"no newline at the end of file\" is not at the end of the file";
        if their_len = 0 then
          failwith "invalid patch (+ size exhausted)";
        let counter = (mine_len, their_len - 1) in
        Some (counter, `Their, mine, (data :: their), m_nl, t_nl)
    | '-', data ->
        if m_nl then
          failwith "\"no newline at the end of file\" is not at the end of the file";
        if mine_len = 0 then
          failwith "invalid patch (- size exhausted)";
        let counter = (mine_len - 1, their_len) in
        Some (counter, `Mine, (data :: mine), their, m_nl, t_nl)
    | '\\', _data ->
      (* NOTE: Any line starting with '\' is taken as if it was
         '\ No newline at end of file' by GNU patch so we do the same *)
      (* diff: 'No newline at end of file' turns out to be context-sensitive *)
      (* so: -xxx\n\\No newline... means mine didn't have a newline *)
      (* but +xxx\n\\No newline... means theirs doesn't have a newline *)
      let my_nl, their_nl = match dir with
        | `Both -> true, true
        | `Mine -> true, t_nl
        | `Their -> m_nl, true
      in
      let counter = (mine_len, their_len) in
      Some (counter, dir, mine, their, my_nl, their_nl)
    | _ -> failwith "invalid patch (unknown character)"

let to_hunk count data mine_no_nl their_no_nl =
  match count_to_sl_sl count with
  | None -> None, mine_no_nl, their_no_nl, count :: data
  | Some ((mine_start, mine_len), (their_start, their_len)) ->
    let counter = (mine_len, their_len) in
    let rec step ~counter dir mine their mine_no_nl their_no_nl = function
      | [] | [""] -> (List.rev mine, List.rev their, mine_no_nl, their_no_nl, [])
      | x::xs -> match sort_into_bags ~counter dir mine their mine_no_nl their_no_nl x with
        | Some (counter, dir, mine, their, mine_no_nl', their_no_nl') -> step ~counter dir mine their mine_no_nl' their_no_nl' xs
        | None -> (List.rev mine, List.rev their, mine_no_nl, their_no_nl, x :: xs)
    in
    let mine, their, mine_no_nl, their_no_nl, rest = step ~counter `Both [] [] mine_no_nl their_no_nl data in
    (Some { mine_start ; mine_len ; mine ; their_start ; their_len ; their }, mine_no_nl, their_no_nl, rest)

let rec to_hunks (mine_no_nl, their_no_nl, acc) = function
  | [] -> (List.rev acc, mine_no_nl, their_no_nl, [])
  | count::data -> match to_hunk count data mine_no_nl their_no_nl with
    | None, mine_no_nl, their_no_nl, rest -> List.rev acc, mine_no_nl, their_no_nl, rest
    | Some hunk, mine_no_nl, their_no_nl, rest -> to_hunks (mine_no_nl, their_no_nl, hunk :: acc) rest

type operation =
  | Edit of string
  | Rename of string * string
  | Delete of string
  | Create of string
  | Rename_only of string * string

let operation_eq a b = match a, b with
  | Edit a, Edit b
  | Delete a, Delete b
  | Create a, Create b -> String.equal a b
  | Rename (a, a'), Rename (b, b')
  | Rename_only (a, a'), Rename_only (b, b') -> String.equal a b && String.equal a' b'
  | _ -> false

let no_file = "/dev/null"

let pp_operation ~git ppf op =
  let real_name direction name =
    if git then
      match direction with `Mine -> "a/" ^ name | `Theirs -> "b/" ^ name
    else name
  in
  let hdr mine their =
    (* even if create/delete, /dev/null is not used in this header *)
    (* according to git documentation *)
    if git then
      Format.fprintf ppf "diff --git %s %s\n"
        (real_name `Mine mine) (real_name `Theirs their)
  in
  match op with
  | Edit name ->
    hdr name name ;
    Format.fprintf ppf "--- %s\n" (real_name `Mine name) ;
    Format.fprintf ppf "+++ %s\n" (real_name `Theirs name)
  | Rename (old_name, new_name) ->
    hdr old_name new_name ;
    Format.fprintf ppf "--- %s\n" (real_name `Mine old_name) ;
    Format.fprintf ppf "+++ %s\n" (real_name `Theirs new_name)
  | Delete name ->
    hdr name name ;
    Format.fprintf ppf "--- %s\n" (real_name `Mine name) ;
    Format.fprintf ppf "+++ %s\n" no_file
  | Create name ->
    hdr name name ;
    Format.fprintf ppf "--- %s\n" no_file ;
    Format.fprintf ppf "+++ %s\n" (real_name `Theirs name)
  | Rename_only (old_name, new_name) ->
    hdr old_name new_name ;
    Format.fprintf ppf "rename from %s\n" (String.slice ~start:2 old_name);
    Format.fprintf ppf "rename to %s\n" (String.slice ~start:2 new_name)

type t = {
  operation : operation ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}

let pp ~git ppf {operation; hunks; mine_no_nl; their_no_nl} =
  pp_operation ~git ppf operation;
  let rec aux = function
    | [] -> ()
    | [x] -> pp_hunk ~mine_no_nl ~their_no_nl ppf x
    | x::xs ->
        pp_hunk ~mine_no_nl:false ~their_no_nl:false ppf x;
        aux xs
  in
  aux hunks

let get_filename ~p full =
  let rec iter idx = function
    | 0 -> String.sub full idx (String.length full - idx)
    | p -> iter (String.index_from full idx '/') (p - 1)
  in
  try Some (iter 0 p) with Not_found -> None

let operation_of_strings ~p mine their =
  let open (struct
    type t =
      | Dev_null
      | Ignore
      | File of string
  end) in
  let get_filename_opt n =
    let s = match String.cut '\t' n with None -> n | Some (x, _) -> x in
    if s = no_file then Dev_null else
      match get_filename ~p s with
      | None -> Ignore
      | Some x -> File x
  in
  match get_filename_opt mine, get_filename_opt their with
  | Dev_null, File n -> Some (Create n)
  | File n, Dev_null -> Some (Delete n)
  | File a, File b -> Some (if String.equal a b then Edit a else Rename (a, b))
  | Dev_null, Dev_null | Ignore, _ | _, Ignore -> None

(* parses a list of lines to a diff.t list *)
let to_diff ~p data =
  (* first locate --- and +++ lines *)
  let rec find_start ?hdr = function
    | [] -> hdr, []
    | x::y::xs when String.is_prefix ~prefix:"rename from " x && String.is_prefix ~prefix:"rename to " y ->
      begin
        let mf = "a/" ^ String.slice ~start:12 x
        and tf = "b/" ^ String.slice ~start:10 y
        in
        match get_filename ~p mf, get_filename ~p tf with
        | None, _ | _, None -> find_start xs
        | Some m, Some t ->
          let hdr = Rename_only (m, t) in
          find_start ~hdr xs
      end
    | x::y::xs when String.is_prefix ~prefix:"--- " x ->
      let mine = String.slice ~start:4 x and their = String.slice ~start:4 y in
      operation_of_strings ~p mine their, xs
    | _::xs -> find_start ?hdr xs
  in
  match find_start data with
  | Some (Rename_only _ as operation), rest ->
    let hunks = [] and mine_no_nl = false and their_no_nl = false in
    Some ({ operation ; hunks ; mine_no_nl ; their_no_nl }, rest)
  | Some operation, rest ->
    let hunks, mine_no_nl, their_no_nl, rest = to_hunks (false, false, []) rest in
    Some ({ operation ; hunks ; mine_no_nl ; their_no_nl }, rest)
  | None, [] -> None
  | None, _ -> assert false

let to_lines = String.cuts '\n'

let to_diffs ~p data =
  let lines = to_lines data in
  let rec doit acc = function
    | [] -> List.rev acc
    | xs -> match to_diff ~p xs with
      | None -> List.rev acc
      | Some (diff, rest) -> doit (diff :: acc) rest
  in
  doit [] lines

let patch filedata diff =
  match diff.operation with
  | Rename_only _ -> filedata
  | Delete _ -> None
  | Create _ ->
    begin match diff.hunks with
      | [ the_hunk ] ->
        let d = the_hunk.their in
        let lines = if diff.their_no_nl then d else d @ [""] in
        Some (String.concat "\n" lines)
      | _ -> assert false
    end
  | _ ->
    let old = match filedata with None -> [] | Some x -> to_lines x in
    let idx, lines = List.fold_left (apply_hunk old) (0, []) diff.hunks in
    let lines = lines @ drop old idx in
    let lines =
      match diff.mine_no_nl, diff.their_no_nl with
      | false, true -> (match List.rev lines with ""::tl -> List.rev tl | _ -> lines)
      | true, false -> lines @ [ "" ]
      | false, false when filedata = None -> lines @ [ "" ]
      | false, false -> lines
      | true, true -> lines
    in
    Some (String.concat "\n" lines)

let diff_op ~operation a b =
  let rec aux ~mine_start ~mine_len ~mine ~their_start ~their_len ~their l1 l2 =
    let create_diff ~mine_no_nl ~their_no_nl =
      let hunks =
        if mine = [] && their = [] then
          []
        else
          [{mine_start; mine_len; mine; their_start; their_len; their}]
      in
      {operation; hunks; mine_no_nl; their_no_nl}
    in
    match l1, l2 with
    | [], [] | [""], [""] when mine = [] && their = [] -> []
    | [], [] -> [create_diff ~mine_no_nl:true ~their_no_nl:true]
    | [""], [] -> [create_diff ~mine_no_nl:false ~their_no_nl:true]
    | [], [""] -> [create_diff ~mine_no_nl:true ~their_no_nl:false]
    | [""], [""] -> [create_diff ~mine_no_nl:false ~their_no_nl:false]
    | a::l1, ([] | [""]) ->
        aux
          ~mine_start:(mine_start + 1) ~mine_len ~mine:(a :: mine)
          ~their_start:(their_start + 1) ~their_len ~their
          l1 l2
    | ([] | [""]), b::l2 ->
        aux
          ~mine_start:(mine_start + 1) ~mine_len ~mine
          ~their_start:(their_start + 1) ~their_len ~their:(b :: their)
          l1 l2
    | a::l1, b::l2 when mine = [] && their = [] && String.equal a b ->
        aux
          ~mine_start:(mine_start + 1) ~mine_len ~mine
          ~their_start:(their_start + 1) ~their_len ~their
          l1 l2
    | a::l1, b::l2 ->
        aux
          ~mine_start ~mine_len:(mine_len + 1) ~mine:(a :: mine)
          ~their_start ~their_len:(their_len + 1) ~their:(b :: their)
          l1 l2
  in
  aux
    ~mine_start:0 ~mine_len:0 ~mine:[]
    ~their_start:0 ~their_len:0 ~their:[]
    (to_lines a) (to_lines b)

let diff ~filename a b = match a, b with
  | None, None -> invalid_arg "no input given"
  | None, Some b -> diff_op ~operation:(Create filename) "" b
  | Some a, None -> diff_op ~operation:(Delete filename) a ""
  | Some a, Some b -> diff_op ~operation:(Edit filename) a b
