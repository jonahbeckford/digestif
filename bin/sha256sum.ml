let sum ic =
  let tmp = Bytes.create 0x1000 in
  let rec go ctx = match input ic tmp 0 0x1000 with
    | 0 -> Digestif.SHA256.get ctx
    | len ->
      let ctx = Digestif.SHA256.feed_bytes ctx ~off:0 ~len tmp in
      go ctx
    | exception End_of_file -> Digestif.SHA256.get ctx in
  go Digestif.SHA256.empty

let () = match Sys.argv with
  | [| _; filename; |] when Sys.file_exists filename ->
    let ic = open_in filename in
    let hash = sum ic in
    close_in ic ; print_endline (Digestif.SHA256.to_hex hash)
  | [| _ |] ->
    let hash = sum stdin in
    print_endline (Digestif.SHA256.to_hex hash)
  | _ -> Format.eprintf "%s [<filename>]\n%!" Sys.argv.(0)

