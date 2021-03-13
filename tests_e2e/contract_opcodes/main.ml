let cmd_output cmd args = 
  let result = { contents = [] } in
  let (readme, writeme) = Unix.pipe () in
  let _pid =
    Unix.create_process cmd (Array.of_list (cmd :: args)) Unix.stdin writeme Unix.stderr in
  let () = Unix.close writeme in
  let in_channel = Unix.in_channel_of_descr readme in
  let () = try
    while true do
      result.contents <-
        ((input_line in_channel) :: (result.contents))
    done
  with
  | End_of_file -> ()
  in
  let () = Unix.close readme in
  String.concat "\n" (List.rev (!result))

let run_script_result script_output =
  let pattern_str = "storage\n *\\(.*\\)\nemitted operations" in
  let _ = Str.search_forward (Str.regexp pattern_str) script_output 0 in
  Str.matched_group 1 script_output

let () =
  print_endline @@ run_script_result {|storage
 100
emitted operations
|}
