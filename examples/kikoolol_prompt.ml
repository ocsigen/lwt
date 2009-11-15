open Lwt
open Lwt_term
open Lwt_read_line

let time, set_time = React.S.create ~eq:(fun _ _ -> false) ()
  (* Signal changing every seconds *)

(* Update time every seconds *)
let rec update_time () =
  set_time ();
  lwt () = Lwt_unix.sleep 1.0 in
  update_time ()
in
update_time ()

class kikoolol =
  let prompt = ref (React.S.const []) in
object(self)
  inherit read_line ~prompt:[] ()

  method prompt = !prompt

  initializer
    prompt := React.S.l3 begin fun columns engine_state () ->
      let tm = Unix.localtime (Unix.time ()) in
      let columns = React.S.value Lwt_term.columns in
      [Bold;
       fg lblue; Text "─( ";
       fg lmagenta; textf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;
       fg lblue; Text " )─"; Text(Text.repeat (columns - 14) "─");
       Bold; fg lwhite; Text "mode: "; Reset; Text(match engine_state.Engine.mode with
                                                     | Engine.Selection _ -> "selection"
                                                     | Engine.Edition _ -> "edition"
                                                     | Engine.Search _ -> "searching"); Text "\n";
       Bold; fg lwhite; Text "clipboard: "; Reset; Text(React.S.value clipboard#contents); Text "\n";
       Bold; fg lwhite; Text "length: "; Reset; Text(string_of_int (Text.length (Engine.all_input engine_state))); Text "\n";
       Bold;
       fg lred; Text(try Sys.getenv "USER" with Not_found -> "");
       fg lgreen; Text "@";
       fg lblue; Text(Unix.gethostname ());
       fg lgreen; Text " $ "]
    end Lwt_term.columns self#engine_state time
end

let () = Lwt_main.run begin
  lwt line = (new kikoolol)#run in
  Lwt_text.printl line
end
