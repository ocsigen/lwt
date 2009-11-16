
(* This example show how to fully customize read-line *)

open Lwt
open Lwt_term
open Lwt_read_line

(* Signal changing every seconds *)
let time =
  let time, set_time = React.S.create ~eq:(fun _ _ -> false) () in

  (* Update time every seconds *)
  let rec update_time () =
    set_time ();
    lwt () = Lwt_unix.sleep 1.0 in
    update_time ()
  in
  ignore (update_time ());

  time

(* Customized read-line class *)
class kikoolol =
object(self)
  inherit read_line ~prompt:[] ()

  val mutable prompt = React.S.const []
  method prompt = prompt

  initializer
    prompt <- React.S.l4 begin fun columns engine_state clipboard () ->
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
       Bold; fg lwhite; Text "clipboard: "; Reset; Text clipboard; Text "\n";
       Bold; fg lwhite; Text "length: "; Reset; Text(string_of_int (Text.length (Engine.all_input engine_state))); Text "\n";
       Bold;
       fg lred; Text(try Sys.getenv "USER" with Not_found -> "");
       fg lgreen; Text "@";
       fg lblue; Text(Unix.gethostname ());
       fg lgreen; Text " $ "]
    end Lwt_term.columns self#engine_state self#clipboard#contents time;

    self#set_message (Some "No completion.")
end

let () = Lwt_main.run begin
  lwt line = (new kikoolol)#run in
  Lwt_text.printl line
end
