(* Simple rendering of markdown using ANSI escape codes.
 
  * text_of_md largely reuses the input text format so the
    markdown should be largely readable!

  * bold and italic for Emph and Strong

  * reverse for headings
  
  * (maybe) underline for things like block quote?

  * default (reset) colour for everything else

  Avoid colour as it could cause clashes with terminal preferences and it's
  difficult to detect.

  A much better version would implement proper paragraph re-formatting and
  track tab alignment for things like code and quote.  

*)

open Astring
open Omd_representation
open Omd_utils

(* Styling. *)

type style = [ `bold | `italic | `underline | `reverse ]
module S = Set.Make(struct type t = style let compare = compare end)

let mk s = List.fold_right S.add s S.empty

let all_styles = mk [`bold; `italic; `underline; `reverse] 

(* set given styles, or hard reset *)
let style b s = 
  let s = S.elements s in
  if s = [] then Buffer.add_string b "\x1b[0m" (* reset *)
  else
    Buffer.add_string b @@ 
    "\x1b[" ^ 
      String.concat ~sep:";" (List.map (function
        | `bold -> "1"
        | `italic -> "3"
        | `underline -> "4"
        | `reverse -> "7"
      ) s)^ "m"

(* set new styles, unless they were already set *)
let push_styles b cur styles = 
  let new_styles = S.union cur styles in
  let s = S.diff new_styles cur in 
  if s = S.empty then styles 
  else (style b s; new_styles)

(* undo push_styles *)
let pop_styles b cur = 
  (style b S.empty; style b cur)

let text_of_md md =
  let b = Buffer.create 128 in
  let rec loop sty md = 
    (* add then remove styles *)
    let stylish styles md = 
      let styles = mk styles in
      let style = push_styles b sty styles in
      loop style md;
      pop_styles b sty 
    in
    let loop = loop sty in
    (* heading *)
    let h n md tl = 
      Buffer.add_string b (String.v ~len:n (fun _ -> '#'));
      Buffer.add_char b ' ';
      stylish [`reverse] md;
      loop tl
    in
    let htmlentities ~md x = x in
    match md with
    | X _ :: tl ->
        loop tl
    | Blockquote q :: tl ->
        loop q;
        loop tl
    | Ref(rc, name, text, fallback) :: tl ->
        Buffer.add_string b (htmlentities ~md:true name);
        loop tl
    | Img_ref(rc, name, alt, fallback) :: tl ->
        Buffer.add_string b (htmlentities ~md:true name);
        loop tl
    | Paragraph md :: tl ->
        loop md;
        Buffer.add_char b '\n';
        Buffer.add_char b '\n';
        loop tl
    | Img(alt, src, title) :: tl ->
        Buffer.add_string b (htmlentities ~md:true alt);
        loop tl
    | Text t :: tl ->
        Buffer.add_string b (htmlentities ~md:true t);
        loop tl
    | Raw t :: tl ->
        Buffer.add_string b t;
        loop tl
    | Raw_block t :: tl ->
        Buffer.add_char b '\n';
        Buffer.add_string b t;
        Buffer.add_char b '\n';
        loop tl
    | Emph md :: tl ->
        (* STYLE *)
        stylish [`italic] md;
        loop tl
    | Bold md :: tl ->
        (* STYLE *)
        stylish [`bold] md;
        loop tl
      
    (* not sure whats going on here; I only see Ulp!? *)
    | Ul l :: tl ->
        List.iter (fun item -> Buffer.add_string b "* "; loop item; Buffer.add_char b '\n') l;
        loop tl
    | Ol l :: tl ->
        List.iteri (fun i item -> Buffer.add_string b (string_of_int i ^ ". "); 
                                  loop item; Buffer.add_char b '\n') l;
        loop tl
    | Ulp l :: tl ->
        List.iter (fun item -> Buffer.add_string b "* "; loop item) l;
        loop tl
    | Olp l :: tl ->
        List.iteri (fun i item -> Buffer.add_string b (string_of_int i ^ ". "); 
                                  loop item) l;
        loop tl
    | Code_block(lang, c) :: tl ->
        Buffer.add_string b (htmlentities ~md:false c);
        loop tl
    | Code(lang, c) :: tl ->
        Buffer.add_string b (htmlentities ~md:false c);
        loop tl
    | Br :: tl ->
        loop tl
    | Hr :: tl ->
        loop tl
    | Html(tagname, attrs, body) :: tl ->
        loop body;
        loop tl
    | Html_block(tagname, attrs, body) :: tl ->
        loop body;
        loop tl
    | Html_comment s :: tl ->
        loop tl
    | Url (href,s,title) :: tl ->
        loop s;
        loop tl
    | H1 md :: tl -> h 1 md tl
    | H2 md :: tl -> h 2 md tl
    | H3 md :: tl -> h 3 md tl
    | H4 md :: tl -> h 5 md tl
    | H5 md :: tl -> h 5 md tl
    | H6 md :: tl -> h 6 md tl 
    | NL :: tl ->
        Buffer.add_string b "\n";
        loop tl
    | [] -> ()
  in
    style b S.empty;
    loop S.empty md;
    style b S.empty;
    Buffer.contents b

