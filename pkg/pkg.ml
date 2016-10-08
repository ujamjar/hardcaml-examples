#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let parse contents =
  let lines = String.cuts ~sep:'\n' contents in
  let add_mod acc l =
    let m = String.trim @@ match String.cut ~sep:'#' l with
    | None -> l
    | Some (m, _ (* comment *)) -> m
    in
    if m = "" then acc else m :: acc
  in
  Ok (List.fold_left add_mod [] lines)

let mlpack ?cond name =  
  let dir = Fpath.dirname name in
  let base = Fpath.basename name in
  let modls = (* modules within the pack *)
    let name = Fpath.(dir // base ^ ".mlpack") in
    OS.File.read name >>= parse
  in
  let intf modls = (* install interfaces for modules in the library - .cmti/.mli *)
    Ok (List.map 
      (fun m ->
         let name = Fpath.(dir // Atring.String.Ascii.uncapitalize m) in
           Pkg.lib ?cond ~exts:Exts.(exts [".cmti"; ".mli"]) name
      ) modls)
  in
  let name = Fpath.(dir // base) in
  let pkg l = 
    let lib = 
      Pkg.lib ?cond 
        ~exts:Exts.(exts [".a"; ".cmi"; ".cma"; ".cmx"; ".cmxa"; ".cmxs"; ".cmti"]) 
        name 
    in
    Ok (lib :: l)
  in
  (modls >>= intf >>= pkg) |> Log.on_error_msg ~use:(fun () -> [])

let targets name =
  OS.File.read name >>= parse >>= (fun modls ->
    let exes = List.map (fun n -> "hc_" ^ n) modls in
    let js = List.map (fun n -> "hcjs_" ^ n ^ ".js") modls in
    let ww = List.map (fun n -> "hcww_" ^ n ^ ".js") modls in
    let html = List.map (fun n -> "html/" ^ n ^ ".html") modls in
    Ok (List.map Pkg.bin exes @
        List.map Pkg.share js @
        List.map Pkg.share ww @
        List.map (Pkg.share ~built:false) html)) |> 
    Log.on_error_msg ~use:(fun () -> [])

let () = 
  Pkg.describe "hardcaml-examples" @@ fun c ->
  Ok (
    mlpack "examples/HardCamlExamples"  @
    targets "apps.targets"
  )

