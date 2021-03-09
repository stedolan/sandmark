#!/usr/bin/env ocaml

#use "topfind" ;;
#require "unix" ;;

let xen = "xen_linkopts = \"-l:rakia/xen/librakia_xen_stubs.a\""
let freestanding = "freestanding_linkopts = \"-l:rakia/freestanding/librakia_freestanding_stubs.a\""
let meta = "_build/default/META.digestif"

let new_line = '\n'

let output_line oc line =
  output_string oc line ;
  output_char oc new_line

let () =
  Unix.chmod meta 0o644 ;
  let oc = open_out_gen [ Open_append ] 0o644 meta in
  output_line oc xen ;
  output_line oc freestanding ;
  close_out oc
