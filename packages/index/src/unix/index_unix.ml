(* The MIT License

Copyright (c) 2019 Craig Ferguson <craig@tarides.com>
                   Thomas Gazagnaire <thomas@tarides.com>
                   Ioana Cristescu <ioana@tarides.com>
                   Clément Pascutto <clement@tarides.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software. *)

let src = Logs.Src.create "index_unix" ~doc:"Index_unix"

module Log = (val Logs.src_log src : Logs.LOG)

exception RO_not_allowed

let current_version = "00000001"

module Stats = Index.Stats

module IO : Index.IO = struct
  external ( ++ ) : int64 -> int64 -> int64 = "%int64_add"

  external ( -- ) : int64 -> int64 -> int64 = "%int64_sub"

  type t = {
    mutable file : string;
    mutable header : int64;
    mutable raw : Raw.t;
    mutable offset : int64;
    mutable flushed : int64;
    mutable fan_size : int64;
    readonly : bool;
    buf : Buffer.t;
    flush_callback : unit -> unit;
  }

  let flush ?no_callback ?(with_fsync = false) t =
    if t.readonly then raise RO_not_allowed;
    let buf = Buffer.contents t.buf in
    let offset = t.offset in
    Buffer.clear t.buf;
    if buf = "" then ()
    else (
      (match no_callback with Some () -> () | None -> t.flush_callback ());
      Log.debug (fun l -> l "[%s] flushing %d bytes" t.file (String.length buf));
      Raw.unsafe_write t.raw ~off:t.flushed buf;
      Raw.Offset.set t.raw offset;
      assert (t.flushed ++ Int64.of_int (String.length buf) = t.header ++ offset);
      t.flushed <- offset ++ t.header);
    if with_fsync then Raw.fsync t.raw

  let rename ~src ~dst =
    flush ~with_fsync:true src;
    Raw.close dst.raw;
    Unix.rename src.file dst.file;
    Buffer.clear dst.buf;
    src.file <- dst.file;
    dst.header <- src.header;
    dst.fan_size <- src.fan_size;
    dst.offset <- src.offset;
    dst.flushed <- src.flushed;
    dst.raw <- src.raw

  let close t =
    if not t.readonly then Buffer.clear t.buf;
    Raw.close t.raw

  let auto_flush_limit = 1_000_000L

  let append t buf =
    if t.readonly then raise RO_not_allowed;
    Buffer.add_string t.buf buf;
    let len = Int64.of_int (String.length buf) in
    t.offset <- t.offset ++ len;
    if t.offset -- t.flushed > auto_flush_limit then flush t

  let read t ~off ~len buf =
    if not t.readonly then assert (t.header ++ off <= t.flushed);
    Raw.unsafe_read t.raw ~off:(t.header ++ off) ~len buf

  let offset t = t.offset

  let get_generation t =
    let i = Raw.Generation.get t.raw in
    Log.debug (fun m -> m "get_generation: %Ld" i);
    i

  let get_fanout t = Raw.Fan.get t.raw

  let set_fanout t buf =
    assert (Int64.equal (Int64.of_int (String.length buf)) t.fan_size);
    Raw.Fan.set t.raw buf

  module Header = struct
    type header = { offset : int64; generation : int64 }

    let pp ppf { offset; generation } =
      Format.fprintf ppf "{ offset = %Ld; generation = %Ld }" offset generation

    let get t =
      let Raw.Header.{ offset; generation; _ } = Raw.Header.get t.raw in
      t.offset <- offset;
      let headers = { offset; generation } in
      Log.debug (fun m -> m "[%s] get_headers: %a" t.file pp headers);
      headers

    let set t { offset; generation } =
      let version = current_version in
      Log.debug (fun m ->
          m "[%s] set_header %a" t.file pp { offset; generation });
      Raw.Header.(set t.raw { offset; version; generation })
  end

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> failwith (Printexc.to_string e)
    | e -> raise e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | e -> raise e

  let protect f x = try f x with e -> protect_unix_exn e

  let safe f x = try f x with e -> ignore_enoent e

  let mkdir dirname =
    let rec aux dir k =
      if Sys.file_exists dir && Sys.is_directory dir then k ()
      else (
        if Sys.file_exists dir then safe Unix.unlink dir;
        (aux [@tailcall]) (Filename.dirname dir) @@ fun () ->
        protect (Unix.mkdir dir) 0o755;
        k ())
    in
    (aux [@tailcall]) dirname (fun () -> ())

  let clear ~generation t =
    t.offset <- 0L;
    t.flushed <- t.header;
    Header.set t { offset = t.offset; generation };
    Raw.Fan.set t.raw "";
    Buffer.clear t.buf;
    Raw.fsync t.raw

  let () = assert (String.length current_version = 8)

  let v ?(flush_callback = fun () -> ()) ~readonly ~fresh ~generation ~fan_size
      file =
    let v ~fan_size ~offset raw =
      let header = 8L ++ 8L ++ 8L ++ 8L ++ fan_size in
      {
        header;
        file;
        offset;
        raw;
        readonly;
        fan_size;
        buf = Buffer.create (4 * 1024);
        flushed = header ++ offset;
        flush_callback;
      }
    in
    let mode = Unix.(if readonly then O_RDONLY else O_RDWR) in
    mkdir (Filename.dirname file);
    match Sys.file_exists file with
    | false ->
        let x = Unix.openfile file Unix.[ O_CREAT; O_CLOEXEC; mode ] 0o644 in
        let raw = Raw.v x in
        Raw.Offset.set raw 0L;
        Raw.Fan.set_size raw fan_size;
        Raw.Version.set raw current_version;
        Raw.Generation.set raw generation;
        v ~fan_size ~offset:0L raw
    | true ->
        let x = Unix.openfile file Unix.[ O_EXCL; O_CLOEXEC; mode ] 0o644 in
        let raw = Raw.v x in
        if readonly && fresh then
          Fmt.failwith "IO.v: cannot reset a readonly file"
        else if fresh then (
          Raw.Offset.set raw 0L;
          Raw.Fan.set_size raw fan_size;
          Raw.Version.set raw current_version;
          Raw.Generation.set raw generation;
          v ~fan_size ~offset:0L raw)
        else
          let version = Raw.Version.get raw in
          if version <> current_version then
            Fmt.failwith "Io.v: unsupported version %s (current version is %s)"
              version current_version;

          let offset = Raw.Offset.get raw in
          let fan_size = Raw.Fan.get_size raw in
          v ~fan_size ~offset raw

  let exists = Sys.file_exists

  let size { raw; _ } = (Raw.fstat raw).st_size

  module Lock = struct
    type t = { path : string; fd : Unix.file_descr }

    exception Locked of string

    let unsafe_lock op f =
      mkdir (Filename.dirname f);
      let fd = Unix.openfile f [ Unix.O_CREAT; Unix.O_RDWR ] 0o600
      and pid = string_of_int (Unix.getpid ()) in
      let pid_len = String.length pid in
      try
        Unix.lockf fd op 0;
        if Unix.single_write_substring fd pid 0 pid_len <> pid_len then (
          Unix.close fd;
          failwith "Unable to write PID to lock file")
        else Some fd
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) ->
          Unix.close fd;
          None
      | e ->
          Unix.close fd;
          raise e

    let with_ic path f =
      let ic = open_in path in
      let a = f ic in
      close_in ic;
      a

    let err_rw_lock path =
      let line = with_ic path input_line in
      let pid = int_of_string line in
      Log.err (fun l ->
          l
            "Cannot lock %s: index is already opened in write mode by PID %d. \
             Current PID is %d."
            path pid (Unix.getpid ()));
      raise (Locked path)

    let lock path =
      Log.debug (fun l -> l "Locking %s" path);
      match unsafe_lock Unix.F_TLOCK path with
      | Some fd -> { path; fd }
      | None -> err_rw_lock path

    let unlock { path; fd } =
      Log.debug (fun l -> l "Unlocking %s" path);
      Unix.close fd

    let pp_dump path =
      match Sys.file_exists path with
      | false -> None
      | true ->
          let contents =
            with_ic path (fun ic ->
                really_input_string ic (in_channel_length ic))
          in
          Some (fun ppf -> Fmt.string ppf contents)
  end
end

module Semaphore = struct
  module S = Semaphore_compat.Semaphore.Binary

  let is_held t =
    let acquired = S.try_acquire t in
    if acquired then S.release t;
    not acquired

  include S

  let acquire n t =
    let x = Mtime_clock.counter () in
    S.acquire t;
    let y = Mtime_clock.count x in
    if Mtime.Span.to_s y > 1. then
      Log.warn (fun l -> l "Semaphore %s was blocked for %a" n Mtime.Span.pp y)

  let with_acquire n t f =
    acquire n t;
    Fun.protect ~finally:(fun () -> S.release t) f
end

module Thread = struct
  type 'a t =
    | Async of { thread : Thread.t; result : ('a, exn) result option ref }
    | Value of 'a

  let async f =
    let result = ref None in
    let protected_f x =
      try result := Some (Ok (f x))
      with exn ->
        result := Some (Error exn);
        raise exn
    in
    let thread = Thread.create protected_f () in
    Async { thread; result }

  let yield = Thread.yield

  let return a = Value a

  let await t =
    match t with
    | Value v -> Ok v
    | Async { thread; result } -> (
        let () = Thread.join thread in
        match !result with
        | Some (Ok _ as o) -> o
        | Some (Error exn) -> Error (`Async_exn exn)
        | None -> assert false)
end

module Make (K : Index.Key.S) (V : Index.Value.S) =
  Index.Make (K) (V) (IO) (Semaphore) (Thread)
module Syscalls = Syscalls

module Private = struct
  module IO = IO
  module Raw = Raw
  module Make (K : Index.Key.S) (V : Index.Value.S) =
    Index.Private.Make (K) (V) (IO) (Semaphore) (Thread)
end
