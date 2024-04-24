(** Borrowed from https://github.com/dbuenzli/hmap/blob/master/src/hmap.ml

    Copyright (c) 2016 Daniel C. Bünzli

    Permission to use, copy, modify, and/or distribute this software for any
    purpose with or without fee is hereby granted, provided that the above
    copyright notice and this permission notice appear in all copies.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
    WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
    ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
    WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
    ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

module T = struct
  type _ t = ..
end

module type T = sig
  type t
  type _ T.t += T : t T.t
end

type 'a t = (module T with type t = 'a)

let create () (type s) =
  let module M = struct
    type t = s
    type _ T.t += T : t T.t
  end in
  (module M : T with type t = s)

type ('a, 'b) eq = Eq : ('a, 'a) eq

let equal : type a b. a t -> b t -> (a, b) eq option =
 fun a b ->
  let module A = (val a : T with type t = a) in
  let module B = (val b : T with type t = b) in
  match A.T with B.T -> Some Eq | _ -> None
