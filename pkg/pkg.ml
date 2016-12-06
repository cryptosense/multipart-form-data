#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "multipart-form-data" @@ fun c ->
  Ok [
    Pkg.mllib "lib/multipart.mllib";
    Pkg.test "test/tests";
  ]
