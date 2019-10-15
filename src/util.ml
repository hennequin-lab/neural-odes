open Typ

let dump_binary filename x =
  let module Mat = Owl.Dense.Matrix.S in
  let file = Unix.(openfile filename [ O_RDWR; O_CREAT; O_TRUNC ] 0o660) in
  let x_mem =
    Unix.map_file
      file
      Bigarray.Float32
      Bigarray.c_layout
      true
      [| Mat.row_num x; Mat.col_num x |]
  in
  Bigarray.Genarray.blit x x_mem;
  Unix.close file


let dump_heatmap filename (x, y, z) =
  assert (Mat.col_num z = Mat.col_num x);
  assert (Mat.row_num z = Mat.col_num y);
  let n = Mat.create 1 1 (float (Mat.col_num z)) in
  let m = Mat.concat_vh [| [| n; x |]; [| Mat.transpose y; z |] |] in
  dump_binary filename (Owl.Dense.Matrix.Generic.cast_d2s m)


