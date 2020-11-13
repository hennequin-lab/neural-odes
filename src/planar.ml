open Owl

let make_grid a b res =
  let grid = Mat.linspace a b res in
  let grid' = Mat.to_array grid in
  let points =
    grid'
    |> Array.map (fun x ->
           grid'
           |> Array.map (fun y -> Mat.of_array [| x; y |] 1 2)
           |> Mat.concatenate ~axis:0)
    |> Mat.concatenate ~axis:0
  in
  grid, points


let save_density ~res log_density filename =
  let grid, z1 = make_grid (-2.5) 2.5 res in
  let d =
    log_density z1 |> Mat.exp |> fun m -> Mat.reshape m [| res; res |] |> Mat.transpose
  in
  Util.dump_heatmap filename (grid, grid, d);
  let dx2 = Maths.sqr (5. /. float res) in
  Mat.save_txt (Mat.create 1 1 (dx2 *. Mat.sum' d)) ~out:(filename ^ "_sum")
