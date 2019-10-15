open Owl

module Make (P : sig
  val file : string
end) =
struct
  let dim = 2

  let sample =
    let img = Mat.load_txt P.file in
    let img = Mat.(255. $- img) in
    let z_max = Mat.max' img in
    let n, m = Mat.shape img in
    let normalise (x, y) =
      Mat.of_array
        [| 5. *. (float x -. (float m /. 2.)) /. float m
         ; 5. *. (float y -. (float n /. 2.)) /. float n
        |]
        1
        2
    in
    let rec sample () =
      let x, y = Random.int m, Random.int n in
      if Random.float z_max < Mat.get img y x then normalise (x, y) else sample ()
    in
    sample


  let sample_batch batch_size =
    Array.init batch_size (fun _ -> sample ()) |> Mat.concatenate ~axis:0
end
