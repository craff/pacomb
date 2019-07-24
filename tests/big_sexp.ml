

let random_id () =
  let c = Char.chr (Char.code 'a' + Random.int 26) in
  let n = string_of_int (Random.int 1000) in
  String.make 1 c ^ n

let print_spaces lvl =
  print_string "\n";
  for _ = 1 to lvl do
    print_string "\t"
  done

let sqrt n = Random.int (truncate ((float n) ** 0.7) + 1)

let rec expr lvl n =
  if n <= 0 then ()
  else if n = 1 then print_string (random_id ())
  else
    begin
      print_spaces lvl;
      let lvl = lvl + 1 in
      print_string "(";
      let s = sqrt n in
      let n = n - 1 in
      let rec fn s n =
        if s <= 1 then expr lvl n
        else
          begin
            let q = n/2 in
            let n0 = n/s - q/2 + Random.int(min 1 q) in
            let n0 = max 1 (min n n0) in
            expr lvl n0;
            print_string " ";
            fn (s-1) (n - n0)
          end
      in
      fn s n;
      print_string ")"

    end

let _ = expr 0 (int_of_string Sys.argv.(1)); print_newline ()
