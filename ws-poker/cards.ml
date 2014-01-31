let all_cards = ref [];;

let () = 
  let suits = ["C"; "D"; "H"; "S"] in
  let faces = ["A"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10";
               "J"; "Q"; "K"] in
  all_cards := Util.lists_prod suits faces
;;
