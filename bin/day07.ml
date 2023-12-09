type hand_class =
  | High
  | OnePair
  | TwoPair
  | Three
  | FullHouse
  | Four
  | Five
[@@deriving show, enum]

type hand =
  { cards : int list
  ; bid : int
  ; class_ : hand_class
  }
[@@deriving show]

module CardMap = Map.Make (Char)

let process_hands lines ~card_to_number ~hand_to_class =
  lines
  |> List.map (fun line ->
    line
    |> String.split_on_char ' '
    |> function
    | [ cards; bid ] ->
      { cards = String.to_seq cards |> Seq.map card_to_number |> List.of_seq
      ; bid = int_of_string bid
      ; class_ = hand_to_class cards
      }
    | _ -> failwith "Invalid line")
  |> List.sort (fun hand_a hand_b ->
    match hand_a.class_, hand_b.class_ with
    | a, b when a = b -> compare hand_a.cards hand_b.cards
    | _ ->
      compare
        (hand_class_to_enum hand_a.class_)
        (hand_class_to_enum hand_b.class_))
;;

let count_cards cards =
  cards
  |> String.to_seq
  |> Seq.fold_left
       (fun map card ->
         CardMap.update
           card
           (function
             | Some count -> Some (count + 1)
             | None -> Some 1)
           map)
       CardMap.empty
  |> CardMap.to_list
  |> List.sort (fun a b -> compare (snd b) (snd a))
;;

let total_winnings =
  Base.List.foldi ~f:(fun idx acc hand -> acc + (hand.bid * (idx + 1))) ~init:0
;;

module Part1 = struct
  let card_to_number = function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | c when c <= '9' && c >= '2' -> Char.code c - Char.code '0'
    | _ -> 1
  ;;

  let hand_to_class cards =
    cards
    |> count_cards
    |> begin
         function
         | [ _ ] -> Five
         | [ (_, 4); _ ] -> Four
         | [ (_, 3); _ ] -> FullHouse
         | [ (_, 3); _; _ ] -> Three
         | [ (_, 2); (_, 2); _ ] -> TwoPair
         | [ (_, 2); _; _; _ ] -> OnePair
         | [ _; _; _; _; _ ] -> High
         | _ -> failwith "invalid hand"
       end
  ;;
end

let () =
  let result =
    Advent.read_lines "./inputs/day07.in"
    |> process_hands
         ~card_to_number:Part1.card_to_number
         ~hand_to_class:Part1.hand_to_class
    |> total_winnings
  in
  Fmt.pr "@.%d@." result
;;

module Part2 = struct
  let card_to_number = function
    | 'A' -> 13
    | 'K' -> 12
    | 'Q' -> 11
    | 'T' -> 10
    | c when c <= '9' && c >= '2' -> Char.code c - Char.code '0'
    | 'J' -> 1
    | _ -> 0
  ;;

  let hand_to_class cards =
    cards
    |> count_cards
    |> begin
         function
         | [ ('J', 4); _ ] | [ _ ] -> Five
         | [ (_, 4); ('J', 1) ] -> Five
         | [ (_, 4); (_, 1) ] -> Four
         | [ ('J', 3); _ ] | [ (_, 3); ('J', 2) ] -> Five
         | [ (_, 3); _ ] -> FullHouse
         | [ ('J', 3); _; _ ]
         | [ (_, 3); ('J', 1); _ ]
         | [ (_, 3); _; ('J', 1) ] -> Four
         | [ (_, 3); _; _ ] -> Three
         | [ ('J', 2); (_, 2); _ ] | [ (_, 2); ('J', 2); _ ] -> Four
         | [ (_, 2); (_, 2); ('J', 1) ] -> FullHouse
         | [ (_, 2); (_, 2); _ ] -> TwoPair
         | [ ('J', 2); _; _; _ ]
         | [ (_, 2); ('J', 1); _; _ ]
         | [ (_, 2); _; ('J', 1); _ ]
         | [ (_, 2); _; _; ('J', 1) ] -> Three
         | [ (_, 2); _; _; _ ] -> OnePair
         | [ ('J', 1); _; _; _; _ ]
         | [ _; ('J', 1); _; _; _ ]
         | [ _; _; ('J', 1); _; _ ]
         | [ _; _; _; ('J', 1); _ ]
         | [ _; _; _; _; ('J', 1) ] -> OnePair
         | [ _; _; _; _; _ ] -> High
         | _ -> failwith "unknown hand"
       end
  ;;
end

let () =
  let result =
    Advent.read_lines "./inputs/day07.in"
    |> process_hands
         ~card_to_number:Part2.card_to_number
         ~hand_to_class:Part2.hand_to_class
    |> total_winnings
  in
  Fmt.pr "@.%d@." result
;;
