let lines = Advent.read_lines "./inputs/day07.in"

type hand =
  { cards : string
  ; bid : int
  }
[@@deriving show]

type hand_class =
  | High
  | OnePair
  | TwoPair
  | Three
  | FullHouse
  | Four
  | Five
[@@deriving enum]

module CardMap = Map.Make (Char)

let process_hand line =
  line
  |> String.split_on_char ' '
  |> function
  | [ cards; bid ] -> { cards; bid = int_of_string bid }
  | _ -> failwith "Invalid line"
;;

let count_cards hand =
  let counts_map = CardMap.empty in
  let counts =
    hand.cards
    |> String.to_seq
    |> Seq.fold_left
         (fun map card ->
           CardMap.update
             card
             (function
               | Some count -> Some (count + 1)
               | None -> Some 1)
             map)
         counts_map
    |> CardMap.to_list
    |> List.sort (fun a b -> compare (snd b) (snd a))
  in
  counts
;;

module Part1 = struct
  let card_to_number = function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | '9' -> 9
    | '8' -> 8
    | '7' -> 7
    | '6' -> 6
    | '5' -> 5
    | '4' -> 4
    | '3' -> 3
    | '2' -> 2
    | _ -> 1
  ;;

  let hand_to_class hand =
    hand
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
    |> hand_class_to_enum
  ;;
end

let () =
  let hands =
    lines
    |> List.map process_hand
    |> List.sort (fun hand_a hand_b ->
      let class_a = Part1.hand_to_class hand_a in
      let class_b = Part1.hand_to_class hand_b in
      match class_a, class_b with
      | a, b when a = b ->
        let cards_a =
          String.to_seq hand_a.cards
          |> Seq.map Part1.card_to_number
          |> List.of_seq
        in
        let cards_b =
          String.to_seq hand_b.cards
          |> Seq.map Part1.card_to_number
          |> List.of_seq
        in
        compare cards_a cards_b
      | _ -> compare class_a class_b)
  in
  let result =
    hands
    |> Base.List.foldi
         ~f:(fun idx acc hand -> acc + (hand.bid * (idx + 1)))
         ~init:0
  in
  Fmt.pr "@.%d@." result
;;

module Part2 = struct
  let card_to_number = function
    | 'A' -> 13
    | 'K' -> 12
    | 'Q' -> 11
    | 'T' -> 10
    | '9' -> 9
    | '8' -> 8
    | '7' -> 7
    | '6' -> 6
    | '5' -> 5
    | '4' -> 4
    | '3' -> 3
    | '2' -> 2
    | 'J' -> 1
    | _ -> 0
  ;;

  let hand_to_class hand =
    hand
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
    |> hand_class_to_enum
  ;;
end

let () =
  let hands =
    lines
    |> List.map process_hand
    |> List.sort (fun hand_a hand_b ->
      let class_a = Part2.hand_to_class hand_a in
      let class_b = Part2.hand_to_class hand_b in
      match class_a, class_b with
      | a, b when a = b ->
        let cards_a =
          String.to_seq hand_a.cards
          |> Seq.map Part2.card_to_number
          |> List.of_seq
        in
        let cards_b =
          String.to_seq hand_b.cards
          |> Seq.map Part2.card_to_number
          |> List.of_seq
        in
        compare cards_a cards_b
      | _ -> compare class_a class_b)
  in
  let result =
    hands
    |> Base.List.foldi
         ~f:(fun idx acc hand -> acc + (hand.bid * (idx + 1)))
         ~init:0
  in
  Fmt.pr "@.%d@." result
;;
