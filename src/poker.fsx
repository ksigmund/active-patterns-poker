module Cards =
    open Microsoft.FSharp.Reflection

    // Spades, Clubs, Hearts, Diamonds
    type Suit =
        | Spades
        | Clubs
        | Hearts
        | Diamonds

        // Override the ToString method to return the Unicode symbole for the suit
        override this.ToString() =
            match this with
            | Spades -> "\u2660"    // ♠
            | Clubs -> "\u2663"     // ♣
            | Hearts -> "\u2665"    // ♥
            | Diamonds -> "\u2666"  // ♦
       
    // 2 - 10 or Jack or Queen or King or Ace
    type Rank =
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | Ten
        | Jack
        | Queen
        | King
        | Ace

        // Returns the numerical value of the rank
        member this.Value =
            match this with
            | Two -> 2
            | Three -> 3
            | Four -> 4
            | Five -> 5
            | Six -> 6
            | Seven -> 7
            | Eight -> 8
            | Nine -> 9
            | Ten -> 10
            | Jack -> 11
            | Queen -> 12
            | King -> 13
            | Ace -> 14

        // Override the ToString method to display the card rank
        override this.ToString() =
            match this with
            | Two -> "2"
            | Three -> "3"
            | Four -> "4"
            | Five -> "5"
            | Six -> "6"
            | Seven -> "7"
            | Eight -> "8"
            | Nine -> "9"
            | Ten -> "10"
            | Jack -> "J"
            | Queen -> "Q"
            | King -> "K"
            | Ace -> "A"

    // A card is a suit and a value
    type Card =
        { Rank: Rank
          Suit: Suit }

        // Get a random instance of a discriminated union (suit or rank)
        static member random<'a>() =
            let unionCases = typeof<'a> |> FSharpType.GetUnionCases
            let i = System.Random().Next(unionCases.Length - 1)
            FSharpValue.MakeUnion(unionCases.[i], [||]) :?> 'a

        static member randomCard =
            { Rank = Card.random<Rank>()
              Suit = Card.random<Suit>() }

    // A hand is five cards
    type FiveCards = Card * Card * Card * Card * Card

module Poker =
    open Cards

    type PokerHand =
        private
            { Cards: FiveCards }

        static member map f =
            function
            | (a, b, c, d, e) -> (f a, f b, f c, f d, f e)

        static member asList values =
            match values with
            | (a, b, c, d, e) -> [ a; b; c; d; e ]

        member this.Suits = this.Cards |> PokerHand.map (fun c -> c.Suit) |> PokerHand.asList
        member this.Ranks = this.Cards |> PokerHand.map (fun c -> c.Rank)

        member this.HighCard =
            match this.Cards with
            | (_, _, _, _, high) -> high

        // Override the ToString method to display the hand
        override this.ToString() =
            this.Cards
            |> PokerHand.asList
            |> List.map (fun c -> $"{c.Rank}{c.Suit}")
            |> String.concat " "

    module PokerHand =

        // Smart constructor to create a hand from five cards.
        // This is the only way to create a hand; results in a list of cards sorted by rank.
        let create fiveCards =

            // Sort the tuple of cards by rank
            let sortedCards =
                fiveCards
                |> PokerHand.asList
                |> List.sortBy (fun c -> c.Rank.Value)
                |> (fun cards -> (cards.[0], cards.[1], cards.[2], cards.[3], cards.[4]))

            { Cards = sortedCards }

        // Single-case active pattern that deconstructs a poker hand into a list of its cards
        let private (|Cards|) (hand: PokerHand) =
            hand.Cards |> PokerHand.asList

        // Singe-case active pattern that deconstructs a poker hand into the ranks for each card
        let private (|Ranks|) (hand: PokerHand) =
            hand.Ranks

        //  Singe-case active pattern that deconstructs a poker hand into the suits for each card
        let private (|Suits|) (hand: PokerHand) = 
            hand.Suits

        //  Singe-case active pattern that deconstructs a poker hand into the values for each card
        let private (|Values|) (hand: PokerHand) =
            hand.Ranks |> PokerHand.map (fun r -> r.Value)

        // Partial (parameterized) active pattern that returns the high card of a straight, otherwise None
        // The highest rank in the hand is provided as a parameter 
        let private (|AnyStraight|_|) (highRank: Rank) (hand: PokerHand) =
            let highValue = highRank.Value

            match hand with
            | Values values when values = (highValue - 4, highValue - 3, highValue - 2, highValue - 1, highValue) -> Some highRank
            | _ -> None

        // Partial active pattern that returns the high card of an Ace-high straight, otherwise None
        let private (|AceHighStraight|_|) = 
            (|AnyStraight|_|) Ace

        // Partial active pattern that is a special case for Ace low straight (Ace is normally a high card)
        let private (|FiveHighStraight|_|) =
            function
            | Ranks r when r = (Two, Three, Four, Five, Ace) -> Some Five
            | _ -> None

        // Partial active pattern that returns the high card of any type of straight, otherwise None
        // A Straight is five cards in numerical order, but not in the same suit.
        let (|Straight|_|) (hand: PokerHand) =
            match hand with
            | FiveHighStraight highCard
            | AnyStraight hand.HighCard.Rank highCard -> Some highCard
            | _ -> None

        // Partial active pattern that returns the suit of a flush, otherwise None.
        // A Flush is five cards in the same suit, not in numerical order.
        let (|Flush|_|) (hand: PokerHand) =
            match hand with
            | Suits suits when suits |> List.distinct |> List.length = 1 -> Some hand.HighCard.Suit
            | _ -> None

        // Partial active patter that returns the high card of a straight flush, otherwise None.
        // A Straight Flush is five cards in a row, all in the same suit.
        let (|StraightFlush|_|) =
            function
            | Straight rank & Flush suit -> Some({ Rank = rank; Suit = suit })
            | _ -> None

        // Partial active pattern that returns the suit of a royal flush, otherwise None
        // pokerpA Royal Flush is made out of 10, Jack, Queen, King, Ace, all of the same suit.
        let (|RoyalFlush|_|) =
            function
            | AceHighStraight _ & Flush suit -> Some suit
            | _ -> None

        // Singe-case active pattern that deconstructs a poker hand into a groups of cards with the same rank,
        // sorted in descending order by the number of cards in each group.
        let private (|GroupedRanks|) (hand: PokerHand) =
            hand.Ranks
            |> PokerHand.asList
            |> List.countBy id
            |> List.sortByDescending (fun (_, count) -> count)

        // Single-case active pattern that deconstructs a poker hand into its highest card.
        let private (|HighCard|) (hand: PokerHand) = 
            hand.HighCard

        // Multi-case active pattern that returns whether a hand represents
        // Four of a Kind:  same card in each of the four suits
        // Full House: pair plus three of a kind in the same hand. 
        // Three of a Kind: three of one card and two non-paired cards.
        // Two Pair: two cards of one rank, two cards of another rank, and one card of a third rank.
        // Pair: two cards of one rank, three cards of three other ranks.
        // High Card: no paired cards.
        let (|HighCard|Pair|TwoPair|ThreeOfAKind|FullHouse|FourOfAKind|) =
            function
            | GroupedRanks ((rank, 4) :: _) -> FourOfAKind rank            
            | GroupedRanks [ (r1, 3); (r2, 2) ] -> FullHouse(r1, r2)               
            | GroupedRanks ((rank, 3) :: _) -> ThreeOfAKind rank               
            | GroupedRanks [ (r1, 2); (r2, 2); _ ] -> TwoPair(r1, r2)
            | GroupedRanks ((rank, 2) :: _) -> Pair rank
            | HighCard highCard -> HighCard highCard.Rank

        // Match all possibly cases and provide a string representation of the hand
        // Cases are ordered from best to worst
        // Each case returns data that is relevant to the case (i.e. the high card for a high card hand or the suit only for a flush)
        let description (hand: PokerHand) : string =
            let desc =
                match hand with
                | RoyalFlush suit -> $"Royal Flush ({suit})"
                | StraightFlush card -> $"Straight Flush of {card.Suit} ({card.Rank} high)"
                | FourOfAKind rank -> $"Four of a Kind ({rank}'s)"
                | FullHouse (r1, r2) -> $"Full House ({r1}s over {r2}'s)"
                | Flush suit -> $"Flush ({suit})"
                | Straight rank -> $"Straight ({rank} high)"
                | ThreeOfAKind rank -> $"Three of a Kind ({rank}s)"
                | TwoPair (r1, r2) -> $"Two Pair ({r1}'s and {r2}'s)"
                | Pair rank -> $"Pair of {rank}'s"
                | HighCard rank -> $"High Card ({rank})"
            $"{hand} is a {desc}"

        // Create a random poker hand of unique cards
        let rec randomHand () =
            let hand = create (Card.randomCard, Card.randomCard, Card.randomCard, Card.randomCard, Card.randomCard)

            match hand with
            | Cards cards when cards |> List.distinct |> List.length < 5 -> randomHand ()
            | _ -> hand

open Cards
open Poker
PokerHand.create (
    { Rank = Queen; Suit = Hearts },
    { Rank = King; Suit = Hearts },
    { Rank = Ace; Suit = Hearts },
    { Rank = Ten; Suit = Hearts },
    { Rank = Jack; Suit = Hearts })
|> PokerHand.description

let random () = PokerHand.randomHand () |> PokerHand.description