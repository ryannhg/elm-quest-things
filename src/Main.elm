module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Random as R exposing (Generator)


type Quest
    = Slay SlayQuestOptions
    | Rescue RescueQuestOptions
    | Retrieve RetreiveQuestOptions



-- QUESTS


quests : Generator Quest
quests =
    R.uniform
        (R.map Slay slayQuests)
        [ R.map Rescue rescueQuests
        , R.map Retrieve retrieveQuests
        ]
        |> R.andThen identity


slayQuests : Generator SlayQuestOptions
slayQuests =
    enemies
        |> R.andThen
            (\enemy ->
                R.map3 SlayQuestOptions
                    (R.constant enemy)
                    locations
                    (reasons enemy)
            )


rescueQuests : Generator RescueQuestOptions
rescueQuests =
    R.map2 RescueQuestOptions
        allies
        locations


retrieveQuests : Generator RetreiveQuestOptions
retrieveQuests =
    R.map RetreiveQuestOptions items



-- SLAY


type alias SlayQuestOptions =
    { target : Enemy
    , location : Location
    , reason : Reason
    }


type Reason
    = Harming String Location
    | Kidnapped Ally



-- RESCUE


type alias RescueQuestOptions =
    { target : Ally
    , location : Location
    }



-- RETRIEVE


type alias RetreiveQuestOptions =
    { target : Item
    }


type Item
    = Amulet Power
    | Treasure
    | Scroll Power
    | Sword Power


type Power
    = StopTime
    | LifeAndDeath
    | RoastMarshmallows


items : Generator Item
items =
    powers
        |> R.andThen
            (\power ->
                R.uniform
                    (Amulet power)
                    [ Treasure
                    , Scroll power
                    , Sword power
                    ]
            )


itemToString : Item -> String
itemToString item =
    case item of
        Amulet power ->
            "Amulet of " ++ powerToString power

        Treasure ->
            "Treasure"

        Scroll power ->
            "Scroll of " ++ powerToString power

        Sword power ->
            "Sword of " ++ powerToString power


powers : Generator Power
powers =
    R.uniform
        StopTime
        [ LifeAndDeath
        , RoastMarshmallows
        ]


powerToString : Power -> String
powerToString power =
    case power of
        StopTime ->
            "Time"

        LifeAndDeath ->
            "Life and Death"

        RoastMarshmallows ->
            "Smores"



-- ENEMIES


type Enemy
    = Dragon Element
    | Necromancer DarkArt
    | Troll
    | Warlord
    | Bandit
    | Thief
    | Rogue



-- ELEMENTS


type Element
    = Frost
    | Fire
    | Poison


elementToString : Element -> String
elementToString element =
    Debug.toString element |> String.toLower


elements : Generator Element
elements =
    R.uniform
        Frost
        [ Fire
        , Poison
        ]



-- DARK ARTS


type DarkArt
    = Conjuring
    | Illusion
    | Destruction


darkArtToString : DarkArt -> String
darkArtToString darkArt =
    Debug.toString darkArt |> String.toLower


darkArts : Generator DarkArt
darkArts =
    R.uniform
        Conjuring
        [ Illusion
        , Destruction
        ]


enemyToString : Enemy -> String
enemyToString enemy =
    case enemy of
        Dragon element ->
            elementToString element ++ " dragon"

        Necromancer darkArt ->
            darkArtToString darkArt ++ " necromancer"

        Troll ->
            "troll"

        Warlord ->
            "warlord"

        Bandit ->
            "bandit"

        Thief ->
            "thief"

        Rogue ->
            "rogue"


enemies : Generator Enemy
enemies =
    R.uniform
        (R.map Dragon elements)
        [ R.map Necromancer darkArts
        , R.constant Troll
        , R.constant Warlord
        , R.constant Bandit
        , R.constant Thief
        , R.constant Rogue
        ]
        |> R.andThen identity


methodOfDestruction : Enemy -> Generator String
methodOfDestruction enemy =
    case enemy of
        Dragon _ ->
            R.uniform
                "burning down"
                [ "devouring people in"
                ]

        Necromancer darkArt ->
            case darkArt of
                Conjuring ->
                    R.uniform "summoning the dead in"
                        [ "resurrecting a demon in"
                        , "sacrificing the people in"
                        ]

                Illusion ->
                    R.uniform "enchanting people in" []

                Destruction ->
                    R.uniform "setting fire to" []

        Troll ->
            R.uniform
                "smashing"
                [ "devouring people in"
                ]

        Warlord ->
            R.uniform "taking over"
                []

        Bandit ->
            R.uniform "raiding"
                []

        Thief ->
            R.uniform "looting"
                []

        Rogue ->
            R.uniform "stabbing everyone in"
                []



-- ALLIES


type Ally
    = King
    | Knight
    | Wizard
    | Princess


allyToString : Ally -> String
allyToString ally =
    case ally of
        King ->
            "king"

        Knight ->
            "knight"

        Wizard ->
            "wizard"

        Princess ->
            "princess"


allies : Generator Ally
allies =
    R.uniform
        Knight
        [ Wizard
        , King
        , Princess
        ]



-- LOCATIONS


type Location
    = Forest
    | Cave
    | Volcano
    | Desert
    | Village
    | Swamps


locationToString : Location -> String
locationToString location =
    case location of
        Forest ->
            "forest"

        Cave ->
            "cave"

        Volcano ->
            "volcano"

        Desert ->
            "desert"

        Village ->
            "village"

        Swamps ->
            "swamps"


locations : Generator Location
locations =
    R.uniform
        Forest
        [ Cave
        , Volcano
        , Desert
        , Village
        , Swamps
        ]



-- REASONS


reasons : Enemy -> Generator Reason
reasons enemy =
    R.uniform
        (R.map Kidnapped allies)
        [ methodOfDestruction enemy
            |> R.andThen (\method -> R.map (Harming method) locations)
        ]
        |> R.andThen identity


reasonToString : Reason -> String
reasonToString reason =
    case reason of
        Kidnapped ally ->
            "They've kidnapped the " ++ allyToString ally ++ "."

        Harming method location ->
            "They're " ++ method ++ " the " ++ locationToString location ++ "!"


toSeed : String -> R.Seed
toSeed =
    String.toList
        >> List.map Char.toCode
        >> List.sum
        >> R.initialSeed


type alias Model =
    { seed : String
    }


type Msg
    = UpdateSeed String


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    Model "hooray"


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSeed seed ->
            { model | seed = seed }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [] [ text "Seed: " ]
            , input [ Events.onInput UpdateSeed, Attr.value model.seed ] []
            ]
        , model.seed
            |> toSeed
            |> R.step quests
            |> Tuple.first
            |> questToString
            |> text
            |> List.singleton
            |> p []
        ]


questToString : Quest -> String
questToString quest =
    case quest of
        Slay { target, location, reason } ->
            String.join ""
                [ "Defeat the "
                , enemyToString target
                , " in the "
                , locationToString location
                , ". "
                , reasonToString reason
                ]

        Rescue { target, location } ->
            String.join ""
                [ "Rescue the "
                , allyToString target
                , " in the "
                , locationToString location
                , "."
                ]

        Retrieve { target } ->
            "Retrieve the " ++ itemToString target ++ "!"
