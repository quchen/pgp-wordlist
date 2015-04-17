{-# LANGUAGE OverloadedStrings #-}

-- | Core functionality for conversion between binary formats and PGP word
--   lists.
module Data.Text.PgpWordlist.Internal.Tables where



import           Data.Text.PgpWordlist.Internal.AltList (AltList)
import qualified Data.Text.PgpWordlist.Internal.AltList as Alt

import           Data.Bimap                             (Bimap, (!))
import qualified Data.Bimap                             as BM

import qualified Data.ByteString.Lazy                   as BSL
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Word



-- | Abstract representation of a PGP word list.
newtype PgpWordlist = PgpWordlist (AltList EvenWord OddWord)
    deriving (Eq, Ord, Show)



-- | Possible translation errors from a list of PGP words to binary format.
data TranslationError =
      BadWord Text         -- ^ Word is not recognized
    | BadParity Text Word8 -- ^ Word is recognized, but from the wrong
                           --   alphabet. Duplicates, omissions, and neighbour
                           --   transpositions are often cause for this.
    deriving (Eq, Ord, Show)



-- | Inverse of 'fromText', modulo whitespace count.
toText :: BSL.ByteString -> Text
toText = T.intercalate " "
       . Alt.toList
       . Alt.bimap (unEvenWord . toEvenWord) (unOddWord . toOddWord)
       . Alt.fromList
       . BSL.unpack



-- | Convert a text of whitespace-separated words to their binary
--   representation. The whitespace splitting behaviour is given by 'T.words'.
fromText :: Text -> Either TranslationError BSL.ByteString
fromText = fmap (BSL.pack . Alt.toList)
         . Alt.bitraverse fromEvenWord fromOddWord
         . Alt.fromList
         . T.words



-- | Word that is supposed to occur in an even position
newtype EvenWord = EvenWord { unEvenWord :: Text }
    deriving (Eq, Ord, Show)

-- | Look up the word corresponding to a byte.
toEvenWord :: Word8 -> EvenWord
toEvenWord = (evenMap !) -- evenMap is total, so the lookup is safe

-- | Simple conversion, taking into account invalid words.
fromEvenWord :: Text -> Either TranslationError Word8
fromEvenWord word = case BM.lookupR (EvenWord word) evenMap of
    Just i  -> Right i
    Nothing -> Left (case BM.lookupR (OddWord word) oddMap of
        Just j  -> BadParity word j
        Nothing -> BadWord word)

-- | Mapping from and to 'EvenWord's
evenMap :: Bimap Word8 EvenWord
evenMap = BM.fromList (map pick12 wordList)
  where
    pick12 :: (a,b,c) -> (a,b)
    pick12 (i,e,_) = (i,e)



-- | Word that is supposed to occur in an odd position
newtype OddWord = OddWord { unOddWord :: Text }
    deriving (Eq, Ord, Show)

-- | Look up the word corresponding to a byte.
toOddWord :: Word8 -> OddWord
toOddWord = (oddMap !) -- oddMap is total, so the lookup is safe

-- | Simple conversion, taking into account invalid words.
fromOddWord :: Text -> Either TranslationError Word8
fromOddWord word = case BM.lookupR (OddWord word) oddMap of
    Just i  -> Right i
    Nothing -> Left (case BM.lookupR (EvenWord word) evenMap of
        Just j  -> BadParity word j
        Nothing -> BadWord word)

-- | Mapping from and to 'OddWord's
oddMap :: Bimap Word8 OddWord
oddMap = BM.fromList (map pick13 wordList)
  where
    pick13 :: (a,b,c) -> (a,c)
    pick13 (i,_,o) = (i,o)



wordList :: [(Word8, EvenWord, OddWord)]
wordList = map (\(i,e,o) -> (i, EvenWord e, OddWord o)) table
  where
    table = [ (0x00, "aardvark" , "adroitness" )
            , (0x01, "absurd"   , "adviser"    )
            , (0x02, "accrue"   , "aftermath"  )
            , (0x03, "acme"     , "aggregate"  )
            , (0x04, "adrift"   , "alkali"     )
            , (0x05, "adult"    , "almighty"   )
            , (0x06, "afflict"  , "amulet"     )
            , (0x07, "ahead"    , "amusement"  )
            , (0x08, "aimless"  , "antenna"    )
            , (0x09, "algol"    , "applicant"  )
            , (0x0a, "allow"    , "apollo"     )
            , (0x0b, "alone"    , "armistice"  )
            , (0x0c, "ammo"     , "article"    )
            , (0x0d, "ancient"  , "asteroid"   )
            , (0x0e, "apple"    , "atlantic"   )
            , (0x0f, "artist"   , "atmosphere" )
            , (0x10, "assume"   , "autopsy"    )
            , (0x11, "athens"   , "babylon"    )
            , (0x12, "atlas"    , "backwater"  )
            , (0x13, "aztec"    , "barbecue"   )
            , (0x14, "baboon"   , "belowground")
            , (0x15, "backfield", "bifocals"   )
            , (0x16, "backward" , "bodyguard"  )
            , (0x17, "banjo"    , "bookseller" )
            , (0x18, "beaming"  , "borderline" )
            , (0x19, "bedlamp"  , "bottomless" )
            , (0x1a, "beehive"  , "bradbury"   )
            , (0x1b, "beeswax"  , "bravado"    )
            , (0x1c, "befriend" , "brazilian"  )
            , (0x1d, "belfast"  , "breakaway"  )
            , (0x1e, "berserk"  , "burlington" )
            , (0x1f, "billiard" , "businessman")
            , (0x20, "bison"    , "butterfat"  )
            , (0x21, "blackjack", "camelot"    )
            , (0x22, "blockade" , "candidate"  )
            , (0x23, "blowtorch", "cannonball" )
            , (0x24, "bluebird" , "capricorn"  )
            , (0x25, "bombast"  , "caravan"    )
            , (0x26, "bookshelf", "caretaker"  )
            , (0x27, "brackish" , "celebrate"  )
            , (0x28, "breadline", "cellulose"  )
            , (0x29, "breakup"  , "certify"    )
            , (0x2a, "brickyard", "chambermaid")
            , (0x2b, "briefcase", "cherokee"   )
            , (0x2c, "burbank"  , "chicago"    )
            , (0x2d, "button"   , "clergyman"  )
            , (0x2e, "buzzard"  , "coherence"  )
            , (0x2f, "cement"   , "combustion" )
            , (0x30, "chairlift", "commando"   )
            , (0x31, "chatter"  , "company"    )
            , (0x32, "checkup"  , "component"  )
            , (0x33, "chisel"   , "concurrent" )
            , (0x34, "choking"  , "confidence" )
            , (0x35, "chopper"  , "conformist" )
            , (0x36, "christmas", "congregate" )
            , (0x37, "clamshell", "consensus"  )
            , (0x38, "classic"  , "consulting" )
            , (0x39, "classroom", "corporate"  )
            , (0x3a, "cleanup"  , "corrosion"  )
            , (0x3b, "clockwork", "councilman" )
            , (0x3c, "cobra"    , "crossover"  )
            , (0x3d, "commence" , "crucifix"   )
            , (0x3e, "concert"  , "cumbersome" )
            , (0x3f, "cowbell"  , "customer"   )
            , (0x40, "crackdown", "dakota"     )
            , (0x41, "cranky"   , "decadence"  )
            , (0x42, "crowfoot" , "december"   )
            , (0x43, "crucial"  , "decimal"    )
            , (0x44, "crumpled" , "designing"  )
            , (0x45, "crusade"  , "detector"   )
            , (0x46, "cubic"    , "detergent"  )
            , (0x47, "dashboard", "determine"  )
            , (0x48, "deadbolt" , "dictator"   )
            , (0x49, "deckhand" , "dinosaur"   )
            , (0x4a, "dogsled"  , "direction"  )
            , (0x4b, "dragnet"  , "disable"    )
            , (0x4c, "drainage" , "disbelief"  )
            , (0x4d, "dreadful" , "disruptive" )
            , (0x4e, "drifter"  , "distortion" )
            , (0x4f, "dropper"  , "document"   )
            , (0x50, "drumbeat" , "embezzle"   )
            , (0x51, "drunken"  , "enchanting" )
            , (0x52, "dupont"   , "enrollment" )
            , (0x53, "dwelling" , "enterprise" )
            , (0x54, "eating"   , "equation"   )
            , (0x55, "edict"    , "equipment"  )
            , (0x56, "egghead"  , "escapade"   )
            , (0x57, "eightball", "eskimo"     )
            , (0x58, "endorse"  , "everyday"   )
            , (0x59, "endow"    , "examine"    )
            , (0x5a, "enlist"   , "existence"  )
            , (0x5b, "erase"    , "exodus"     )
            , (0x5c, "escape"   , "fascinate"  )
            , (0x5d, "exceed"   , "filament"   )
            , (0x5e, "eyeglass" , "finicky"    )
            , (0x5f, "eyetooth" , "forever"    )
            , (0x60, "facial"   , "fortitude"  )
            , (0x61, "fallout"  , "frequency"  )
            , (0x62, "flagpole" , "gadgetry"   )
            , (0x63, "flatfoot" , "galveston"  )
            , (0x64, "flytrap"  , "getaway"    )
            , (0x65, "fracture" , "glossary"   )
            , (0x66, "framework", "gossamer"   )
            , (0x67, "freedom"  , "graduate"   )
            , (0x68, "frighten" , "gravity"    )
            , (0x69, "gazelle"  , "guitarist"  )
            , (0x6a, "geiger"   , "hamburger"  )
            , (0x6b, "glitter"  , "hamilton"   )
            , (0x6c, "glucose"  , "handiwork"  )
            , (0x6d, "goggles"  , "hazardous"  )
            , (0x6e, "goldfish" , "headwaters" )
            , (0x6f, "gremlin"  , "hemisphere" )
            , (0x70, "guidance" , "hesitate"   )
            , (0x71, "hamlet"   , "hideaway"   )
            , (0x72, "highchair", "holiness"   )
            , (0x73, "hockey"   , "hurricane"  )
            , (0x74, "indoors"  , "hydraulic"  )
            , (0x75, "indulge"  , "impartial"  )
            , (0x76, "inverse"  , "impetus"    )
            , (0x77, "involve"  , "inception"  )
            , (0x78, "island"   , "indigo"     )
            , (0x79, "jawbone"  , "inertia"    )
            , (0x7a, "keyboard" , "infancy"    )
            , (0x7b, "kickoff"  , "inferno"    )
            , (0x7c, "kiwi"     , "informant"  )
            , (0x7d, "klaxon"   , "insincere"  )
            , (0x7e, "locale"   , "insurgent"  )
            , (0x7f, "lockup"   , "integrate"  )
            , (0x80, "merit"    , "intention"  )
            , (0x81, "minnow"   , "inventive"  )
            , (0x82, "miser"    , "istanbul"   )
            , (0x83, "mohawk"   , "jamaica"    )
            , (0x84, "mural"    , "jupiter"    )
            , (0x85, "music"    , "leprosy"    )
            , (0x86, "necklace" , "letterhead" )
            , (0x87, "neptune"  , "liberty"    )
            , (0x88, "newborn"  , "maritime"   )
            , (0x89, "nightbird", "matchmaker" )
            , (0x8a, "oakland"  , "maverick"   )
            , (0x8b, "obtuse"   , "medusa"     )
            , (0x8c, "offload"  , "megaton"    )
            , (0x8d, "optic"    , "microscope" )
            , (0x8e, "orca"     , "microwave"  )
            , (0x8f, "payday"   , "midsummer"  )
            , (0x90, "peachy"   , "millionaire")
            , (0x91, "pheasant" , "miracle"    )
            , (0x92, "physique" , "misnomer"   )
            , (0x93, "playhouse", "molasses"   )
            , (0x94, "pluto"    , "molecule"   )
            , (0x95, "preclude" , "montana"    )
            , (0x96, "prefer"   , "monument"   )
            , (0x97, "preshrunk", "mosquito"   )
            , (0x98, "printer"  , "narrative"  )
            , (0x99, "prowler"  , "nebula"     )
            , (0x9a, "pupil"    , "newsletter" )
            , (0x9b, "puppy"    , "norwegian"  )
            , (0x9c, "python"   , "october"    )
            , (0x9d, "quadrant" , "ohio"       )
            , (0x9e, "quiver"   , "onlooker"   )
            , (0x9f, "quota"    , "opulent"    )
            , (0xa0, "ragtime"  , "orlando"    )
            , (0xa1, "ratchet"  , "outfielder" )
            , (0xa2, "rebirth"  , "pacific"    )
            , (0xa3, "reform"   , "pandemic"   )
            , (0xa4, "regain"   , "pandora"    )
            , (0xa5, "reindeer" , "paperweight")
            , (0xa6, "rematch"  , "paragon"    )
            , (0xa7, "repay"    , "paragraph"  )
            , (0xa8, "retouch"  , "paramount"  )
            , (0xa9, "revenge"  , "passenger"  )
            , (0xaa, "reward"   , "pedigree"   )
            , (0xab, "rhythm"   , "pegasus"    )
            , (0xac, "ribcage"  , "penetrate"  )
            , (0xad, "ringbolt" , "perceptive" )
            , (0xae, "robust"   , "performance")
            , (0xaf, "rocker"   , "pharmacy"   )
            , (0xb0, "ruffled"  , "phonetic"   )
            , (0xb1, "sailboat" , "photograph" )
            , (0xb2, "sawdust"  , "pioneer"    )
            , (0xb3, "scallion" , "pocketful"  )
            , (0xb4, "scenic"   , "politeness" )
            , (0xb5, "scorecard", "positive"   )
            , (0xb6, "scotland" , "potato"     )
            , (0xb7, "seabird"  , "processor"  )
            , (0xb8, "select"   , "provincial" )
            , (0xb9, "sentence" , "proximate"  )
            , (0xba, "shadow"   , "puberty"    )
            , (0xbb, "shamrock" , "publisher"  )
            , (0xbc, "showgirl" , "pyramid"    )
            , (0xbd, "skullcap" , "quantity"   )
            , (0xbe, "skydive"  , "racketeer"  )
            , (0xbf, "slingshot", "rebellion"  )
            , (0xc0, "slowdown" , "recipe"     )
            , (0xc1, "snapline" , "recover"    )
            , (0xc2, "snapshot" , "repellent"  )
            , (0xc3, "snowcap"  , "replica"    )
            , (0xc4, "snowslide", "reproduce"  )
            , (0xc5, "solo"     , "resistor"   )
            , (0xc6, "southward", "responsive" )
            , (0xc7, "soybean"  , "retraction" )
            , (0xc8, "spaniel"  , "retrieval"  )
            , (0xc9, "spearhead", "retrospect" )
            , (0xca, "spellbind", "revenue"    )
            , (0xcb, "spheroid" , "revival"    )
            , (0xcc, "spigot"   , "revolver"   )
            , (0xcd, "spindle"  , "sandalwood" )
            , (0xce, "spyglass" , "sardonic"   )
            , (0xcf, "stagehand", "saturday"   )
            , (0xd0, "stagnate" , "savagery"   )
            , (0xd1, "stairway" , "scavenger"  )
            , (0xd2, "standard" , "sensation"  )
            , (0xd3, "stapler"  , "sociable"   )
            , (0xd4, "steamship", "souvenir"   )
            , (0xd5, "sterling" , "specialist" )
            , (0xd6, "stockman" , "speculate"  )
            , (0xd7, "stopwatch", "stethoscope")
            , (0xd8, "stormy"   , "stupendous" )
            , (0xd9, "sugar"    , "supportive" )
            , (0xda, "surmount" , "surrender"  )
            , (0xdb, "suspense" , "suspicious" )
            , (0xdc, "sweatband", "sympathy"   )
            , (0xdd, "swelter"  , "tambourine" )
            , (0xde, "tactics"  , "telephone"  )
            , (0xdf, "talon"    , "therapist"  )
            , (0xe0, "tapeworm" , "tobacco"    )
            , (0xe1, "tempest"  , "tolerance"  )
            , (0xe2, "tiger"    , "tomorrow"   )
            , (0xe3, "tissue"   , "torpedo"    )
            , (0xe4, "tonic"    , "tradition"  )
            , (0xe5, "topmost"  , "travesty"   )
            , (0xe6, "tracker"  , "trombonist" )
            , (0xe7, "transit"  , "truncated"  )
            , (0xe8, "trauma"   , "typewriter" )
            , (0xe9, "treadmill", "ultimate"   )
            , (0xea, "trojan"   , "undaunted"  )
            , (0xeb, "trouble"  , "underfoot"  )
            , (0xec, "tumor"    , "unicorn"    )
            , (0xed, "tunnel"   , "unify"      )
            , (0xee, "tycoon"   , "universe"   )
            , (0xef, "uncut"    , "unravel"    )
            , (0xf0, "unearth"  , "upcoming"   )
            , (0xf1, "unwind"   , "vacancy"    )
            , (0xf2, "uproot"   , "vagabond"   )
            , (0xf3, "upset"    , "vertigo"    )
            , (0xf4, "upshot"   , "virginia"   )
            , (0xf5, "vapor"    , "visitor"    )
            , (0xf6, "village"  , "vocalist"   )
            , (0xf7, "virus"    , "voyager"    )
            , (0xf8, "vulcan"   , "warranty"   )
            , (0xf9, "waffle"   , "waterloo"   )
            , (0xfa, "wallet"   , "whimsical"  )
            , (0xfb, "watchword", "wichita"    )
            , (0xfc, "wayside"  , "wilmington" )
            , (0xfd, "willow"   , "wyoming"    )
            , (0xfe, "woodlark" , "yesteryear" )
            , (0xff, "zulu"     , "yucatan"    )
            ]
