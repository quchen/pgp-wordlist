{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}

module Main where



import           Data.Text.PgpWordlist.Internal.Convert

import qualified Data.ByteString.Lazy                   as BSL
import           Test.Tasty
import qualified Test.Tasty.HUnit                       as HU
import qualified Test.Tasty.QuickCheck                  as QC
import           Text.Printf
import           Data.Word



main :: IO ()
main = defaultMain tree



tree :: TestTree
tree = testGroup "Tests"
    [ allBytesConvertible
    , examplesBackAndForth
    , randomRoundtrips
    ]



allBytesConvertible :: TestTree
allBytesConvertible = testGroup "All bytes have a corresponding word" tests
  where
    tests = splice (map evenTest allWord8) (map oddTest allWord8)
    allWord8 = [minBound ..] :: [Word8]
    evenTest = testExists "%02x is convertible to an even word" toEvenWord
    oddTest  = testExists "%02x is convertible to an odd word"  toOddWord
    testExists msg f i = HU.testCase (printf msg i) (exists (f i))
    exists x =  HU.assertBool "" (x `seq` True)

    -- Combine two lists alternatingly
    -- splice [1,3,5] [2,4,6,8,10] ==> [1,2,3,4,5,6,7,10]
    splice []     ys = ys
    splice (x:xs) ys = x : splice ys xs



examplesBackAndForth :: TestTree
examplesBackAndForth = testGroup "Conversion beween various examples" tests
  where
    tests = concat
        [ [ HU.testCase (printf "#%2d: Bytes to PGP words" i)
                        (HU.assertEqual "" (toText bytes) pgpWords)
          , HU.testCase (printf "#%2d: PGP words to bytes" i)
                        (HU.assertEqual "" (Right bytes) (fromText pgpWords))
          ]
        | (bytes, pgpWords) <- testCases
        | i <- [0..] :: [Int]
        ]

    testCases = map (\(x,y) -> (BSL.pack x, y))
        [ ( [0xce, 0xe7, 0x1c, 0xbf, 0xe6, 0x59, 0x49, 0x48]
          , "spyglass truncated befriend rebellion tracker examine deckhand dictator"
          )
        , ( [0x06, 0x7a, 0xd2, 0xfc, 0xb7, 0x74, 0x48, 0x18]
          , "afflict infancy standard wilmington seabird hydraulic deadbolt borderline"
          )
        , ( [0xb3, 0xc6, 0x75, 0x64, 0xa7, 0x9e, 0x0a, 0xbb]
          , "scallion responsive indulge getaway repay onlooker allow publisher"
          )
        , ( [0x84, 0x98, 0xd3, 0x33, 0xf1, 0xe4, 0x2a, 0x92]
          , "mural narrative stapler concurrent unwind tradition brickyard misnomer"
          )
        , ( [0xb2, 0x8d, 0x47, 0xf8, 0x8c, 0x57, 0xb5, 0x09]
          , "sawdust microscope dashboard warranty offload eskimo scorecard applicant"
          )
        , ( [0x2e, 0x26, 0xf1, 0x24, 0x65, 0xb9, 0xae, 0xd7]
          , "buzzard caretaker unwind capricorn fracture proximate robust stethoscope"
          )
        , ( [0x72, 0x19, 0x07, 0xf2, 0xca, 0x6f, 0xe4, 0xee]
          , "highchair bottomless ahead vagabond spellbind hemisphere tonic universe"
          )
        , ( [0x52, 0x8f, 0x47, 0x3a, 0xbd, 0xf3, 0x64, 0x5b]
          , "dupont midsummer dashboard corrosion skullcap vertigo flytrap exodus"
          )
        , ( [0xe9, 0x75, 0x73, 0x8c, 0x03, 0xc1, 0x49, 0x7f]
          , "treadmill impartial hockey megaton acme recover deckhand integrate"
          )
        , ( [0xfd, 0xce, 0xd3, 0x8f, 0xfa, 0x3d, 0xd8, 0xf2]
          , "willow sardonic stapler midsummer wallet crucifix stormy vagabond"
          )
        , ( [0x2d, 0x56, 0x8c, 0x11, 0x95, 0x47, 0x4f, 0x6f]
          , "button escapade offload babylon preclude determine dropper hemisphere"
          )
        , ( [0xe4, 0x05, 0x46, 0xfe, 0xc9, 0xf4, 0x41, 0x9a]
          , "tonic almighty cubic yesteryear spearhead virginia cranky newsletter"
          )
        , ( [0xb2, 0x68, 0xfc, 0xe4, 0x2c, 0xf5, 0xe5, 0x04]
          , "sawdust gravity wayside tradition burbank visitor topmost alkali"
          )
        , ( [0xb4, 0xfb, 0xec, 0xe3, 0xc8, 0xe4, 0x29, 0xa5]
          , "scenic wichita tumor torpedo spaniel tradition breakup paperweight"
          )
        , ( [0xbb, 0x96, 0x68, 0xc6, 0x33, 0x11, 0x4f, 0x7c]
          , "shamrock monument frighten responsive chisel babylon dropper informant"
          )
        , ( [0x51, 0x5e, 0x7f, 0xe3, 0x1b, 0xc0, 0x6f, 0xc7]
          , "drunken finicky lockup torpedo beeswax recipe gremlin retraction"
          )
        ]

randomRoundtrips :: TestTree
randomRoundtrips = makeGroup tests
  where
    makeGroup = localOption (QC.QuickCheckMaxSize 1024)
              . testGroup "Random roundtrips"
    tests = [ QC.testProperty "Bytes -> PGP words -> Bytes" $
                \bytes -> let bs = BSL.pack bytes
                          in  length bytes > 10
                              QC.==>
                              fromText (toText bs) == Right bs
            ]
