{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}

module Main where



import           Control.DeepSeq
import qualified Data.ByteString.Lazy                   as BSL
import           Test.Tasty
import qualified Test.Tasty.HUnit                       as HU
import qualified Test.Tasty.QuickCheck                  as QC
import           Text.Printf

import           Data.Text.PgpWordlist.Internal.Convert
import           Data.Text.PgpWordlist.Internal.Types



main :: IO ()
main = defaultMain tree



tree :: TestTree
tree = testGroup "Tests"
    [ allBytesConvertible
    , examplesBackAndForth
    , randomRoundtrips
    ]



allBytesConvertible :: TestTree
allBytesConvertible = HU.testCase description test
  where
    description = "All bytes have a corresponding even/odd word"
    assertNoBottoms x = x `deepseq` HU.assertBool "" True
    test = assertNoBottoms (do
        byte <- [minBound ..]
        convert <- [unEvenWord . toEvenWord, unOddWord . toOddWord]
        return $! convert byte )



examplesBackAndForth :: TestTree
examplesBackAndForth = testGroup "Explicit example conversions" tests
  where
    tests = concat
        [ [ HU.testCase (printf "#%d: Bytes to PGP words" i)
                        (HU.assertEqual "" (toText bytes) pgpWords)
          , HU.testCase (printf "#%d: PGP words to bytes" i)
                        (HU.assertEqual "" (Right bytes) (fromText pgpWords))
          ]
        | (bytes, pgpWords) <- testCases
        | i <- [0..] :: [Int]
        ]

    first f (a,b) = (f a, b) -- Bifunctor came into Base in 7.10 only

    testCases = map (first BSL.pack)
        [ ( [ 0xce, 0xe7, 0x1c, 0xbf, 0xe6, 0x59, 0x49, 0x48
            , 0x06, 0x7a, 0xd2, 0xfc, 0xb7, 0x74, 0x48, 0x18 ]
          , "spyglass truncated befriend rebellion tracker examine deckhand \
            \dictator afflict infancy standard wilmington seabird hydraulic \
            \deadbolt borderline"
          )
        , ( [ 0xb3, 0xc6, 0x75, 0x64, 0xa7, 0x9e, 0x0a, 0xbb
            , 0x84, 0x98, 0xd3, 0x33, 0xf1, 0xe4, 0x2a, 0x92 ]
          , "scallion responsive indulge getaway repay onlooker allow \
            \publisher mural narrative stapler concurrent unwind tradition \
            \brickyard misnomer"
          )
        , ( [ 0xb2, 0x8d, 0x47, 0xf8, 0x8c, 0x57, 0xb5, 0x09
            , 0x2e, 0x26, 0xf1, 0x24, 0x65, 0xb9, 0xae, 0xd7 ]
          , "sawdust microscope dashboard warranty offload eskimo scorecard \
            \applicant buzzard caretaker unwind capricorn fracture proximate \
            \robust stethoscope"
          )
        , ( [ 0x72, 0x19, 0x07, 0xf2, 0xca, 0x6f, 0xe4, 0xee
            , 0x52, 0x8f, 0x47, 0x3a, 0xbd, 0xf3, 0x64, 0x5b ]
          , "highchair bottomless ahead vagabond spellbind hemisphere tonic \
            \universe dupont midsummer dashboard corrosion skullcap vertigo \
            \flytrap exodus"
          )
        , ( [ 0xe9, 0x75, 0x73, 0x8c, 0x03, 0xc1, 0x49, 0x7f
            , 0xfd, 0xce, 0xd3, 0x8f, 0xfa, 0x3d, 0xd8, 0xf2 ]
          , "treadmill impartial hockey megaton acme recover deckhand \
            \integrate willow sardonic stapler midsummer wallet crucifix \
            \stormy vagabond"
          )
        , ( [ 0x2d, 0x56, 0x8c, 0x11, 0x95, 0x47, 0x4f, 0x6f
            , 0xe4, 0x05, 0x46, 0xfe, 0xc9, 0xf4, 0x41, 0x9a ]
          , "button escapade offload babylon preclude determine dropper \
            \hemisphere tonic almighty cubic yesteryear spearhead virginia \
            \cranky newsletter"
          )
        , ( [ 0xb2, 0x68, 0xfc, 0xe4, 0x2c, 0xf5, 0xe5, 0x04
            , 0xb4, 0xfb, 0xec, 0xe3, 0xc8, 0xe4, 0x29, 0xa5 ]
          , "sawdust gravity wayside tradition burbank visitor topmost alkali \
            \scenic wichita tumor torpedo spaniel tradition breakup paperweight"
          )
        , ( [ 0xbb, 0x96, 0x68, 0xc6, 0x33, 0x11, 0x4f, 0x7c
            , 0x51, 0x5e, 0x7f, 0xe3, 0x1b, 0xc0, 0x6f, 0xc7 ]
          , "shamrock monument frighten responsive chisel babylon dropper \
            \informant drunken finicky lockup torpedo beeswax recipe gremlin \
            \retraction"
          )
        ]

randomRoundtrips :: TestTree
randomRoundtrips = makeGroup tests
  where
    makeGroup = localOption (QC.QuickCheckMaxSize 1024)
              . localOption (QC.QuickCheckTests 1000)
              . testGroup "Random roundtrips"
    tests = [ QC.testProperty "Bytes -> PGP words -> Bytes" $
                \bytes -> let bs = BSL.pack bytes
                          in  length bytes > 10
                              QC.==>
                              fromText (toText bs) == Right bs
            ]
