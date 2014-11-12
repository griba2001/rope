{-# LANGUAGE PackageImports, RecordWildCards, NamedFieldPuns #-}
module TestDetailed (tests) where

import qualified Test.QuickCheck as Q
import Distribution.TestSuite as TS
import TestRope

toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {} = TS.Pass
toTSResult Q.GaveUp {} = TS.Fail "GaveUp"
toTSResult Q.Failure {reason} = TS.Fail reason

runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
        qres <- Q.verboseCheckWithResult Q.stdArgs {Q.maxSuccess = 30, Q.maxSize = 22} prop
        return $ (Finished . toTSResult) qres

tests :: IO [Test]
tests = return [ Test $ TestInstance (runQuickCheck propToRopeFromRopeIsId) "propToRopeFromRopeIsId" ["string"] [] undefined,
                 Test $ TestInstance (runQuickCheck propAppend) "propAppend" ["string"] [] undefined,
                 Test $ TestInstance (runQuickCheck propInsert) "propInsert" ["string"] [] undefined,
                 Test $ TestInstance (runQuickCheck propDelete) "propDelete" ["string"] [] undefined,
                 Test $ TestInstance (runQuickCheck propReport) "propReport" ["string"] [] undefined
                ]
