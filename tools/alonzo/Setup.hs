#! /usr/bin/env runhaskell

import Distribution.Simple -- (
--    defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.LocalBuildInfo -- (
--    LocalBuildInfo(..) )
import Distribution.Simple.PreProcess -- (
--    PreProcessor(..), PPSuffixHandler(..) )
import Distribution.Simple.Program -- (
    -- Program(..), ConfiguredProgram(..),
    -- simpleProgram, findProgramVersion,
    -- runDbProgram )
import Distribution.Simple.Utils -- (
    -- FileGlob(..), parseFileGlob )
import Distribution.Verbosity

main :: IO ()
main =
    defaultMainWithHooks alonzoHooks

alonzoHooks :: UserHooks
alonzoHooks = simpleUserHooks {
    -- confHook = \a b -> do
    --     buildInfo <- confHook simpleUserHooks a b
    --     -- register bnfc as a configured program
    --     (bnfc, progDB) <- requireProgram normal bnfcProgram (withPrograms buildInfo)
    --     return buildInfo {
    --         withPrograms = progDB
    --     }
    hookedPreProcessors = [ppBNFC]
,   hookedPrograms = [bnfcProgram]
-- ,   buildHook =
--         -- run bnfc to obtain the Alex and Happy files
--         -- add a flag for ghc to find the generated files in the build tree
--         buildHook simpleUserHooks
}

-- program description for the configure step
bnfcProgram :: Program
bnfcProgram = (simpleProgram "bnfc") {
    programFindVersion = findProgramVersion "--version" id
,   programPostConf = \v prog ->
        return prog {
            programDefaultArgs = ["-haskell"]
        }
}

-- program pre-processor for the build step
ppBNFC :: PPSuffixHandler
ppBNFC =
    ( "cf"
    , \bi lbi -> PreProcessor {
            platformIndependent = True,
            runPreProcessor = mkSimplePreProcessor $ \inFile outFile v -> do
                info v ("converting " ++ inFile ++ "to" ++ outFile)
                let Just (FileGlob outDir _) = parseFileGlob outFile
                runDbProgram v bnfcProgram (withPrograms lbi) ["-o",outDir,inFile]
        }
    )
