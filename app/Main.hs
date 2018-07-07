{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import GHC.Generics (Generic)

-- store
import Data.Store
-- conduit-combinators
import Conduit hiding (connect, throwM)
import Data.Conduit.Network (sinkSocket, sourceSocket)
-- array
import Data.Array.Base
import Data.Array.IArray
import Data.Array.Unboxed
-- pretty-show
import Text.Show.Pretty (ppShow)
-- lens
import Control.Lens (view, _2, _4, _3)
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB (hPut, ByteString, head, last)
import qualified Data.ByteString.Lazy.Char8 as LB (hPutStrLn, unpack, pack, split, readFile)
import Data.ByteString.Builder (Builder, toLazyByteString, byteStringHex, hPutBuilder)
-- deepseq
import Control.DeepSeq
-- network
import Network.Socket hiding (send, sendTo, recv, recvFrom, withSocketDo)
import Network.Socket.ByteString (send, sendTo, recv, recvFrom)
-- cereal
import Data.Serialize (decode, encode, decodeLazy, encodeLazy, Serialize, put, get, putWord32be)
import Data.Serialize.Put (runPut)
import Data.Serialize.Get (runGet)
-- base
import Numeric (showHex)
import Data.List (sort, nub, intercalate)
import Data.Word (Word8, Word16, Word32)
import Control.Concurrent (ThreadId, MVar, takeMVar, putMVar, tryPutMVar, modifyMVar_, swapMVar, readMVar, newMVar, withMVar, forkIO, forkOS, threadDelay, newEmptyMVar, killThread)
import qualified Control.Concurrent.MVar.Strict as St (MVar, swapMVar, readMVar, newMVar, withMVar, newEmptyMVar, tryTakeMVar, putMVar, modifyMVar_, modifyMVar)
import Control.Monad (guard, forever, replicateM, replicateM_, liftM, liftM2, when, unless, forM, forM_, void)
import Data.Fixed (Milli)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getExecutablePath, getProgName)
import Control.Exception (evaluate)
-- time
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock (diffUTCTime, getCurrentTime, UTCTime(..), NominalDiffTime)
import Data.Time.Calendar (Day, addGregorianMonthsClip)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (utcToLocalTime, localDay)
import System.IO
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, listDirectory, makeAbsolute, removeFile)
-- safe-exception
import Control.Exception.Safe
-- hashable
import Data.Hashable (hash)

data MyData = MyData
    { myTime :: NominalDiffTime
    , myUTCTime :: UTCTime
    } deriving (Generic, Store)

main :: IO ()
main = do
    vStr <- newMVar (B.pack [1,2,3,4]) :: IO (MVar B.ByteString)
    initServer
    initClient vStr
    forkIO $ forever $ forM_ [1..] $ \ n -> do
        bStr <- readMVar vStr
        putStrLn $ show n
        putStrLn $ show $ B.last bStr
        wait 1
    wait 10

initServer :: IO ThreadId
initServer = forkIO $ bracket (openSockTCPServer "60123") close procRequests
 where procRequests :: Socket -> IO ()
       procRequests mastersock = do
           (connsock, clientaddr) <- accept mastersock
           thID <- forkIO $ procMessages connsock clientaddr
           wait 3
           killThread thID
           
       procMessages :: Socket -> SockAddr -> IO ()
       procMessages connsock clientaddr = do
           bStr <- B.readFile "C:/Users/nao/Desktop/2018-6-25-Packet Log.7z"
           n <- send connsock bStr
           putStrLn $ show n ++ " byte sent"
           wait 1
--            runConduitRes $ sourceLazy bStr .| sinkSocket connsock
           close connsock

initClient :: MVar B.ByteString -> IO ThreadId
initClient vStr = forkIO $ forever $ handle errorHandler $ withSocket "localhost" "60123" mainLoopClient
 where mainLoopClient sock addr = do
           connect sock addr
           bStr <- recv sock (2 ^ 30)
--            bStr <- runConduitRes $ sourceSocket sock .| sinkLazy
           putStrLn $ "modifyMVar"
           modifyMVarPure_ vStr (B.append bStr)
--            case decodeLazy msg :: Either String Int of
--                Right n -> do
-- --                    str' <- readMVar vStr
--                    putStrLn $ show "appended"
--                Left err  -> putStrLn $ "initClient decode error: " ++ show err
       errorHandler :: SomeException -> IO ()
       errorHandler err = putStrLn $ "initClient exception: " ++ show err

openSockTCPServer
    :: String -- ^ Port number or name; 514 is default
    -> IO Socket
openSockTCPServer port = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 5
    return sock

withSocket 
    :: HostName -- ^ IP Address of the target server to communicate
    -> ServiceName -- ^ Port number of the target server to communicate
    -> (Socket -> SockAddr -> IO a)
    -> IO a
withSocket hostname port io = bracket initializer (close . fst) (uncurry io)
 where initializer = do
           addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
           let serveraddr = head addrinfos
           sock <- socket (addrFamily serveraddr) Stream defaultProtocol
           return (sock, addrAddress serveraddr)

modifyMVarPure_ :: NFData a => MVar a -> (a -> a) -> IO ()
modifyMVarPure_ var f = St.modifyMVar_  var $ return . f

-- main :: IO ()
-- main = do
--     path <- getExecutablePath
--     bstr <- B.readFile path
--     putStrLn $ showHex (fromIntegral $ hash bstr :: Word32) ""

--     mainReceiver

sendInterval :: Milli
sendInterval = 0.5

message :: ByteString
message = B.pack $ take 1000 $ cycle [1..255]

senderWaitTime :: Milli
senderWaitTime = 10

receiverWaitTime :: Milli
receiverWaitTime = 10

allPorts :: [String]
allPorts = nub $ scRcvPorts -- senderPorts ++ receiverPorts

-- firmware ver. v3.4   65%
-- firmware ver. v3.4.6 

-- scRcvPorts | ocRcvPorts | otherPorts | scSndPorts | ocSndPorts | unknownPorts | total
-- packet per sec
-- 486.0      + 90.0       + 84.0       + 130        + 39.9       + 52.3         = 882.2 [packet/s]
-- bps
-- 3329980.8  + 871200.0   + 629497     + 99593.6    + 91291.2    + 228614.4     = 5250177.0 [bit/s] -- 現状
-- 980675.2   + 871200.0   + 629497     + 99593.6    + 91291.2                   = 2672257.0 [bit/s] -- 絶対必用なやつ

senderPorts :: [String]
senderPorts = nub $ scSndPorts ++ ocSndPorts

receiverPorts :: [String]
receiverPorts = nub $ scRcvPorts ++ ocRcvPorts ++ otherPorts

scRcvPorts :: [String]
scRcvPorts = do
--     tag <- TagSC2SCcommunicationP2P : [TagStatusInfo .. TagLostTrainInfo]
    tag <- [TagSRSReception .. TagLostTrainInfo]
    sc <- [SC801 ..]
    return $ makePortNum tag $ Right sc

scSndPorts :: [String]
scSndPorts = do
--     tag <- TagSC2SCcommunicationP2P : [TagStatusInfo .. TagLostTrainInfo]
    tag <- [TagTSRCommand .. TagMasterClock]
    sc <- [SC801 ..]
    return $ makePortNum tag $ Right sc

ocRcvPorts :: [String]
ocRcvPorts = do
    tag <- [TagOC2ATS1 .. TagOC2ATS3]
    oc <- [OC801 ..]
    return $ makePortNum tag $ Left oc

ocSndPorts :: [String]
ocSndPorts = do
    tag <- [TagATS2OC]
    oc <- [OC801 ..]
    return $ makePortNum tag $ Left oc

otherPorts :: [String]
otherPorts =
    [ synchStateServerPortNumber
    , updateRequestCommandPortNumber
    , msgServerStatusPortNumber
    , msgWorkstationStatusPortNumber
    , msgWorkstationCommandPortNumber
    , onlineTimetableServerPortNumber
    , msgTrainStatusPortNumber
    , msgPSDStatusPortNumber
    , msgTODStatusPortNumber
    , msgOTTStatusPortNumber
    , headwayStatusServerPortNumber
    , portNumberPASPIDS
    , portNumberTrainRadio
    , portNumberTVS
    , portNumberSCADA
    , portNumberCCTV_TrainInfo
    , portNumberCCTV_PEA
    , portNumberPSD2ATS
    , portNumberATS2PSD
    , portNumberUPS
    , portNumberTOD
    , portNumberImMaster
    , portNumberAccountServer
    , portNumberASPMHeartBeat
    ]

mainSender :: IO ()
mainSender = do
    vStdout <- newMVar stdout
    senderLog <- newMVar []
    mapM_ (initSender vStdout senderLog) senderPorts
    wait senderWaitTime
    log <- readMVar senderLog
    let numOfByteSent = sum $ map snd log
    putStrLn $ ppShow log
    putStrLn $ concat ["Byte Sent: ", show $ fromIntegral numOfByteSent / 2]
    putStrLn $ concat ["Byte per second: ", show $ fromIntegral numOfByteSent / realToFrac senderWaitTime]

mainReceiver :: IO ()
mainReceiver = do
    vStdout <- newMVar stdout
    receiverLog <- newMVar []
    threadIds <- mapM (initReceiver vStdout receiverLog) allPorts
    wait receiverWaitTime
    log <- readMVar receiverLog
    let numOfByteReceived = sum $ map (view _2) log
--     putStrLn $ ppShow log
    putStrLn $ concat ["Byte Received: ", show $ fromIntegral numOfByteReceived]
    putStrLn $ concat ["Bits per second: ", show $ 8 * fromIntegral numOfByteReceived / realToFrac receiverWaitTime]
    let numOfSCByteReceived = sum $ map (view _2) $ filter ((`elem` scRcvPorts) . (view _4)) $ log
    putStrLn $ concat ["SC Byte Received: ", show $ fromIntegral numOfSCByteReceived]
    putStrLn $ concat ["Bits per second: ", show $ 8 * fromIntegral numOfSCByteReceived / realToFrac receiverWaitTime]
    let numOfOCByteReceived = sum $ map (view _2) $ filter ((`elem` ocRcvPorts) . (view _4)) $ log
    putStrLn $ concat ["OC Byte Received: ", show $ fromIntegral numOfOCByteReceived]
    putStrLn $ concat ["Bits per second: ", show $ 8 * fromIntegral numOfOCByteReceived / realToFrac receiverWaitTime]
    let numOfOtherByteReceived = sum $ map (view _2) $ filter ((`elem` otherPorts) . (view _4)) $ log
    putStrLn $ concat ["Other Byte Received: ", show $ fromIntegral numOfOtherByteReceived]
    putStrLn $ concat ["Bits per second: ", show $ 8 * fromIntegral numOfOtherByteReceived / realToFrac receiverWaitTime]
    let numOfPacketReceived = length log
    putStrLn $ concat ["numOfPacketReceived: ", show $ fromIntegral numOfPacketReceived]
    putStrLn $ concat ["numOfPacket per second: ", show $ fromIntegral numOfPacketReceived / realToFrac receiverWaitTime]

--    mapM_ putStrLn $ sort $ nub $ map (\ (_,_, addr, port) -> port) log

-- (time, B.length msg, show addr, portNumber)
--     mapM_ killThread threadIds

initSender
    :: MVar Handle -- ^ lock for stdout
    -> MVar [(UTCTime, Int)]
    -> String
    -> IO ThreadId
initSender vStdout senderLog portNumber = forkIO $ bracket initializer finalizer $ addTimer sendInterval . mainLoopSenderWorkstationStatus
 where initializer :: IO ((Socket, SockAddr), (Socket, SockAddr))
       initializer = do
           sockInfo1 <- openSockSender True "172.21.255.255" portNumber
           sockInfo2 <- openSockSender True "172.22.255.255" portNumber
           return $ (sockInfo1, sockInfo2)
       finalizer :: ((Socket, SockAddr), (Socket, SockAddr)) -> IO ()
       finalizer ((sock1, addr1), (sock2, addr2)) = do
           close sock1
           close sock2
       mainLoopSenderWorkstationStatus :: ((Socket, SockAddr), (Socket, SockAddr)) -> IO ()
       mainLoopSenderWorkstationStatus ((sock1, addr1), (sock2, addr2)) = do
           n1 <- sendTo sock1 message addr1
           time1 <- getCurrentTime
           n2 <- sendTo sock2 message addr2
           time2 <- getCurrentTime
           modifyMVarPure_ senderLog ((time1, n1) :)
           modifyMVarPure_ senderLog ((time2, n2) :)
           return ()

initReceiver
    :: MVar Handle
    -> MVar [(UTCTime, Int, String, String)]
    -> String
    -> IO ThreadId
initReceiver vStdout receiverLog portNumber = do
    let mainLoopReceiverWSStatus :: Socket -> IO ()
        mainLoopReceiverWSStatus sock = forever $ do
            (msg, addr) <- recvFrom sock 1472 -- ethernetのフレーム長
            time <- getCurrentTime
            modifyMVarPure_ receiverLog ((time, B.length msg, show addr, portNumber) :)
    forkIO $ bracket (openSockUDPServer portNumber) close mainLoopReceiverWSStatus

openSockSender
     :: Bool -- ^ isBroadCast
     -> String -- ^ IP Address
     -> String -- ^ Port Number
     -> IO (Socket, SockAddr)
openSockSender isBroadCast addr port = do
     addrinfos <- getAddrInfo Nothing (Just addr) (Just port)
     let receiverAddr = head addrinfos
     sock <- socket AF_INET Datagram 0
     when isBroadCast $ setSocketOption sock Broadcast 1
     setSocketOption sock SendBuffer 1472
     return (sock, addrAddress receiverAddr)

openSockUDPServer
    :: String -- ^ Port number or name; 514 is default
    -> IO Socket
openSockUDPServer port = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr)
    return sock

addTimer :: Milli -> IO () -> IO ()
addTimer delay io = forever $ do
    forkIO $ io `catch` errorHandler
    wait delay
 where errorHandler :: SomeException -> IO ()
       errorHandler err = putStrLn $ show err

wait
    :: Milli -- ^ seconds
    -> IO ()
wait s = threadDelay $ truncate $ s * 1000000

--
synchStateServerPortNumber :: String
synchStateServerPortNumber = "60011"
updateRequestCommandPortNumber :: String
updateRequestCommandPortNumber = "60010"
msgServerStatusPortNumber :: String
msgServerStatusPortNumber       = "60001"
msgWorkstationStatusPortNumber :: String
msgWorkstationStatusPortNumber  = "60002"
msgWorkstationCommandPortNumber :: String
msgWorkstationCommandPortNumber = "60003"
onlineTimetableServerPortNumber :: String
onlineTimetableServerPortNumber = "60004"
msgTrainStatusPortNumber :: String
msgTrainStatusPortNumber        = "60005"
msgPSDStatusPortNumber :: String
msgPSDStatusPortNumber          = "60006"
msgTODStatusPortNumber :: String
msgTODStatusPortNumber          = "60007"
msgOTTStatusPortNumber :: String
msgOTTStatusPortNumber          = "60008"
headwayStatusServerPortNumber :: String
headwayStatusServerPortNumber   = "60009"

portNumberPASPIDS        :: String
portNumberPASPIDS               = "51101"
portNumberTrainRadio     :: String
portNumberTrainRadio            = "51141"
portNumberTVS            :: String
portNumberTVS                   = "502"
portNumberSCADA          :: String
portNumberSCADA                 = "2404"
portNumberCCTV_TrainInfo :: String
portNumberCCTV_TrainInfo        = "52100"
portNumberCCTV_PEA       :: String
portNumberCCTV_PEA              = "52200"
portNumberPSD2ATS        :: String
portNumberPSD2ATS               = "2201"
portNumberATS2PSD        :: String
portNumberATS2PSD               = "2202"
portNumberUPS            :: String
portNumberUPS                   = "502"
portNumberTOD            :: String
portNumberTOD                   = "52011"
portNumberImMaster       :: String
portNumberImMaster              = "52012"
portNumberAccountServer  :: String
portNumberAccountServer         = "60100"
portNumberASPMHeartBeat  :: String
portNumberASPMHeartBeat         = "60101"

makePortNum :: TagIF -> Either OC_ID SC_ID -> String
makePortNum tag equip = case equip of
        Left oc -> f 58000 oc
        Right sc -> f 55000 sc
 where f offset equip =  show $ offset + (fromEnum equip + 1) * 100 + fromIntegral (arrTagToMsgType ! tag)

arrTagToMsgType :: UArray TagIF Word8
arrTagToMsgType = accumArray (flip const) 0 (minBound, maxBound) $ 
    [ (TagSRSReception, 1)
    , (TagSRSSend, 2)
    , (TagTrainInformation, 3)
    , (Tag100MLANInput, 4)
    , (Tag100MLANOutput, 5)
    , (TagSC2SCcommunicationP2P, 6)
    , (TagMonitorinformation1, 7)
    , (TagMonitorInformation5, 11)
    , (TagMonitorInformation6, 12)
    , (TagMonitorInformation7, 13)
    , (TagMonitorInformation8, 14)
    , (TagMonitorInformation9, 15)
    , (TagRSOnlineRewrite1, 16)
    , (TagRSOnlineRewrite2, 17)
    , (TagRSOnlineRewrite3, 18)
    , (TagRSOnlineRewrite4, 19)
    , (TagRSOnlineRewrite5, 20)
    , (TagRSOnlineRewrit6, 21)
    , (TagRSOnlineRewrite7, 22)
    , (TagAliveInformation, 23)

    , (TagTSRCommand, 41)
    , (TagNoEntryCommand, 42)
    , (TagTrainCommand, 43)
    , (TagStationCommand, 44)
    , (TagZoneCommand, 45)
    , (TagMasterClock, 46)

    , (TagStatusInfo, 31)
    , (TagZoneInfo, 32)
    , (TagTSRInfo, 33)
    , (TagNoEntryInfo, 34)
    , (TagTrainInfo, 35)
    , (TagStationInfo, 36)
    , (TagTrackInfo, 37)
    , (TagLostTrainInfo, 39)
    
    , (TagATS2OC, 99)
    , (TagOC2ATS1, 98)
    , (TagOC2ATS2, 97)
    , (TagOC2ATS3, 96)
    ]

data OC_ID
    = OC801 -- BCGN
    | OC802 -- JLA
    | OC803 -- IWNR
    | OC804 -- RKPM
    | OC805 -- IGDA
    | OC806 -- JPW
    | OC807 -- JSTB
    | OC808 -- KIKD
        deriving (Show, Ord, Eq, Enum, Bounded, Ix, Generic)

instance NFData OC_ID
instance Serialize OC_ID

data SC_ID
    = SC801 -- BCGN
    | SC802 -- JLA
    | SC803 -- IWNR
    | SC804 -- KJMD
    | SC805 -- PSPK
    | SC806 -- RKPM
    | SC807 -- SKVR
    | SC808 -- PALM
    | SC809 -- JPW
    | SC810 -- KIKD
    | SC811 -- JSTB
        deriving (Show, Ord, Eq, Enum, Bounded, Ix, Generic, Read)

instance NFData SC_ID

data TagIF
    = TagTSRCommand
    | TagNoEntryCommand
    | TagTrainCommand
    | TagStationCommand
    | TagZoneCommand
    | TagMasterClock

    | TagSRSReception
    | TagSRSSend
    | TagTrainInformation
    | Tag100MLANInput
    | Tag100MLANOutput
    | TagSC2SCcommunicationP2P
    | TagMonitorinformation1

    | TagMonitorInformation5
    | TagMonitorInformation6
    | TagMonitorInformation7
    | TagMonitorInformation8
    | TagMonitorInformation9
    | TagRSOnlineRewrite1
    | TagRSOnlineRewrite2
    | TagRSOnlineRewrite3
    | TagRSOnlineRewrite4
    | TagRSOnlineRewrite5
    | TagRSOnlineRewrit6
    | TagRSOnlineRewrite7
    | TagAliveInformation
    
    | TagStatusInfo
    | TagZoneInfo
    | TagTSRInfo
    | TagNoEntryInfo
    | TagTrainInfo
    | TagStationInfo
    | TagTrackInfo
    | TagLostTrainInfo
    
    | TagATS2OC
    | TagOC2ATS1
    | TagOC2ATS2
    | TagOC2ATS3
    
    | TagWSStatus
    | TagSRVStatus
        deriving (Show, Read, Ord, Eq, Enum, Bounded, Ix)

getUnknownPortNums :: String -> [String]
getUnknownPortNums str = nub $ filter (`notElem` allPorts) $ map f $ lines str
 where f :: String -> String
       f = reverse . takeWhile isDigit . drop 1 . dropWhile (/= ':') . reverse

unknownPorts :: [String]
unknownPorts =
    [ "55538"
    , "55510"
--     , "360933287"
--     , ""
    , "55638"
    , "55724"
    , "55610"
--     , "112"
    , "55924"
    , "55838"
    , "55810"
    , "55524"
    , "55938"
    , "55624"
    , "55738"
    , "55910"
    , "55710"
    , "55824"
--     , "23"
--     , "10"
--     , "94"
--     , "57"
--     , "6"
--     , "08"
--     , "7"
    ]

tailShow :: IO ()
tailShow = do
    h <- openFile "C:/Users/nao/network-analysis/nohup.out" ReadMode
    txt <- T.hGetContents h 
    let txt' = T.lines txt
    outEach100000 txt' 0
    hClose h

outEach100000 :: [Text] -> Int -> IO ()
outEach100000 [] _ = return ()
outEach100000 txts n = do
    let (ls, rs) = splitAt 100000 txts
    T.writeFile ("C:/Users/nao/network-analysis/temporary/" ++ show n ++ ".txt") $ T.unlines ls
    outEach100000 rs (n + 1)

isolationCommand :: String
isolationCommand = unlines [intercalate "\t" $ concatMap (\ x -> ["Door " ++ show x ++ " Set", "Door " ++ show x ++ " Clear"]) [i .. i + 3] | i <- [1, 5 .. 45]]