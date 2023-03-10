{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-|
Module      : SuperCollider.Message
Description : SuperCollider OSC Message patterns
Copyright   : (c) Mario Lang, 2022
License     : BSD3
Maintainer  : mlang@blind.guru
Stability   : experimental

The SuperCollider3 [Server-Command-Reference](https://doc.sccode.org/Reference/Server-Command-Reference.html)
implemented as a bidirectional pattern synonym library.
-}
module Sound.SuperCollider.Message (
  -- * Top-Level Commands
    pattern Quit, pattern QuitDone
  , pattern Notify, pattern NotifyWithID, pattern NotifyReply
  , pattern Status, pattern StatusReply
  , pattern Plugin
  , pattern DumpOSC
  , pattern Sync, pattern SyncReply
  , pattern ClearSched
  , pattern PrintErrors
  , pattern Version, pattern VersionReply
  -- * Synth Definition Commands
  , pattern ReceiveSynthDef
  , pattern ReceiveSynthDefThen
  , pattern ReceiveSynthDefDone
  , pattern LoadSynthDef
  , pattern LoadSynthDefThen
  , pattern LoadSynthDefDone
  , pattern LoadSynthDefDirectory
  , pattern LoadSynthDefDirectoryThen
  , pattern LoadSynthDefDirectoryDone
  , pattern FreeSynthDef
  -- * Node Commands
  , pattern FreeNode
  , pattern RunNode
  , pattern SetNodeControl
  , pattern SetNodeControls
  , pattern FillNodeControls
  , pattern MapNodeControl
  , pattern MapNodeControls
  , pattern PlaceNodeBefore
  , pattern PlaceNodeAfter
  , pattern QueryNode
  -- * Synth Commands
  , pattern NewSynth
  , pattern SynthCreated
  , pattern SynthEnded
  , pattern SynthInfo
  , pattern GetSynthControls
  -- * Group Commands
  , pattern NewGroup, pattern NewDefaultGroup
  , pattern FreeNodesInGroup
  -- * Unit Generator Commands
  , pattern UnitGenerator
  -- * Buffer Commands
  -- | Buffers are stored in a global array, indexed by integers starting at zero.
  , BufferID
  , pattern AllocateBuffer, pattern AllocateBufferThen
  , pattern AllocReadBuffer, pattern AllocReadBufferThen
  ,         allocReadBuffer,         allocReadBufferThen
  , pattern AllocReadBufferDone
  , pattern RequestBufferInfo, pattern BufferInfo
  , Message
  ( Message
  , Done
  )
, Datum(..), ControlIndex, controlIndex, ControlValue
, DumpMode(NoDump, DumpParsed, DumpHex, DumpParsedAndHex)
, AddAction(AddToHead, AddToTail, Before, After, Replace)
, pattern DefaultGroup
, ClientID(..), NodeID, GroupID, SynthID, Filename, StartFrame, FrameCount
) where

import           Control.DeepSeq              (NFData)
import           Data.Binary                  (decodeOrFail, encode)
import           Data.Bool                    (bool)
import           Data.Int                     (Int32)
import           Data.Sequence                (Seq ((:|>)))
import qualified Data.Sequence                as Seq (Seq (Empty))
import           Data.Sequence.NonEmpty       (NESeq ((:||>)))
import           Data.String                  (IsString (fromString))
import           GHC.Generics                 (Generic)
import           Sound.Osc                    (Ascii,
                                               Datum (AsciiString, Blob, Double, Float, Int32),
                                               Message (Message), ascii,
                                               ascii_to_string, d_ascii_string,
                                               d_int32, decodeMessage,
                                               encodeMessage, int32, string)
import           Sound.SuperCollider.SynthDef

-- | Quit program. Exits the synthesis server.
-- Replies to sender with 'QuitDone' just before completion.
pattern Quit :: Message
pattern Quit = Message "/quit" []

pattern Done :: [Datum] -> Message
pattern Done args = Message "/done" args

pattern QuitDone :: Message
pattern QuitDone = Done [AsciiString "/quit"]

newtype ClientID = ClientID { unClientID :: Int32 }
                 deriving (Enum, Eq, Generic, NFData, Ord, Read, Show)

boolDatum :: Bool -> Datum
boolDatum = Int32 . bool 0 1

-- | Register to receive notifications from server.
-- The server will answer with 'NotifyReply' which will contain a unique
-- 'ClientID' assigned to this client.  If you have already got a 'ClientID'
-- and want to change the current notification status see 'NotifyWithID'.
pattern Notify :: Bool -> Message
pattern Notify flag <- Message "/notify" [Int32 (isOn -> flag)] where
  Notify flag = Message "/notify" [boolDatum flag]

pattern NotifyWithID :: Bool -> ClientID -> Message
pattern NotifyWithID flag clientID <- Message "/notify" [Int32 (isOn -> flag), Int32 (ClientID -> clientID)] where
  NotifyWithID flag clientID = Message "/notify" [boolDatum flag, Int32 (unClientID clientID)]

pattern NotifyReply :: ClientID -> Int32 -> Message
pattern NotifyReply clientID maxLogins <- Done [AsciiString "/notify", Int32 (ClientID -> clientID), Int32 maxLogins] where
  NotifyReply clientID maxLogins = Done [AsciiString "/notify", Int32 (unClientID clientID), Int32 maxLogins]

-- | Query the status.
-- The server will answer with 'StatusReply'.
pattern Status :: Message
pattern Status = Message "/status" []

pattern StatusReply :: Int32 -> Int32 -> Int32 -> Int32 -> Float -> Float -> Double -> Double -> Message
pattern StatusReply ugens synths groups synthDefs avgCpu peakCpu nominalSampleRate actualSampleRate = Message "/status.reply" [Int32 1, Int32 ugens, Int32 synths, Int32 groups, Int32 synthDefs, Float avgCpu, Float peakCpu, Double nominalSampleRate, Double actualSampleRate]

-- | Plug-in defined command.
pattern Plugin :: Ascii -> [Datum] -> Message
pattern Plugin name args = Message "/cmd" (AsciiString name : args)

newtype DumpMode = DumpMode { unDumpMode :: Int32 } deriving (Eq)
pattern NoDump :: DumpMode
pattern NoDump = DumpMode 0
pattern DumpParsed :: DumpMode
pattern DumpParsed = DumpMode 1
pattern DumpHex :: DumpMode
pattern DumpHex = DumpMode 2
pattern DumpParsedAndHex :: DumpMode
pattern DumpParsedAndHex = DumpMode 3

-- | Display incoming OSC messages.
-- Turns on and off printing of the contents of incoming Open Sound Control
-- messages. This is useful when debugging your command stream.
pattern DumpOSC :: DumpMode -> Message
pattern DumpOSC code <- Message "/dumpOSC" [Int32 (DumpMode -> code)] where
  DumpOSC code = Message "/dumpOSC" [Int32 $ unDumpMode code]

type SyncID = Int32

-- | Notify when async commands have completed.
-- Replies with a 'SyncReply' message when all asynchronous commands received
-- before this one have completed. The reply will contain the sent unique ID.
pattern Sync :: SyncID -> Message
pattern Sync id   = Message "/sync"   [Int32 id]

pattern SyncReply :: SyncID -> Message
pattern SyncReply id = Message "/synced" [Int32 id]

-- | Clear all scheduled bundles.
-- Removes all bundles from the scheduling queue.
pattern ClearSched :: Message
pattern ClearSched = Message "/clearSched" []

-- | Enable/disable error message posting.
-- Turn on or off error messages printed to the SuperCollider output stream.
-- Useful when sending a message, such as 'FreeNode',
-- whose failure does not necessarily indicate anything wrong.
pattern PrintErrors :: Bool -> Message
pattern PrintErrors flag <- Message "/error" [Int32 (matchGlobalErrorFlag -> Just flag)] where
  PrintErrors flag = Message "/error" [Int32 $ bool 0 1 flag]

matchGlobalErrorFlag :: Int32 -> Maybe Bool
matchGlobalErrorFlag = \case
  1 -> Just True
  0 -> Just False
  _ -> Nothing

-- | Query the SuperCollider version.
-- Replies to sender with the 'VersionReply' message.
pattern Version :: Message
pattern Version = Message "/version" []

pattern VersionReply :: Ascii
                     -- ^ Program name
                     -> Int32
                     -- ^ Major version number
                     -> Int32
                     -- ^ Minor version number
                     -> Ascii
                     -- ^ Patch version name
                     -> Ascii
                     -- ^ Git branch or tag name
                     -> Ascii
                     -- ^ First seven hex digits of the commit hash
                     -> Message
pattern VersionReply name major minor patch vcTag vcHash = Message "/version.reply" [AsciiString name, Int32 major, Int32 minor, AsciiString patch, AsciiString vcTag, AsciiString vcHash]

-- | Receive a synth definition.
-- Replies to sender with 'ReceiveSynthDefDone' when complete.
pattern ReceiveSynthDef :: SynthDef -> Message
pattern ReceiveSynthDef def <- Message "/d_recv" [Blob (decodeOrFail -> Right ("", _, def))] where
  ReceiveSynthDef def = Message "/d_recv" [Blob (encode def)]

pattern ReceiveSynthDefDone :: Message
pattern ReceiveSynthDefDone = Done [AsciiString "/d_recv"]

-- | Receive a synth definition and an asynchronous continuation message.
-- Replies to sender with 'ReceiveSynthDefDone' when complete.
pattern ReceiveSynthDefThen :: SynthDef -> Message -> Message
pattern ReceiveSynthDefThen def msg <- Message "/d_recv" [Blob (decodeOrFail -> Right ("", _, def)), Blob (decodeMessage -> msg)] where
  ReceiveSynthDefThen def msg = Message "/d_recv" [Blob (encode def), Blob (encodeMessage msg)]

-- | Load synth definitions from file(s).
-- Replies to sender with 'LoadSynthDefDone' when complete.
pattern LoadSynthDef :: FilePath -> Message
pattern LoadSynthDef glob <- Message "/d_load" [AsciiString (ascii_to_string -> glob)] where
  LoadSynthDef glob = Message "/d_load" [string glob]

pattern LoadSynthDefDone :: Message
pattern LoadSynthDefDone = Done [AsciiString "/d_load"]

pattern LoadSynthDefThen :: FilePath -> Message -> Message
pattern LoadSynthDefThen glob msg <- Message "/d_load" [AsciiString (ascii_to_string -> glob), Blob (decodeMessage -> msg)] where
  LoadSynthDefThen glob msg = Message "/d_load" [string glob, Blob (encodeMessage msg)]

-- | Load a directory of synth definitions.
-- Replies to sender with 'LoadSynthDefDirectoryDone' when complete.
pattern LoadSynthDefDirectory :: FilePath -> Message
pattern LoadSynthDefDirectory dir <- Message "/d_loadDir" [AsciiString (ascii_to_string -> dir)] where
  LoadSynthDefDirectory dir = Message "/d_loadDir" [string dir]

pattern LoadSynthDefDirectoryDone :: Message
pattern LoadSynthDefDirectoryDone = Done [AsciiString "/d_loadDir"]

pattern LoadSynthDefDirectoryThen :: FilePath -> Message -> Message
pattern LoadSynthDefDirectoryThen dir msg <- Message "/d_loadDir" [AsciiString (ascii_to_string -> dir), Blob (decodeMessage -> msg)] where
  LoadSynthDefDirectoryThen dir msg = Message "/d_loadDir" [string dir, Blob (encodeMessage msg)]

-- | Delete synth definitions.
-- Removes synth definitions.
-- The definitions are removed immediately, and do not wait for synth nodes
-- based on these definitions to end.
pattern FreeSynthDef :: [String] -> Message
pattern FreeSynthDef names <- Message "/d_free" (match -> Just names) where
  FreeSynthDef names = Message "/d_free" $ datum names

-- ----------------------------------------------------------------------------


type NodeID = Int32

-- | Delete nodes.
-- Stops nodes abruptly, removes them from their groups, and frees their memory.
-- Using this can cause a click if the nodes are not silent at the time they
-- are freed.
pattern FreeNode :: [NodeID] -> Message
pattern FreeNode ids <- Message "/n_free" (match -> Just ids) where
  FreeNode ids = Message "/n_free" $ datum ids

-- | Turn nodes on or off.
-- Using this method to start and stop nodes can cause a click if the node is
-- not silent at the time run flag is toggled.
pattern RunNode :: [(NodeID, Bool)] -> Message
pattern RunNode args <- Message "/n_run" (match -> Just args) where
  RunNode args = Message "/n_run" $ datum args

-- Either a named control or a control index
data ControlIndex = CName Ascii
                  | CIndex Int32
                  deriving (Eq, Generic, Read, Show)

instance NFData ControlIndex

controlIndex :: Int32 -> ControlIndex
controlIndex = CIndex

controlDatum :: ControlIndex -> Datum
controlDatum (CIndex c) = Int32 c
controlDatum (CName c)  = AsciiString c

instance IsString ControlIndex where
  fromString = CName . ascii

data ControlValue = CVInt Int32
                  | CVFloat Float
                  deriving (Eq, Generic, Read, Show)

instance NFData ControlValue

instance Ord ControlValue where
  compare (CVFloat a) (CVFloat b) = compare a b
  compare (CVFloat a) (CVInt b)   = compare a (realToFrac b)
  compare (CVInt a) (CVFloat b)   = compare (realToFrac a) b
  compare (CVInt a) (CVInt b)     = compare a b

instance Num ControlValue where
  fromInteger = CVInt . fromInteger
  abs (CVInt a)   = CVInt $ abs a
  abs (CVFloat a) = CVFloat $ abs a
  negate (CVInt a)   = CVInt $ negate a
  negate (CVFloat a) = CVFloat $ negate a
  signum (CVInt a)   = CVInt $ signum a
  signum (CVFloat a) = CVFloat $ signum a
  CVInt a   + CVInt b   = CVInt   $ a + b
  CVFloat a + CVFloat b = CVFloat $ a + b
  CVInt a   + CVFloat b = CVFloat $ realToFrac a + b
  CVFloat a + CVInt b   = CVFloat $ a + realToFrac b
  CVInt a   * CVInt b   = CVInt   $ a * b
  CVFloat a * CVFloat b = CVFloat $ a * b
  CVInt a   * CVFloat b = CVFloat $ realToFrac a * b
  CVFloat a * CVInt b   = CVFloat $ a * realToFrac b
  CVInt a   - CVInt b   = CVInt   $ a - b
  CVFloat a - CVFloat b = CVFloat $ a - b
  CVInt a   - CVFloat b = CVFloat $ realToFrac a - b
  CVFloat a - CVInt b   = CVFloat $ a - realToFrac b

instance Fractional ControlValue where
  fromRational = CVFloat . fromRational
  recip (CVFloat a) = CVFloat $ recip a
  recip (CVInt a)   = CVFloat $ recip (realToFrac a)
  CVFloat a / CVFloat b = CVFloat $ a / b
  CVInt a / CVFloat b   = CVFloat $ realToFrac a / b
  CVFloat a / CVInt b   = CVFloat $ a / realToFrac b
  CVInt a / CVInt b     = CVFloat $ realToFrac a / realToFrac b

instance Real ControlValue where
  toRational (CVInt a)   = toRational a
  toRational (CVFloat a) = toRational a

instance Floating ControlValue where
  pi = CVFloat pi
  exp (CVFloat a) = CVFloat $ exp a
  exp (CVInt a)   = CVFloat $ exp (realToFrac a)
  log (CVFloat a) = CVFloat $ log a
  log (CVInt a)   = CVFloat $ log (realToFrac a)
  sin (CVFloat a) = CVFloat $ sin a
  sin (CVInt a)   = CVFloat $ sin (realToFrac a)
  cos (CVFloat a) = CVFloat $ cos a
  cos (CVInt a)   = CVFloat $ cos (realToFrac a)
  tan (CVFloat a) = CVFloat $ tan a
  tan (CVInt a)   = CVFloat $ tan (realToFrac a)
  asin (CVFloat a) = CVFloat $ asin a
  asin (CVInt a)   = CVFloat $ asin (realToFrac a)
  acos (CVFloat a) = CVFloat $ acos a
  acos (CVInt a)   = CVFloat $ acos (realToFrac a)
  atan (CVFloat a) = CVFloat $ atan a
  atan (CVInt a)   = CVFloat $ atan (realToFrac a)
  sinh (CVFloat a) = CVFloat $ sinh a
  sinh (CVInt a)   = CVFloat $ sinh (realToFrac a)
  cosh (CVFloat a) = CVFloat $ cosh a
  cosh (CVInt a)   = CVFloat $ cosh (realToFrac a)
  tanh (CVFloat a) = CVFloat $ tanh a
  tanh (CVInt a)   = CVFloat $ tanh (realToFrac a)
  asinh (CVFloat a) = CVFloat $ asinh a
  asinh (CVInt a)   = CVFloat $ asinh (realToFrac a)
  acosh (CVFloat a) = CVFloat $ acosh a
  acosh (CVInt a)   = CVFloat $ acosh (realToFrac a)
  atanh (CVFloat a) = CVFloat $ atanh a
  atanh (CVInt a)   = CVFloat $ atanh (realToFrac a)

-- | Set a node's control value(s).
-- Takes a list of pairs of controls and values and sets the controls to those
-- values.  If the node is a group, then it sets the controls of
-- every node in the group.
pattern SetNodeControl :: NodeID -> [(ControlIndex, ControlValue)] -> Message
pattern SetNodeControl nid args <- Message "/n_set" (Int32 nid : (match -> Just args)) where
  SetNodeControl nid args = Message "/n_set" $ Int32 nid : datum args

-- | Set ranges of a node's control value(s).
-- Set contiguous ranges of control indices to sets of values.
-- For each range, the starting control index is given followed by the values.
-- If the node is a group, then it sets the controls of every node in the group.
pattern SetNodeControls :: NodeID -> [(ControlIndex, [ControlValue])] -> Message
pattern SetNodeControls nid args <- Message "/n_setn" (Int32 nid : (match -> Just args)) where
  SetNodeControls nid args = Message "/n_setn" $ Int32 nid : datum args

-- | Fill ranges of a node's control value(s).
-- Set contiguous ranges of control indices to single values.
-- For each range, the starting control index is given
-- followed by the number of controls to change, followed by the value to fill.
-- If the node is a group, then it sets the controls of every node in the group.
pattern FillNodeControls :: NodeID -> [(ControlIndex, Int32, ControlValue)] -> Message
pattern FillNodeControls nid args <- Message "/n_fill" (Int32 nid : (match -> Just args)) where
  FillNodeControls nid args = Message "/n_fill" $ Int32 nid : datum args

type BusID = Int32
data BusType a = AudioBus a
               | ControlBus a deriving (Eq, Generic, Read, Show)

instance NFData a => NFData (BusType a)

matchMap :: Message -> Maybe (NodeID, BusType [(ControlIndex, BusID)])
matchMap = \case
  Message "/n_mapa" (Int32 nid : (match -> Just args)) ->
    Just (nid, AudioBus args)
  Message "/n_map" (Int32 nid : (match -> Just args)) ->
    Just (nid, ControlBus args)
  _ -> Nothing

matchMapN :: Message -> Maybe (NodeID, BusType [(ControlIndex, BusID, Int32)])
matchMapN = \case
  Message "/n_mapan" (Int32 nid : (match -> Just args)) ->
    Just (nid, AudioBus args)
  Message "/n_mapn" (Int32 nid : (match -> Just args)) ->
    Just (nid, ControlBus args)
  _ -> Nothing

-- | Map a node's controls to read from a bus.
-- Takes a list of pairs of control names or indices and bus indices
-- and causes those controls to be read continuously from a global bus.
-- If the node is a group, then it maps the controls of every node in the group.
-- If the bus index is -1 then any current mapping is undone.
-- Any 'SetNode', 'SetNodeN' and 'FillNodeControls' command will also
-- unmap the control.
pattern MapNodeControl :: NodeID -> BusType [(ControlIndex, BusID)] -> Message
pattern MapNodeControl nid args <- (matchMap -> Just (nid, args)) where
  MapNodeControl nid (AudioBus args) = Message "/n_mapa" $ Int32 nid : datum args
  MapNodeControl nid (ControlBus args) = Message "/n_map" $ Int32 nid : datum args

-- | Map a node's controls to read from buses.
-- Takes a list of triplets of control names or indices, bus indices,
-- and number of controls to map and causes those controls to be mapped
-- sequentially to buses.
-- If the node is a group, then it maps the controls of every node in the group.
-- If the bus index is -1 then any current mapping is undone.
-- Any 'SetNodeControl', 'SetNodeControls' and 'FillNodeControls' command will
-- also unmap the control.
pattern MapNodeControls :: NodeID -> BusType [(ControlIndex, BusID, Int32)] -> Message
pattern MapNodeControls nid args <- (matchMapN -> Just (nid, args)) where
  MapNodeControls nid (AudioBus args) = Message "/n_mapan" $ Int32 nid : datum args
  MapNodeControls nid (ControlBus args) = Message "/n_mapn" $ Int32 nid : datum args

-- | Place a node before another.
-- Places node A in the same group as node B, to execute immediately before node B.
pattern PlaceNodeBefore :: [(NodeID, NodeID)] -> Message
pattern PlaceNodeBefore args <- Message "/n_before" (match -> Just args) where
  PlaceNodeBefore args = Message "/n_before" $ datum args

-- | Place a node after another.
-- Places node A in the same group as node B, to execute immediately after node B.
pattern PlaceNodeAfter :: [(NodeID, NodeID)] -> Message
pattern PlaceNodeAfter args <- Message "/n_after" (match -> Just args) where
  PlaceNodeAfter args = Message "/n_after" $ datum args

-- | Get info about a node.
-- The server sends either a 'SynthInfo' or a 'GroupInfo' message
-- for each node to registered clients.
pattern QueryNode :: [NodeID] -> Message
pattern QueryNode nodes <- Message "/n_query" (match -> Just nodes) where
  QueryNode nodes = Message "/n_query" $ datum nodes

type GroupID = NodeID

newtype AddAction = AddAction { unAddAction :: Int32 } deriving (Eq)
pattern AddToHead, AddToTail, Before, After, Replace :: AddAction
pattern AddToHead = AddAction 0
pattern AddToTail = AddAction 1
pattern Before    = AddAction 2
pattern After     = AddAction 3
pattern Replace   = AddAction 4

pattern NewGroup :: GroupID -> AddAction -> NodeID -> Message
pattern NewGroup gid addAction targetID <- Message "/g_new" [Int32 gid, Int32 (AddAction -> addAction), Int32 targetID] where
  NewGroup gid addAction targetID = Message "/g_new" [int32 gid, int32 (unAddAction addAction), int32 targetID]

pattern DefaultGroup :: GroupID
pattern DefaultGroup = 1

pattern NewDefaultGroup :: Message
pattern NewDefaultGroup = NewGroup DefaultGroup AddToTail 0

-- | Delete all nodes in a group.
pattern FreeNodesInGroup :: [GroupID] -> Message
pattern FreeNodesInGroup gids <- Message "/g_freeAll" (match -> Just gids) where
  FreeNodesInGroup gids = Message "/g_freeAll" $ datum gids

-- | Send a command to a unit generator.
-- Sends all arguments following the command name to the unit generator to be
-- performed. Commands are defined by unit generator plug ins.
pattern UnitGenerator :: NodeID -> Int32 -> Ascii -> [Datum] -> Message
pattern UnitGenerator nid ugen cmd args = Message "/u_cmd" (Int32 nid : Int32 ugen : AsciiString cmd : args)

type BufferIndex = Int32
type ChannelCount = Int32

pattern AllocateBuffer :: BufferIndex -> FrameCount -> ChannelCount -> Message
pattern AllocateBuffer bid nframes chans = Message "/b_alloc" [Int32 bid, Int32 nframes, Int32 chans]

pattern AllocateBufferThen :: BufferIndex -> FrameCount -> ChannelCount -> Message -> Message
pattern AllocateBufferThen bid nframes chans msg <- Message "/b_alloc" [Int32 bid, Int32 nframes, Int32 chans, Blob (decodeMessage -> msg)] where
  AllocateBufferThen bid nframes chans msg = Message "/b_alloc" [Int32 bid , Int32 nframes, Int32 chans, Blob (encodeMessage msg)]

-- | Get buffer info.
pattern RequestBufferInfo :: [BufferIndex] -> Message
pattern RequestBufferInfo bids <- Message "/b_query" (match -> Just bids) where
  RequestBufferInfo bids = Message "/b_query" $ datum bids

type SampleRate = Float

pattern BufferInfo :: [(BufferIndex, FrameCount, ChannelCount, SampleRate)] -> Message
pattern BufferInfo xs <- Message "/b_info" (match -> Just xs) where
  BufferInfo xs = Message "b/b_info" $ datum xs

type SynthDefName = Ascii
type SynthID = NodeID

pattern NewSynth :: SynthDefName -> SynthID
                 -> AddAction -> NodeID
                 -> [(ControlIndex, ControlValue)]
                 -> Message
pattern NewSynth name sid addAction targetID args <- Message "/s_new" (AsciiString name : Int32 sid : Int32 (AddAction -> addAction) : Int32 targetID : (match -> Just args)) where
  NewSynth name sid addAction targetID args = Message "/s_new" (AsciiString name : Int32 sid : int32 (unAddAction addAction) : Int32 targetID : datum args)

-- | Get control value(s).
-- Replies to sender with the corresponding 'SetNode' command.
pattern GetSynthControls :: SynthID -> [ControlIndex] -> Message
pattern GetSynthControls sid args <- Message "/s_get" (Int32 sid : (match -> Just args)) where
  GetSynthControls sid args = Message "/s_get" $ Int32 sid : datum args

pattern SynthCreated :: NodeID -> GroupID -> NodeID -> NodeID
                     -> Message
pattern SynthCreated id pgid nextNodeID prevNodeID = Message "/n_go"
  [Int32 id, Int32 pgid, Int32 nextNodeID, Int32 prevNodeID, Int32 0]
pattern SynthEnded :: NodeID -> GroupID -> NodeID -> NodeID
                   -> Message
pattern SynthEnded id pgid nextNodeID prevNodeID = Message "/n_end"
  [Int32 id, Int32 pgid, Int32 nextNodeID, Int32 prevNodeID, Int32 0]
pattern SynthInfo :: SynthID -> GroupID -> NodeID -> NodeID -> Message
pattern SynthInfo id pgid nextNodeID prevNodeID = Message "/n_info"
  [Int32 id, Int32 pgid, Int32 nextNodeID, Int32 prevNodeID, Int32 0]

type BufferID = Int32
type StartFrame = Int32
type FrameCount = Int32
type Filename = Ascii

-- | Allocate buffer space and read a sound file.
-- Allocates buffer to number of channels of file and number of samples requested,
-- or fewer if sound file is smaller than requested.
-- Reads sound file data from the given starting frame in the file.
-- If the number of frames argument is less than or equal to zero,
-- the entire file is read.
-- Replies to sender with 'AllocReadBufferDone' when complete.
pattern AllocReadBuffer :: BufferID -> Filename -> StartFrame -> FrameCount
                        -> Message
pattern AllocReadBuffer id filename startFrame numFrames = Message "/b_allocRead"
  [Int32 id, AsciiString filename, Int32 startFrame, Int32 numFrames]
pattern AllocReadBufferDone :: BufferID -> Message
pattern AllocReadBufferDone bid = Done [AsciiString "/b_allocRead", Int32 bid]

pattern AllocReadBufferThen :: BufferID -> Filename -> StartFrame -> FrameCount
                            -> Message
                            -> Message
pattern AllocReadBufferThen bid filename startFrame numFrames msg <- Message "/b_allocRead" [Int32 bid, AsciiString filename, Int32 startFrame, Int32 numFrames, Blob (decodeMessage -> msg)] where
  AllocReadBufferThen bid filename startFrame numFrames msg = Message "/b_allocRead" [Int32 bid, AsciiString filename, Int32 startFrame, Int32 numFrames, Blob (encodeMessage msg)]

allocReadBuffer :: NESeq (BufferID, Filename, StartFrame, FrameCount)
                -> Message
allocReadBuffer (xs :||> x) = xs `allocReadBufferThen` uncurry4 AllocReadBuffer x

allocReadBufferThen :: Seq (BufferID, Filename, StartFrame, FrameCount)
                    -> Message
                    -> Message
allocReadBufferThen Seq.Empty msg  = msg
allocReadBufferThen (xs :|> x) msg = foldr f (f x msg) xs where
  f = uncurry4 AllocReadBufferThen

-- -----------------------------------------------------------------------------

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

isOn :: (Num a, Ord a) => a -> Bool
isOn = (> 0)

matchList :: ([a] -> Maybe ([a], b)) -> [a] -> Maybe [b]
matchList f = go where
  go [] = Just []
  go xs = f xs >>= \(xs', x) -> (x :) <$> go xs'

matchListN :: (Ord n, Num n) => (a -> Maybe (a, b)) -> n -> a -> Maybe (a, [b])
matchListN f = go where
  go n xs | n < 0     = Nothing
          | n == 0    = Just (xs, [])
          | otherwise = f xs >>= \(xs', x) -> fmap (x :) <$> go (n - 1) xs'

class Convert a where
  match :: [Datum] -> Maybe a
  datum :: a -> [Datum]

instance Convert [Int32] where
  match = matchList $ \case
    Int32 x : xs -> Just (xs, x)
    _            -> Nothing
  datum = map Int32

instance Convert [String] where
  match = matchList $ \case
    AsciiString x:xs -> Just (xs, ascii_to_string x)
    _                -> Nothing
  datum = map string

instance Convert [ControlIndex] where
  match = matchList $ \case
    Int32 c : xs       -> Just (xs, CIndex c)
    AsciiString c : xs -> Just (xs, CName c)
    _                  -> Nothing
  datum = map controlDatum

instance Convert [(Int32, Int32)] where
  match = matchList $ \case
    Int32 k : Int32 v : xs -> Just (xs, (k, v))
    _                      -> Nothing
  datum = concatMap $ \(k, v) -> [Int32 k, Int32 v]

instance Convert [(Int32, Bool)] where
  match = (fmap . fmap . fmap) (> z) . match where z = 0 :: Int32
  datum = datum . (fmap . fmap) (bool 0 1 :: Bool -> Int32)

instance Convert [(Int32, Int32, Int32, Float)] where
  match = matchList $ \case
    Int32 a : Int32 b : Int32 c : Float d : xs -> Just (xs, (a, b, c, d))
    _                      -> Nothing
  datum = concatMap $ \(a, b, c, d) -> [Int32 a, Int32 b, Int32 c, Float d]

instance Convert [(ControlIndex, Int32)] where
  match = matchList $ \case
    Int32 c : Int32 v : xs       -> Just (xs, (CIndex c, v))
    AsciiString c : Int32 v : xs -> Just (xs, (CName c, v))
    _                            -> Nothing
  datum = concatMap $ \(c, v) -> [controlDatum c, Int32 v]

instance Convert [(ControlIndex, Int32, Int32)] where
  match = matchList $ \case
    Int32 c : Int32 v : Int32 w : xs       -> Just (xs, (CIndex c, v, w))
    AsciiString c : Int32 v : Int32 w : xs -> Just (xs, (CName c, v, w))
    _                                      -> Nothing
  datum = concatMap $ \(c, v, w) -> [controlDatum c, Int32 v, Int32 w]

instance Convert [(ControlIndex, Int32, ControlValue)] where
  match = matchList $ \case
    Int32 c : Int32 n : Int32 v : xs -> Just (xs, (CIndex c, n, CVInt v))
    Int32 c : Int32 n : Float v : xs -> Just (xs, (CIndex c, n, CVFloat v))
    AsciiString c : Int32 n : Int32 v : xs -> Just (xs, (CName c, n, CVInt v))
    AsciiString c : Int32 n : Float v : xs -> Just (xs, (CName c, n, CVFloat v))
    _ -> Nothing
  datum = concatMap $ \case
    (c, n, CVInt v)   -> [controlDatum c, Int32 n, Int32 v]
    (c, n, CVFloat v) -> [controlDatum c, Int32 n, Float v]

instance Convert [(ControlIndex, ControlValue)] where
  match = matchList $ \case
    Int32 c : Int32 v : xs       -> Just (xs, (CIndex c, CVInt v))
    Int32 c : Float v : xs       -> Just (xs, (CIndex c, CVFloat v))
    AsciiString c : Int32 v : xs -> Just (xs, (CName c, CVInt v))
    AsciiString c : Float v : xs -> Just (xs, (CName c, CVFloat v))
    _                            -> Nothing
  datum = foldMap $ \case
    (k, CVInt v)   -> [controlDatum k, Int32 v]
    (k, CVFloat v) -> [controlDatum k, Float v]

instance Convert [(ControlIndex, [ControlValue])] where
  match = matchList ctrl where
    ctrl = \case
      Int32 c : Int32 n : xs -> fmap (CIndex c, ) <$> matchListN value n xs
      AsciiString c : Int32 n : xs -> fmap (CName c, ) <$>
        matchListN value n xs
      _ -> Nothing
    value = \case
      Int32 x:xs -> Just (xs, CVInt x)
      Float x:xs -> Just (xs, CVFloat x)
      _          -> Nothing
  datum = concatMap ctrl where
    ctrl (c, vs) = controlDatum c : Int32 (fromIntegral (length vs)) : go vs
    go = map $ \case
      CVInt v   -> Int32 v
      CVFloat v -> Float v

