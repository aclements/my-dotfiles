-- Note that, because XMonad blocks the entire process while waiting
-- on X events, dzenMux depends on heavyweight process-oriented
-- concurrency.  Thus, for example, two xmonad processes will appear
-- in ps and custom producers cannot communicate with other XMonad
-- code through standard Concurrent Haskell abstractions.

module XMonad.Util.DzenMux
    ( -- * Multiplexing a dzen
      dzenMux
      -- * Sample producers
    , prodClock
    , prodTail
    , prodExternal
    , prodConst
      -- * Implementing producers
    , Producer
    , yield
    , terminate
    , testProducer)
    where

import XMonad.Core

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (zipWithM)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (isNothing, fromJust)
import System.IO (Handle, hGetLine, hGetChar, hPutStrLn, hClose,
                  hSetBuffering, BufferMode(LineBuffering))
import System.IO.Error (try, isEOFError)
import System.Locale (defaultTimeLocale)
import System.Posix.IO (createPipe, fdToHandle, closeFd,
                        setFdOption, FdOption(CloseOnExec))
import System.Time

-- | An update from a 'Producer'.
data Update
    = Update String Producer
    | Terminate

-- | A computation in the IO monad that produces a possibly infinite
-- stream of String updates.  Producer computations should end with
-- either 'yield' or 'terminate'.
type Producer = IO Update

-- | The state of the dzen multiplexer.  This records the handle that
-- should be used for concatenated output, as well as the latest
-- updates from each producer.  The handle is wrapped in an MVar
-- that's held during 'refresh' to prevent output races.  Likewise,
-- the latest updates are wrapped in MVar's to allow in-place
-- thread-safe updates.
data State = State (MVar Handle) [MVar (Maybe String)]

-- | Start a dzen multiplexer.  This will run each producer
-- concurrently.  Whenever a producer produces a new value, it will
-- concatenate the latest value of each producer and write this string
-- to the given handle.  If this handle is the input to a dzen
-- process, this allows multiple concurrent threads to update
-- independent fields in the dzen output.
dzenMux :: MonadIO m => Handle -> [Producer] -> m ()
dzenMux hDzen prods = liftIO (dzenMux' hDzen prods)

-- | The real meat of 'dzenMux'.
dzenMux' :: Handle -> [Producer] -> IO ()
dzenMux' hDzen prods = do
  -- Create a pipe so we can tell when the parent dies or exec's
  (child, parent) <- createPipe
  setFdOption parent CloseOnExec True
  -- Create the process that will draw on the producers and write to
  -- the output handle
  doubleFork $ do
    -- Create our state
    partRefs <- mapM (\_ -> newMVar Nothing) prods
    hRef <- newMVar hDzen
    let st = State hRef partRefs
    -- Start the producers
    zipWithM (\ref prod -> forkIO (runProducer st ref prod)) partRefs prods
    -- Wait until the parent dies
    closeFd parent
    child' <- fdToHandle child
    try $ hGetChar child'
    return ()
  closeFd child
  hClose hDzen

-- | Drive a producer, updating the given MVar with its results and
-- calling 'refresh' until the producer terminates.
runProducer :: State -> MVar (Maybe String) -> Producer -> IO ()
runProducer st outRef prod = do
  update <- prod
  case update of
    Update str cont ->
        do modifyMVar_ outRef (const $ return $ Just str)
           refresh st
           runProducer st outRef cont
    Terminate -> return ()

-- | Render the current state to the output handle.  If any of the
-- parts have not yet been updated with an initial value, does
-- nothing.  Note that this is safe to call from multiple threads
-- concurrently.
refresh :: State -> IO ()
refresh (State hRef partRefs) = withMVar hRef $ \hOut -> do
  parts <- mapM readMVar partRefs
  if any isNothing parts then return ()
     else hPutStrLn hOut (concat (map fromJust parts))

-- | When used as the final action in a 'Producer', this will update
-- the producer's output and resume execution with the given
-- continuation.
yield :: String -> Producer -> Producer
yield str cont = return (Update str cont)

-- | When used as the final action in a 'Producer', this will
-- terminate that producer.
terminate :: Producer
terminate = return Terminate

-- | Produce a clock that formats the local time according to the
-- given format string.  For information on the format string, see
-- 'formatCalendarTime'.
prodClock :: String -> Producer
prodClock fmt = produce (clockFormatGranularity fmt)
    where
      produce cycleMicros = do
        tod <- getClockTime
        cal <- toCalendarTime tod
        let TOD todSecs todPicos = tod
            microsPerSec = 1000000
            picosPerMicro = 1000000
            todMicros = todSecs * microsPerSec + todPicos `quot` picosPerMicro
            delay = (cycleMicros - (todMicros `mod` cycleMicros)
                                     + (microsPerSec `quot` 10))
        yield (formatCalendarTime defaultTimeLocale fmt cal)
              (threadDelay (fromInteger delay) >> produce cycleMicros)

-- | Compute the granularity of a clock format string, in
-- microseconds.  This is a value g such that the result of formatting
-- times [g*n, g*(n+1)) in microseconds since the epoch will result in
-- the same string.
clockFormatGranularity :: String -> Integer
clockFormatGranularity fmt = decode fmt
    where decode ('%':'-':cs) = decode ('%':cs)
          decode ('%':'_':cs) = decode ('%':cs)
          decode ('%':c:cs)   = escape c `min` decode cs
          decode (_:cs)       = decode cs
          decode []           =
              -- The maximum granularity we can use without timezone
              -- information is a quarter hour because the granularity
              -- is applied to UTC
              hour `quot` 4
          escape c
              | c `elem` "BbhCmntVWYy%" = day
              | c `elem` "AaDdejUuwx"   = day
              | c `elem` "HIklpZ"       = hour
              | c `elem` "MR"           = minute
              | c `elem` "crTSsX"       = second
              | otherwise               = second  -- Be conservative
          second = 1000000
          minute = 60 * second
          hour   = 60 * minute
          day    = 24 * hour

-- | Read from a handle, producing lines as they are read.
prodTail :: Handle -> Producer
prodTail h = do
  line' <- try $ hGetLine h
  case line' of
    Left ioe
      | isEOFError ioe -> terminate
      | otherwise -> yield ("DzenMux.prodTail: " ++ show ioe) terminate
    Right line -> yield line (prodTail h)

-- | Construct a producer that can be updated via the return action.
-- This is particularly useful for tying this module to
-- "XMonad.Hooks.DynamicLog", as the returned action can be given as
-- the value of 'XMonad.Hooks.DynamicLog.ppOutput'.
prodExternal :: IO (String -> IO (), Producer)
prodExternal = do
  (from', to') <- createPipe
  setFdOption from' CloseOnExec True  
  setFdOption to' CloseOnExec True  
  from <- fdToHandle from'
  to   <- fdToHandle to'
  hSetBuffering to LineBuffering
  -- XXX Would be good to close 'from' in the parent
  return (hPutStrLn to, hClose to >> prodTail from)

-- | Produce some fixed string.
prodConst :: String -> Producer
prodConst str = yield str terminate

-- | Test a producer, printing the strings produced by it until either
-- interrupted, or the producer terminates.
testProducer :: Producer -> IO ()
testProducer prod = do
  update <- prod
  case update of
    Update str cont -> putStrLn str >> testProducer cont
    Terminate -> putStrLn "<Terminated>"

-- main = do
--   (l, p) <- externalProducer
--   dzenMux' stdout [p]
--   eat l
--       where eat l = getLine >>= l >> eat l
