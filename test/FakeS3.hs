-- | A minimal stand-in for S3 that serves a single object, with just enough
-- behaviour to exercise ranged downloads (including servers that handle Range
-- badly).
module FakeS3
  ( Behaviour(..)
  , RequestLog
  , withFakeS3
  ) where

import Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import Data.List (lookup)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

data Behaviour
  = HonourRange
    -- ^ Like S3: answer a Range request with 206 and a Content-Range header.
  | IgnoreRange
    -- ^ Ignore Range and answer with the whole object and 200.
  | UnknownTotal
    -- ^ Answer with 206, but without disclosing the object size.
  | FailAtOffset Int
    -- ^ Fail requests for the range starting at the given offset.
  | MissingObject
    -- ^ Answer everything the way S3 reports an object that is not there.
  deriving (Eq, Show)

-- | Range header of every request the server received, in arrival order.
type RequestLog = IORef [Maybe ByteString]

-- | Serve @object@ on a free port, and pass that port to the given action.
withFakeS3 :: Behaviour -> ByteString -> (RequestLog -> Int -> IO a) -> IO a
withFakeS3 behaviour object action = do
  requestLog <- newIORef []
  Warp.testWithApplication (pure (app behaviour object requestLog)) (action requestLog)

app :: Behaviour -> ByteString -> RequestLog -> Wai.Application
app behaviour object requestLog request respond = do
  let m_range = lookup HTTP.hRange (Wai.requestHeaders request)
  modifyIORef' requestLog (<> [m_range])
  let whole = respond $ Wai.responseLBS HTTP.status200
        [("Content-Length", show (BS.length object))] (LBS.fromStrict object)
  case (behaviour, m_range >>= parseRange) of
    (MissingObject, _) ->
      respond $ Wai.responseLBS HTTP.status404 []
        "<Error><Code>NoSuchKey</Code><Message>The specified key does not exist.</Message></Error>"
    (IgnoreRange, _) ->
      whole
    (_, Nothing) ->
      whole
    (FailAtOffset offset, Just (start, _)) | start == offset ->
      respond $ Wai.responseLBS HTTP.status500 []
        "<Error><Code>InternalError</Code></Error>"
    (_, Just (start, end)) -> do
      let lastByte = min end (BS.length object - 1)
          body = BS.take (lastByte - start + 1) $ BS.drop start object
          total = case behaviour of
            UnknownTotal -> "*"
            _ -> show (BS.length object)
      respond $ Wai.responseLBS HTTP.status206
        [ ("Content-Range", "bytes " <> show start <> "-" <> show lastByte <> "/" <> total)
        , ("Content-Length", show (BS.length body))
        ] (LBS.fromStrict body)

-- | Parse an inclusive @bytes=first-last@ range. Other forms are not used by
-- taskrunner, so they are treated as no range at all.
parseRange :: ByteString -> Maybe (Int, Int)
parseRange header = do
  rest <- B8.stripPrefix "bytes=" header
  let (startStr, rest') = B8.break (== '-') rest
  start <- readMaybe (B8.unpack startStr)
  end <- readMaybe (B8.unpack (B8.drop 1 rest'))
  pure (start, end)
