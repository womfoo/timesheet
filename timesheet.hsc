import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Function
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.Directory
import System.IO.Unsafe
import System.Locale
import System.Process
import System.Win32

#include <windows.h>

eVENTLOG_BACKWARDS_READ  = #{const EVENTLOG_BACKWARDS_READ}
eVENTLOG_SEQUENTIAL_READ = #{const EVENTLOG_SEQUENTIAL_READ}
mAX_RECORD_BUFFER_SIZE = 0x7ffff -- 512K - 1, as per msdn

openEventLog :: String -> IO HANDLE
openEventLog provider =
  withTString provider $ \ c_provider ->
  failIfNull "OpenEventLog" $ c_OpenEventLogW nullPtr c_provider

foreign import stdcall "windows.h ReadEventLogW" c_ReadEventLogW ::
  HANDLE -> DWORD -> DWORD -> Ptr a -> DWORD -> Ptr DWORD -> Ptr DWORD -> IO Bool -- made PVOID LPVOID

foreign import stdcall "windows.h OpenEventLogW" c_OpenEventLogW ::
  LPCWSTR -> LPCWSTR -> IO HANDLE

foreign import stdcall "windows.h CloseEventLog"
  c_CloseEventLog :: HANDLE -> IO Bool

data EventLog =
  EventLog { length_             :: DWORD
           , reserved            :: DWORD
           , recordnumber        :: DWORD
           , timegenerated       :: DWORD
           , timewritten         :: DWORD
           , eventid             :: DWORD
           , eventtype           :: WORD
           , numstrings          :: WORD
           , eventcategory       :: WORD
           , reservedflags       :: WORD
           , closingrecordnumber :: DWORD
           , stringoffset        :: DWORD
           , usersidlength       :: DWORD
           , usersidoffset       :: DWORD
           , datalength          :: DWORD
           , dataoffset          :: DWORD
           } deriving Show

instance Storable EventLog where
  sizeOf    _ = #{size EVENTLOGRECORD}
  alignment _ = alignment (undefined :: CInt)
  poke p foo  = do
    #{poke EVENTLOGRECORD, Length              } p $ length_             foo
    #{poke EVENTLOGRECORD, Reserved            } p $ reserved            foo
    #{poke EVENTLOGRECORD, RecordNumber        } p $ recordnumber        foo
    #{poke EVENTLOGRECORD, TimeGenerated       } p $ timegenerated       foo
    #{poke EVENTLOGRECORD, TimeWritten         } p $ timewritten         foo
    #{poke EVENTLOGRECORD, EventID             } p $ eventid             foo
    #{poke EVENTLOGRECORD, EventType           } p $ eventtype           foo
    #{poke EVENTLOGRECORD, NumStrings          } p $ numstrings          foo
    #{poke EVENTLOGRECORD, EventCategory       } p $ eventcategory       foo
    #{poke EVENTLOGRECORD, ReservedFlags       } p $ reservedflags       foo
    #{poke EVENTLOGRECORD, ClosingRecordNumber } p $ closingrecordnumber foo
    #{poke EVENTLOGRECORD, StringOffset        } p $ stringoffset        foo
    #{poke EVENTLOGRECORD, UserSidLength       } p $ usersidlength       foo
    #{poke EVENTLOGRECORD, UserSidOffset       } p $ usersidoffset       foo
    #{poke EVENTLOGRECORD, DataLength          } p $ datalength          foo
    #{poke EVENTLOGRECORD, DataOffset          } p $ dataoffset          foo
  peek p = return EventLog
    `ap` (#{peek EVENTLOGRECORD, Length              } p )
    `ap` (#{peek EVENTLOGRECORD, Reserved            } p )
    `ap` (#{peek EVENTLOGRECORD, RecordNumber        } p )
    `ap` (#{peek EVENTLOGRECORD, TimeGenerated       } p )
    `ap` (#{peek EVENTLOGRECORD, TimeWritten         } p )
    `ap` (#{peek EVENTLOGRECORD, EventID             } p )
    `ap` (#{peek EVENTLOGRECORD, EventType           } p )
    `ap` (#{peek EVENTLOGRECORD, NumStrings          } p )
    `ap` (#{peek EVENTLOGRECORD, EventCategory       } p )
    `ap` (#{peek EVENTLOGRECORD, ReservedFlags       } p )
    `ap` (#{peek EVENTLOGRECORD, ClosingRecordNumber } p )
    `ap` (#{peek EVENTLOGRECORD, StringOffset        } p )
    `ap` (#{peek EVENTLOGRECORD, UserSidLength       } p )
    `ap` (#{peek EVENTLOGRECORD, UserSidOffset       } p )
    `ap` (#{peek EVENTLOGRECORD, DataLength          } p )
    `ap` (#{peek EVENTLOGRECORD, DataOffset          } p )

dwordToLocalTime dtime = utcToLocalTime tz $ posixSecondsToUTCTime $ fromIntegral dtime
  where tz = unsafePerformIO getCurrentTimeZone

main = do
  homedir <- getHomeDirectory
  hEventLog <- openEventLog "System"
  let dwBytesToRead = mAX_RECORD_BUFFER_SIZE
  events <- allocaBytes (fromIntegral dwBytesToRead) $ \pBuffer -> 
    alloca $ \ dwBytesRead -> 
    alloca $ \ dwMinimumBytesToRead -> do
      -- TODO: check c_ReadEventLogW return code
      c_ReadEventLogW hEventLog
        (eVENTLOG_SEQUENTIAL_READ .|. eVENTLOG_BACKWARDS_READ)
        0
        pBuffer
        dwBytesToRead
        dwBytesRead
        dwMinimumBytesToRead
      -- TODO: we are cheating by retrieving data with the max buffer. implement reading of the remaining log data
      -- status <- getLastError
      -- putStrLn ("status: " ++ show status)
      peekEvents pBuffer
  c_CloseEventLog hEventLog
  let timestamps = map (dwordToLocalTime . timegenerated) events
      groupedDays = groupBy ((==) `on` localDay) $ sort timestamps
      rows = map toRow groupedDays
      csvrows = map toCSVRow rows
      csvdata = unlines csvrows
      filename = homedir ++ "\\Desktop\\timesheet.csv"
  writeFile filename csvdata
  system $ show filename -- open output with excel/default program. use `show` to enclose the filename in quotes

peekEvents ptr = go 0 ptr
  where
    go offset ptr = do
         let ptr' = plusPtr ptr (fromIntegral offset)
         e <- peek ptr'
         if length_ e /= 0 then
           do let nextoffset = fromIntegral (length_ e)
              es <- go nextoffset ptr'
              return (e:es)
           else return []

toRow :: [LocalTime] -> [String]
toRow xs = [show (localDay dmin), timestring dmin, timestring dmax, daystring]
  where
    dmin = minimum xs
    dmax = maximum xs
    timestring = formatTime defaultTimeLocale "%I:%M %p"
    daystring = formatTime defaultTimeLocale "%a" dmin

toCSVRow = intercalate ","
