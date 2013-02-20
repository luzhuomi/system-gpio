module System.Hardware.GPIO.Pin 
       where

import Data.Array
import System.IO

data Pin = Pin { num :: Int
               , handle :: Handle 
               , dir :: PinMode
               } 
         deriving (Show, Eq)

data PinMode = In | Out | Pwm deriving Eq

instance Show PinMode where
  show In = "in"
  show Out = "out"
  show Pwm = "pwm"

-- mapping raspberry pi pin number to internal bmc2xxx pin number
pinMapping :: Array Int Int
pinMapping = listArray (0,16) [17, 18, 21, 22, 23, 24, 25, 4, 0, 1, 8, 7, 10, 9, 11, 14, 15]


global_GPIO_PATH = "/sys/class/gpio"
global_EXPORT_PATH = global_GPIO_PATH ++ "/export"
global_UNEXPORT_PATH = global_GPIO_PATH ++ "/unexport"
global_DEVICE_PATH i = global_GPIO_PATH ++ "/gpio"++ (show i)
global_DIRECTION_PATH i = global_DEVICE_PATH i ++ "/direction"
global_VALUE_PATH i = global_DEVICE_PATH i ++ "/value"


openWriteClose :: FilePath -> String -> IO ()
openWriteClose fp str = do 
  { hdl <- openFile fp WriteMode
  ; hPutStr hdl str 
  ; hFlush hdl
  ; hClose hdl
  }

-- ^ init pin_number direction
init :: Int -> PinMode -> IO Pin
init i d = do 
  { let pn = pinMapping ! i
  ; openWriteClose global_EXPORT_PATH (show pn)
  ; openWriteClose (global_DIRECTION_PATH pn) (show d)
  ; hdl <- case d of 
           { In -> openFile (global_VALUE_PATH pn) ReadMode
           ; _  -> openFile (global_VALUE_PATH pn) ReadWriteMode
           }
  ; let pin = Pin i hdl d    
  ; case d of 
       { Out -> set pin Zero
       ; _   -> return () 
       }
  ; return pin
  }
           
close :: Pin -> IO ()
close pin = do
  { let pn = pinMapping ! (num pin)
  ; case (dir pin) of 
       { Out -> set pin Zero
       ; _   -> return () 
       }
  ; let hdl = handle pin
  ; hFlush hdl
  ; hClose hdl
  ; openWriteClose global_UNEXPORT_PATH (show pn)
  }

data Value = One | Zero deriving (Show, Eq)

read :: Pin -> IO Value
read pin = do 
  { let hdl = handle pin
  ; hSeek hdl AbsoluteSeek 0
  ; c <- hGetChar hdl
  ; if (c == '1') 
    then return One
    else return Zero
  }

  
set :: Pin -> Value -> IO ()
set (Pin _ hdl Out) value = do 
  { hSeek hdl AbsoluteSeek 0
  ; case value of 
       { One -> hPutChar hdl '1'
       ; Zero -> hPutChar hdl '0'
       }
  ; hFlush hdl
  }
set (Pin _ _ _) value = return ()  

