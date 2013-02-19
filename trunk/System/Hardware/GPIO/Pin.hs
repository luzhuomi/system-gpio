module System.Hardware.GPIO.Pin 
       where

import Data.Array

data Pin where 
  InPin :: Int -> Pin
  OutPin :: Int -> Pin
  PwmPin :: Int -> Pin
 deriving (Show, Eq)

num :: Pin -> Int
num (InPin n) = n
num (OutPin n) = n
num (PwmPin n) = n

dir :: Pin -> String
dir (InPin _) = "in"
dir (OutPin _) = "out"
dir (PwmPin _) = "pwm"

pinMapping :: Array Int Int
pinMapping = listArray (0,16) [17, 18, 21, 22, 23, 24, 25, 4, 0, 1, 8, 7, 10, 9, 11, 14, 15]


global_GPIO_PATH = "/sys/class/gpio"
global_EXPORT_PATH = global_GPIO_PATH ++ "/export"
global_UNEXPORT_PATH = global_GPIO_PATH ++ "/unexport"
global_DEVICE_PATH i = global_GPIO_PATH ++ "/gpio"++ (show i)
global_DIRECTION_PATH i = global_DEVICE_PATH i ++ "/direction"
global_VALUE_PATH i = global_DEVICE_PATH i ++ "/value"

init :: Pin -> IO ()
init pin = do 
  { let pn = pinMapping ! (num pin)
  ; writeFile global_EXPORT_PATH (show pn)
  ; writeFile (global_DIRECTION_PATH pn) (dir pin)
  ; case pin of 
       { OutPin _ -> set pin Zero
       ; _        -> return () 
       }
  }
           
close :: Pin -> IO ()
close pin = do
  { let pn = pinMapping ! (num pin)
  ; writeFile global_UNEXPORT_PATH (show pn)
  ; case pin of 
       { OutPin _ -> set pin Zero
       ; _        -> return () 
       }
  }

data Value = One | Zero deriving (Show, Eq)

read :: Pin -> IO Value
read pin = do 
  { let pn = pinMapping ! (num pin)
  ; s <- readFile (global_VALUE_PATH pn)
  ; if (s == "1") 
    then return One
    else return Zero
  }

  
set :: Pin -> Value -> IO ()
set pin@(OutPin n) value = do 
  { let pn = pinMapping ! n
  ; case value of 
       { One -> writeFile (global_VALUE_PATH pn) "1"
       ; Zero -> writeFile (global_VALUE_PATH pn) "0"
       }
  }

