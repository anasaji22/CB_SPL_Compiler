module Register where

newtype Register = Register Int

instance Show Register where
  show :: Register -> String
  show (Register n) = "$" ++ show n

isFreeUse :: Register -> Bool
isFreeUse (Register n) = n >= 8 && n <= 23

minus :: Register -> Int -> Register
minus (Register n) subtrahend = Register $ n - subtrahend

previous :: Register -> Register
previous (Register n) = Register $ n - 1

next :: Register -> Register
next (Register n) = Register $ n + 1

firstFreeUse :: Register
firstFreeUse = Register 8

framePointer :: Register
framePointer = Register 25

stackPointer :: Register
stackPointer = Register 29

returnAddress :: Register
returnAddress = Register 31

null :: Register
null = Register 0