{-# LANGUAGE CPP,ForeignFunctionInterface #-}

#if defined(i386_HOST_ARCH)
#   define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
#   define WINDOWS_CCONV ccall
#else
#   error Unknown mingw32 arch
#endif

module Lib where

import System.Win32.Types (HANDLE, BOOL, SHORT, DWORD)
import Graphics.Win32.Misc (getStdHandle,sTD_OUTPUT_HANDLE)
import qualified Control.Monad as CM (void)

import Data.Bits

import Data.Char
import Foreign.C.Types

import GHC.GHCi.Helpers (flushAll)

printChar :: Char -> IO ()
printChar c = do
  putChar c
  flushAll

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt


newtype COORD = COORD DWORD

foreign import WINDOWS_CCONV "windows.h SetConsoleCursorPosition"
            setConsoleCursorPosition :: HANDLE -> COORD -> IO BOOL

mkCOORD:: SHORT -> SHORT -> COORD
mkCOORD x y = COORD $ ((fromIntegral y) `shiftL` 16) .|. (fromIntegral x)

setCursor :: Int -> Int -> IO ()
setCursor x y = do
    hOut <- getStdHandle  sTD_OUTPUT_HANDLE
    let cp = mkCOORD (fromIntegral x) (fromIntegral y)
    CM.void $ setConsoleCursorPosition hOut cp