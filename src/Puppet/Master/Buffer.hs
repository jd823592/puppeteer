{-# LANGUAGE TypeOperators #-}

module Puppet.Master.Buffer ( Buffer
                            , BufferChange
                            , mkBuffer
                            , start
                            , end
                            , left
                            , right
                            , up
                            , down
                            , remember
                            , insert
                            , deleteL
                            , deleteR
                            , toString
                            ) where

import Control.Lens
import Control.Zipper

type Buffer = Top :>> [String] :>> String :>> Char
type BufferChange = Buffer -> Buffer

mkBuffer :: Buffer
mkBuffer = zipper [" "] & fromWithin traverse & fromWithin traverse

start :: BufferChange
start = leftmost

end :: BufferChange
end = rightmost

left :: BufferChange
left = tug leftward

right :: BufferChange
right = tug rightward

up :: BufferChange
up = tug leftward
   . rightmost
   . fromWithin traverse
   . tug rightward
   . upward

down :: BufferChange
down = tug leftward
     . rightmost
     . fromWithin traverse
     . tug leftward
     . upward

remember :: BufferChange
remember = fromWithin traverse
         . fromWithin traverse
         . zipper
         . (" " :)
         . rezip

bringUp :: BufferChange
bringUp b = let i   = b  & tooth
                b'  = b  & upward
                l   = b' & view focus
                b'' = b' & leftmost & focus .~ l in b'' & fromWithin traverse & tugTo i

insert :: Char -> BufferChange
insert c b = let b'  = b  & bringUp
                 i   = b' & tooth
                 b'' = b' & upward
                 (p, s) = b'' & view focus & splitAt i in b'' & focus .~ (p ++ [c] ++ s) & fromWithin traverse & tugTo (i + 1)

deleteL :: BufferChange
deleteL b = let b'     = b   & bringUp
                i      = b'  & tooth
                b''    = b'  & upward
                (p, s) = b'' & view focus & splitAt i in b'' & focus .~ (take (length p - 1) p ++ s) & fromWithin traverse & tugTo (i - 1)

deleteR :: BufferChange
deleteR b = let b'  = b   & bringUp
                i   = b'  & tooth
                b'' = b'  & upward
                c   = b'' & view focus & splitAt i in case c of
                    (p, s@(_ : _)) -> b'' & focus .~ (p ++ drop 1 s) & fromWithin traverse & tugTo i
                    _              -> b'

toString :: Buffer -> (Int, String)
toString b = let i = b & tooth
                 l = b & upward & view focus in (i, l)
