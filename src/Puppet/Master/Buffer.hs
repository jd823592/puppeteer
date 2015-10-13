module Puppet.Master.Buffer ( Buffer
                            , BufferChange
                            , mkBuffer
                            , start
                            , end
                            , left
                            , right
                            , insert
                            , deleteL
                            , deleteR
                            , toString
                            ) where

type Buffer = (String, String)
type BufferChange = Buffer -> Buffer

mkBuffer :: Buffer
mkBuffer = ([], [' '])

start :: BufferChange
start (ls, rs) = ([], reverse ls ++ rs)

end :: BufferChange
end (ls, rs) = let (r : rs') = reverse rs in (rs' ++ ls, [r])

left :: BufferChange
left (l : ls, rs) = (ls, l : rs)
left b            = b

right :: BufferChange
right (ls, r : rs@(_ : _)) = (r : ls, rs)
right b                    = b

insert :: Char -> BufferChange
insert c (ls, rs) = (c : ls, rs)

deleteL :: BufferChange
deleteL (_ : ls, rs) = (ls, rs)
deleteL b            = b

deleteR :: BufferChange
deleteR (ls, _ : rs@(_ : _)) = (ls, rs)
deleteR b                    = b

toString :: Buffer -> (Int, String)
toString (ls, rs) = (length ls, reverse ls ++ rs)