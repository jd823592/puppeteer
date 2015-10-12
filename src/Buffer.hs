module Buffer (Buffer, mkBuffer, left, right, insert, deleteL, deleteR, toString) where

type Buffer = (String, String)

mkBuffer :: Buffer
mkBuffer = ([], [' '])

left :: Buffer -> Buffer
left (l : ls, rs) = (ls, l : rs)
left b            = b

right :: Buffer -> Buffer
right (ls, r : rs@(_ : _)) = (r : ls, rs)
right b                    = b

insert :: Char -> Buffer -> Buffer
insert c (ls, rs) = (c : ls, rs)

deleteL :: Buffer -> Buffer
deleteL (_ : ls, rs) = (ls, rs)
deleteL b            = b

deleteR :: Buffer -> Buffer
deleteR (ls, _ : rs@(_ : _)) = (ls, rs)
deleteR b                    = b

toString :: Buffer -> (Int, String)
toString (ls, rs) = (length ls, reverse ls ++ rs)
