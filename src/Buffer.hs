module Buffer (Buffer, empty, left, right, insert, backspace, delete, toString) where

type Buffer = (String, String)

empty :: Buffer
empty = ([], [' '])

left :: Buffer -> Buffer
left (l : ls, rs) = (ls, l : rs)
left b            = b

right :: Buffer -> Buffer
right (ls, r : rs@(_ : _)) = (r : ls, rs)
right b                    = b

insert :: Char -> Buffer -> Buffer
insert c (ls, rs) = (c : ls, rs)

backspace :: Buffer -> Buffer
backspace (l : ls, rs) = (ls, rs)
backspace b            = b

delete :: Buffer -> Buffer
delete (ls, _ : rs) = (ls, rs)
delete b            = b

toString :: Buffer -> (String, Int)
toString (ls, rs) = (reverse ls ++ rs, length ls)
