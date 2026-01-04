let rec sum init list = match list with
	| Nil => init
	| Cons cur rest => sum (cur + init) rest
end

let items = 1 :: 2 :: 3 :: 4 :: Nil

sum 0 items
