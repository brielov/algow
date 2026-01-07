let rec sum init list = match list
	when Nil -> init
	when Cons cur rest -> sum (cur + init) rest
end

let items = [1, 2, 3, 4]

sum 0 items
