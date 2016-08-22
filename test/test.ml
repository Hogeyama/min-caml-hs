let rec getx v =
  (let (x, y, z) = v in x) in
print_int (truncate (getx (1., 2., 3.)))
