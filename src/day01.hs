solve1 xs = [x * y | x <- xs, y <- xs, x <= y, x + y == 2020]

solve2 xs = [x * y * z | x <- xs, y <- xs, z <- xs, x <= y, y <= z, x + y + z == 2020]

main = interact $ unlines . map show . solve2 . map read . lines