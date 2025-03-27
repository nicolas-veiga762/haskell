f x = 2 * x + 1
b x y = 2 * x + y
maior a b = if a > b then a else b
-- maior3 a b c = if a > b && a > c then a else if b > c && b > a then b else c
-- também pode ser maior3 a b c = maior (maior a b) c

fat 0 = 1
fat n = n * fat(n-1)

pot b 0 = 1
pot b e = b * pot b (e - 1)

mdc a b = if a == b then a else if a > b then mdc (a - b) b else mdc a (b - a)

kkj c u | c == u = c | c > u = kkj (c - u) u |otherwise = kkj c (u - c)

