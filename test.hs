flatten :: [(Integer, Integer)] -> [Integer]
flatten listo = concat [[a,b] | (a, b) <- listo]

