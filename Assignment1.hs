-- CISC 260 (Programming Paradigms) : Assignment 1
-- Code written by Alejandra Kudo


-- Problem 1: Write a function called shape that takes three parameters, 
--the lengths of the three axis of an ellipsoid, and returns a character describing the kind of ellipsoid it is: ‘S’ for a sphere, 
-- ‘F’ for a football, ‘E’ for an ellipsoid which is not a sphere or football, and ‘X’ if any of the parameters are equal to or less than zero.
-- The parameters for this function should all be Doubles and the result should be a Char.
shape :: Double -> Double -> Double -> Char
shape  a b c 
	| (a == b)  && (b ==c) = 'S'
	| (a <= 0.0) || (b <= 0) || (c <= 0.0) = 'X'
	| (a == b) || (b == c) || (a==c) = 'F'
	| (a /= b) && (b/= c) && (a/=c) = 'E'
	


-- Problem 2: If the lengths of the three axes of an ellipsoid are a, b, and c (as in the diagram for Problem 1),
-- thevolumeoftheellipsoidis4/3**a*b*c. Inthecaseofasphere,whereaandbandcareall equal to the same value r, 
--this leads to the familiar formula 4/3 r3 for the volume of a sphere.
--Write a Haskell function called volume which takes three parameters of type Double (the lengths of the three axes) 
-- and returns a result of type Double (the volume of an ellipsoid with those axes).
volume :: Double -> Double -> Double -> Double
volume a b c 
	| (a < 0) || (b < 0) || (c <0) = error "Negative value"
	| (a == 0) || (b==0) || (c==0) = 0
	| (a == b) && (b== c) = (4/3)*pi*(a^3) 
	| otherwise = (4 /3)*pi*a*b*c


-- Problem 3:  Write a function called logSum that takes two Int parameters a and b. 
-- Its value must be the sums of the natural logarithms of all the numbers between a and b inclusive. 
--In other words, the valuemustbeloga+log(a+1)+log(a+2)+log(a+3)+...+log(b-1)+logb. 
--Ifa==b,thevalue should be log a. If a > b, the value should be zero 
--(since there are no integers between a and b in this case). The result of logSum should be a Double.
--To compute natural logarithms, use the Haskell log function.
-- USE RECURSION
-- fromIntegral :: (Num a, Integral b) => a -> b
logSum :: Int -> Int -> Double 
logSum a b 
	| a == b = log (fromIntegral a )
	| a > b = 0 
	| otherwise = logSum a (b-1) + log (fromIntegral b) 



-- Problem 4: Astronauts have discovered a strange new kind of life form that grows in isolated spots on some other planets in the solar system. They’ve named these beings “ET”s and have brought some back to earth to study. Scientists have concluded that ETs have peculiar grown patterns that seem to obey the following rules in Earth gravity:
-- If the mass of an ET is less than 1 gram its mass will double in one day. For example, if an ET weighs 0.75 grams today it will weight 1.5 grams tomorrow.
--If the mass of an ET is at least 1 gram but less than 20 grams its mass after one day will be 1.5 times the starting mass plus 2 grams. For example, an ET that weighs 2 grams today will weigh 5 grams tomorrow (1.5 *2 + 2)
--If the mass of an ET is at least 20 grams but less than 100 grams its mass after one day will be 1.2 times its starting mass plus 1 gram. For example, an ET that weighs 50 grams today will weigh 61 grams tomorrow (50*1.2 + 1)
--If the mass of an ET is 100 grams or more its mass will be 1.1 times its starting mass plus 0.5 grams. For example, an ET that weighs 200 grams today will weigh 220.5 grams tomorrow (200*1.1 + 0.5).
--The scientists aren’t very good at programming and have asked you to provide them with a Haskell function that will predict the growth of an ET in a given number of days. This function should be called growET and should take two parameters (in the order given):
--The initial mass of the ET in grams (a Double)
--The number of days the ET will be allowed to grow (an Int)
--It should return the predicted mass of the ET in grams (a Double) at the end of the growth period.

growET :: Double -> Int -> Double 
growET x n
	| x < 0 = error "negative value"
	| (x > 0) && (n < 0) = error "negative days doesn't exist"
	| (x > 0) && (n > 100) = error "ET Plant doesnt survive past 100 days"
	| n == 0 = x
	| x < 1 = growET (x*2) (n-1)
	| (x >= 1) && (x < 20) = growET(x*1.5 + 2) (n-1)
	| (x >= 20) && (x < 100) = growET(x*1.2 + 1) (n-1)
	| (x >= 100) = growET(x*1.1 + 0.5) (n-1)
