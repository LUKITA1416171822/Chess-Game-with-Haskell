type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location| B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])
setBoard = (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
stringa="   a    b    c    d    e    f    g    h"
toString (P (_,_)) = "P"
toString (N (_,_)) = "N"
toString (K (_,_)) = "K"
toString (Q (_,_)) = "Q"
toString (R (_,_)) = "R"
toString (B (_,_)) = "B"
find [] [] _ _ ="  "
find [] ((N (k1,n2)) : t2) c3 n = if  (n2==n)&&(c3==k1) then toString (N (k1,n2))++"B" else find [] t2 c3 n 
find [] ((K (k1,n2)) : t2) c3 n=  if  (n2==n)&&(c3==k1) then toString (K (k1,n2))++"B" else find [] t2 c3 n
find [] ((Q (k1,n2)) : t2) c3 n=  if  (n2==n)&&(c3==k1) then toString (Q (k1,n2))++"B" else find [] t2 c3 n
find [] ((R (k1,n2)) : t2) c3 n=  if  (n2==n)&&(c3==k1) then toString (R (k1,n2))++"B" else find [] t2 c3 n
find [] ((P (k1,n2)) : t2) c3 n=  if  (n2==n)&&(c3==k1) then toString (P (k1,n2))++"B" else find [] t2 c3 n
find [] ((B (k1,n2)) : t2) c3 n=  if  (n2==n)&&(c3==k1) then toString (B (k1,n2))++"B" else find [] t2 c3 n
find ((P (k,n1)) : t1) [] c3 n = if (n==n1)&&(k==c3) then toString (P (k,n1))++"W" else find t1 [] c3 n
find ((K (k,n1)) : t1) [] c3 n = if (n==n1)&&(k==c3) then toString (K (k,n1))++"W" else find t1 [] c3 n
find ((Q (k,n1)) : t1) [] c3 n = if (n==n1)&&(k==c3) then toString (Q (k,n1))++"W" else find t1 [] c3 n
find ((R (k,n1)) : t1) [] c3 n = if (n==n1)&&(k==c3) then toString (R (k,n1))++"W" else find t1 [] c3 n
find ((P (k,n1)) : t1) [] c3 n = if (n==n1)&&(k==c3) then toString (P (k,n1))++"W" else find t1 [] c3 n
find ((B (k,n1)) : t1) [] c3 n = if (n==n1)&&(k==c3) then toString (B (k,n1))++"W" else find t1 [] c3 n
find ((P (k,n1)) : t1) ((N (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (P (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (N (k,n1))++"B" else find t1 t2 c3 n 
find ((P (k,n1)) : t1) ((K (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (P (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (K (k,n1))++"B" else find t1 t2 c3 n   
find ((P (k,n1)) : t1) ((Q (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (P (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (Q (k,n1))++"B" else find t1 t2 c3 n   
find ((P (k,n1)) : t1) ((R (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (P (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (R (k,n1))++"B" else find t1 t2 c3 n   
find ((P (k,n1)) : t1) ((B (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (P (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (B (k,n1))++"B" else find t1 t2 c3 n   
find ((P (k,n1)) : t1) ((P (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (P (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (P (k,n1))++"B" else find t1 t2 c3 n 
find ((N (k,n1)) : t1) ((N (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (N (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (N (k,n1))++"B" else find t1 t2 c3 n 
find ((N (k,n1)) : t1) ((K (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (N (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (K (k,n1))++"B" else find t1 t2 c3 n   
find ((N (k,n1)) : t1) ((Q (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (N (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (Q (k,n1))++"B" else find t1 t2 c3 n   
find ((N (k,n1)) : t1) ((R (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (N (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (R (k,n1))++"B" else find t1 t2 c3 n   
find ((N (k,n1)) : t1) ((B (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (N (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (B (k,n1))++"B" else find t1 t2 c3 n   
find ((N (k,n1)) : t1) ((P (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (N (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (P (k,n1))++"B" else find t1 t2 c3 n  
find ((R (k,n1)) : t1) ((N (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (R (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (N (k,n1))++"B" else find t1 t2 c3 n 
find ((R (k,n1)) : t1) ((K (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (R (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (K (k,n1))++"B" else find t1 t2 c3 n   
find ((R (k,n1)) : t1) ((Q (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (R (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (Q (k,n1))++"B" else find t1 t2 c3 n   
find ((R (k,n1)) : t1) ((R (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (R (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (R (k,n1))++"B" else find t1 t2 c3 n   
find ((R (k,n1)) : t1) ((B (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (R (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (B (k,n1))++"B" else find t1 t2 c3 n   
find ((R (k,n1)) : t1) ((P (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (R (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (P (k,n1))++"B" else find t1 t2 c3 n     
find ((K (k,n1)) : t1) ((N (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (K (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (N (k,n1))++"B" else find t1 t2 c3 n 
find ((K (k,n1)) : t1) ((K (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (K (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (K (k,n1))++"B"else find t1 t2 c3 n   
find ((K (k,n1)) : t1) ((Q (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (K (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (Q (k,n1))++"B" else find t1 t2 c3 n   
find ((K (k,n1)) : t1) ((R (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (K (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (R (k,n1))++"B" else find t1 t2 c3 n   
find ((K (k,n1)) : t1) ((B (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (K (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (B (k,n1))++"B" else find t1 t2 c3 n   
find ((K (k,n1)) : t1) ((P (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (K (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (P (k,n1))++"B" else find t1 t2 c3 n   
find ((Q (k,n1)) : t1) ((N (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (Q (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (N (k,n1))++"B" else find t1 t2 c3 n 
find ((Q (k,n1)) : t1) ((K (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (Q (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (K (k,n1))++"B" else find t1 t2 c3 n   
find ((Q (k,n1)) : t1) ((Q (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (Q (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (Q (k,n1))++"B" else find t1 t2 c3 n   
find ((Q (k,n1)) : t1) ((R (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (Q (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (R (k,n1))++"B" else find t1 t2 c3 n   
find ((Q (k,n1)) : t1) ((B (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (Q (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (B (k,n1))++"B" else find t1 t2 c3 n   
find ((Q (k,n1)) : t1) ((P (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (Q (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (P (k,n1))++"B" else find t1 t2 c3 n  
find ((B (k,n1)) : t1) ((N (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (B (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (N (k,n1))++"B" else find t1 t2 c3 n 
find ((B (k,n1)) : t1) ((K (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (B (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (K (k,n1))++"B" else find t1 t2 c3 n   
find ((B (k,n1)) : t1) ((Q (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (B (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (Q (k,n1))++"B" else find t1 t2 c3 n   
find ((B (k,n1)) : t1) ((R (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (B (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (R (k,n1))++"B" else find t1 t2 c3 n   
find ((B (k,n1)) : t1) ((B (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (B (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (B (k,n1))++"B" else find t1 t2 c3 n   
find ((B (k,n1)) : t1) ((P (k1,n2)) : t2) c3 n = if (n==n1)&&(k==c3) then toString (B (k,n1))++"W" else if  (n2==n)&&(c3==k1) then toString (P (k,n1))++"B" else find t1 t2 c3 n  
visualizeBoard:: Board->String
visualizeBoard (White, p1, p2)= stringa++"\n"++disRow p1 p2 8++"Turn: "++(show White)
visualizeBoard (Black, p1, p2)= stringa++"\n"++disRow p1 p2 8++"Turn: "++(show Black)
disRow _ _ 0="\n"
disRow p1 p2 n =(show n)++"| "++find p1 p2 'a' n ++" |"++" "++find p1 p2 'b' n ++" |"++" "++find p1 p2 'c' n ++" |"++" "++find p1 p2 'd' n++" |"++" "++find p1 p2 'e' n ++" |"++" "++find p1 p2 'f' n++" |"++" "++find p1 p2 'g' n ++" |"++" "++find p1 p2 'h' n ++" |"++"\n"++disRow p1 p2 (n-1)
--then if (n1==n) then toString (c1 (k, n1)) ++"W" else if (k1==c3) then if (n2==n) then toString (c2 (k1,n2)) ++"B" else find t1 t2 c3 n





withinBoard (x2,n2) = (x2=='a' || x2=='b' || x2=='c' || x2=='d' || x2=='e' || x2=='f' || x2=='g' || x2=='h') && n2>=1 && n2<=8	
validBishop 0 x1 x2= x1==x2
validBishop n x1 x2= validBishop (n-1) (succ x1) x2
--isOccupied (B (x1,n1)) (p,(wh:wt),(bh:bt)) (x2,n2) =True
isFree :: Location ->[Piece]-> Bool
isFree (x1,n1) [B (x2,n2)]= not ((x1==x2)&&(n1==n2))
isFree (x1,n1) ((B (x2,n2)):t) | x1==x2 && n1==n2 =False
								  | otherwise= isFree (x1,n1) t
isFree (x1,n1) [P (x2,n2)]= not ((x1==x2)&&(n1==n2))
isFree (x1,n1) ((P (x2,n2)):t) | x1==x2 && n1==n2 =False
								  | otherwise= isFree (x1,n1) t
								  
isFree (x1,n1) [N (x2,n2)]= not((x1==x2)&&(n1==n2))
isFree (x1,n1) ((N (x2,n2)):t) | x1==x2 && n1==n2 =False
								  | otherwise= isFree (x1,n1) t
isFree (x1,n1) [Q (x2,n2)]= not ((x1==x2)&&(n1==n2))
isFree (x1,n1) ((Q (x2,n2)):t) | x1==x2 && n1==n2 =False
								  | otherwise= isFree (x1,n1) t
isFree (x1,n1) []= True
isFree (x1,n1) ((K (x2,n2)):t) | x1==x2 && n1==n2 =False
								  | otherwise= isFree (x1,n1) t
isFree (x1,n1) [R (x2,n2)]= not ((x1==x2)&&(n1==n2))
isFree (x1,n1) ((R (x2,n2)):t) | x1==x2 && n1==n2 =False
								  | otherwise= isFree (x1,n1) t

--isOccupied (K (x1,n1)) (p,(wh:wt),(bh:bt)) (x2,n2) =True
isFreePathBishopR:: Int->Piece->Location->Board->Location->Bool
isFreePathBishopR 0 (B (x1,n1)) (x2,n2)  (p,(wh:wt),(bh:bt)) (x3,n3)= (x3==x2)&&(n3==n2)
isFreePathBishopR n (B (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)= ((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt)) || (not(isFree (x1,n1) (wh:wt))&& 
															isFree (x2,n2) (wh:wt))) && 
															isFreePathBishopR (n-1) (B (x1,n1)) (( succ x2),(n2+1)) (p,(wh:wt),(bh:bt)) (x3,n3)

isFreePathBishopL:: Int->Piece->Location->Board->Location->Bool
isFreePathBishopL 0 (B (x1,n1)) (x2,n2)  (p,(wh:wt),(bh:bt)) (x3,n3)= (x3==x2)&&(n3==n2)
isFreePathBishopL n (B (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)= ((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt)) || (not(isFree (x1,n1) (wh:wt))&& 
															isFree (x2,n2) (wh:wt))) && 
															isFreePathBishopL (n-1) (B (x1,n1)) (( succ x2),(n2-1)) (p,(wh:wt),(bh:bt)) (x3,n3)
--isOccupied (B (x1,n1)) (p,(wh:wt),(bh:bt)) (x2,n2) && isOccupiedPathBishop (B (x1,n1))  (p,(wh:wt),(bh:bt)) ((succ x2),(n2+1))
isLegal:: Piece->Board->Location->Bool
--((not(isFree (x1,n1) (bh:bt))&&  (isFree (x2,n2) (bh:bt))) || (not(isFree (x1,n1) (wh:wt))&&  (isFree (x2,n2) (wh:wt)))) &&
--isLegal (B (x1,n1)) (p,(wh:wt),(bh:bt)) (x2,n2)= isOccupied (B (x1,n1)) (p,(wh:wt),(bh:bt)) (x2,n2) &&  withinBoard (x2,n2) && n2-n1>0 && isOccupiedPathBishop (B (x1,n1))  (p,(wh:wt),(bh:bt)) ((x2),(n2))



--isLegal (B (x1,n1)) (p,(wh:wt),(bh:bt)) (x2,n2) |  withinBoard (x2,n2) && n2-n1>0 =  (isFreePathBishop (B (x1,n1)) ((succ x1),(n1+1))  (p,(wh:wt),(bh:bt)) ((x2),(n2))) &&  (((validBishop (n2-n1) x1 x2)||(validBishop (n2-n1) x2 x1))) 
--												
--												|  withinBoard (x2,n2) && n1-n2>0 = (isFreePathBishop (B (x1,n1)) ((succ x2),(n2+1)) (p,(wh:wt),(bh:bt)) ((x1),(n1))) && (((validBishop (n1-n2) x2 x1)||(validBishop (n1-n2) x1 x2))) 
--												| otherwise = False
												-- &&  isFreePathBishop (B (x2,n2))  (p,(wh:wt),(bh:bt)) (x1,n1)
												
												

	

charInt s | s=='a' =1
		  | s=='b' =2
		  | s=='c' =3
		  | s=='d' =4
		  | s=='e' =5
		  | s=='f' =6
		  | s=='g' =7
		  | s=='h' =8
		  



isFreePathRookD:: Int->Piece->Location->Board->Location->Bool
isFreePathRookR:: Int->Piece->Location->Board->Location->Bool
isFreePathRookL:: Int->Piece->Location->Board->Location->Bool
isFreePathRookU:: Int->Piece->Location->Board->Location->Bool

isFreePathRookU 0 (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)= (x3==x2)&&(n3==n2)
isFreePathRookU n (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)=  ((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt)) || (not(isFree (x1,n1) (wh:wt)))) &&
																	 isFreePathRookU (n-1) (R (x1,n1)) (x2,(n2+1)) (p,(wh:wt),(bh:bt)) (x3,n3)
isFreePathRookU2 0 (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3) = (x3==x2)&&(n3==n2)&&((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt)) || ((not(isFree (x1,n1) (wh:wt)))&& isFree (x2,n2) (wh:wt)))
isFreePathRookU2 n (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3) = 
	((not(isFree (x1,n1) (wh:wt))&& isFree (x2,n2) (wh:wt) && isFree (x2,n2) (bh:bt)) ||((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt) && isFree (x2,n2) (wh:wt)))) && isFreePathRookU2 (n-1) (R (x1,n1)) (x2,(n2+1)) (p,(wh:wt),(bh:bt)) (x3,n3)



isFreePathRookD 0 (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)= (x3==x2)&&(n3==n2)
isFreePathRookD n (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3) = ((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt)) || (not(isFree (x1,n1) (wh:wt)))&& isFree (x2,n2) (wh:wt)) && isFreePathRookD (n-1) (R (x1,n1)) (x2,(n2-1)) (p,(wh:wt),(bh:bt)) (x3,n3)


isFreePathRookD2 0 (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3) = (x3==x2)&&(n3==n2)&&((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt)) || ((not(isFree (x1,n1) (wh:wt)))&& isFree (x2,n2) (wh:wt)))
isFreePathRookD2 n (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3) = 
	((not(isFree (x1,n1) (wh:wt))&& isFree (x2,n2) (wh:wt) && isFree (x2,n2) (bh:bt)) ||((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt) && isFree (x2,n2) (wh:wt)))) && isFreePathRookD2 (n-1) (R (x1,n1)) (x2,(n2-1)) (p,(wh:wt),(bh:bt)) (x3,n3)
	

isFreePathRookR 0 (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)= (x3==x2)&&(n3==n2)
isFreePathRookR n (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3) =((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt)) || (not(isFree (x1,n1) (wh:wt)))) && isFreePathRookR (n-1) (R (x1,n1)) (succ x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)

isFreePathRookR2 0 (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)= (x3==x2)&&(n3==n2)&&((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt)) || ((not(isFree (x1,n1) (wh:wt)))&& isFree (x2,n2) (wh:wt)))
isFreePathRookR2 n (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3) =
	((not(isFree (x1,n1) (wh:wt))&& isFree (x2,n2) (wh:wt) && isFree (x2,n2) (bh:bt)) ||((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt) && isFree (x2,n2) (wh:wt)))) && isFreePathRookR2 (n-1) (R (x1,n1)) (succ x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)


isFreePathRookL 0 (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)=(x3==x2)&&(n3==n2)
isFreePathRookL n (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3) =((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt)) || (not(isFree (x1,n1) (wh:wt)))) && isFreePathRookL (n-1) (R (x1,n1)) (pred x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)

isFreePathRookL2 0 (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)=(x3==x2)&&(n3==n2)&&((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt)) || ((not(isFree (x1,n1) (wh:wt)))&& isFree (x2,n2) (wh:wt)))
isFreePathRookL2 n (R (x1,n1)) (x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3) =
	((not(isFree (x1,n1) (wh:wt))&& isFree (x2,n2) (wh:wt) && isFree (x2,n2) (bh:bt)) ||((not(isFree (x1,n1) (bh:bt))&& isFree (x2,n2) (bh:bt) && isFree (x2,n2) (wh:wt)))) && isFreePathRookL2 (n-1) (R (x1,n1)) (pred x2,n2) (p,(wh:wt),(bh:bt)) (x3,n3)


isLegal (R (x1,n1)) (p,white,black) (x2,n2) | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white))&& (x1==x2 || n1==n2) && withinBoard (x2,n2) = 
												((n2>n1) && isFreePathRookU2 (n2-n1-1) (R (x1,n1)) (x1,(n1+1)) (p,white,black) (x2,n2)) || ((n1>n2) && isFreePathRookD2	 (n1-n2-1) (R (x1,n1)) (x1,(n1-1)) (p,white,black) (x2,n2))||
												((((charInt x1)-(charInt x2))>0) && isFreePathRookL2 ((charInt x1)-(charInt x2)-1) (R (x1,n1)) ((pred x1),n1) (p,white,black) (x2,n2)) ||
												((((charInt x2)-(charInt x1))>0) && isFreePathRookR2 ((charInt x2)-(charInt x1)-1) (R (x1,n1)) ((succ x1),n1) (p,white,black) (x2,n2))
	 
												| otherwise=False


isLegal (B (x1,n1)) (p,white,black) (x2,n2)  | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white))&& withinBoard (x2,n2) && n2-n1>0 =  ((isFreePathBishopR (n2-n1-1) (B (x1,n1)) ((succ x1),(n1+1))  (p,white,black) ((x2),(n2))) || 
																	(isFreePathBishopL (n2-n1-1) (B (x1,n1)) ((succ x2),(n2-1)) (p,white,black) ((x1),(n1)))) &&  (((validBishop (n2-n1) x1 x2)||(validBishop (n2-n1) x2 x1))) 
												
													  | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white))&&  withinBoard (x2,n2)&&  n1-n2>0 = ((isFreePathBishopR (n1-n2-1) (B (x1,n1)) ((succ x2),(n2+1)) (p,white,black) ((x1),(n1))) || 
																	(isFreePathBishopL (n1-n2-1) (B (x1,n1)) ((succ x1),(n1-1))  (p,white,black) ((x2),(n2)))) && (((validBishop (n1-n2) x2 x1)||(validBishop (n1-n2) x1 x2))) 
												| otherwise = False
isLegal (K (x1,n1)) (p,white,black) (x2,n2) 
									   | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) && (withinBoard (x2,n2)) && x1==x2 && n1==(n2+1) = True 
									   | ((not(isFree (x1,n1) black)&& isFree (x2,n2) white) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) && withinBoard (x2,n2) && x1==x2 && n1==(n2-1) = True
									   | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) && withinBoard (x2,n2) && x2== succ x1 && n1==n2 = True
									  
									   | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) && withinBoard (x2,n2) &&  x1== succ x2 && n1==n2 = True
									  
									   | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) && withinBoard (x2,n2) &&  x2== succ x1 && n1==n2+1 = True
									   
									   | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) && withinBoard (x2,n2) &&  x2== succ x1 && n1==n2-1 = True
									   
									   | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) && withinBoard (x2,n2) &&  x1== succ x2 && n1==n2+1 = True
									   
									   | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) && withinBoard (x2,n2) &&  x1== succ x2 && n1==n2-1 = True
									   
									   | otherwise=False

isLegal (Q (x1,n1)) (p,white,black) (x2,n2) = (isLegal (B (x1,n1)) (p,white,black) (x2,n2)) ||  (isLegal (R (x1,n1)) (p,white,black) (x2,n2))

isLegal (P (x1,n1)) (p,white,black) (x2,n2) | ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) 
													&& withinBoard (x2,n2) && x1==x2 &&
												(not(isFree (x1,n1) white)) && ((n1 == 2))&&  ((not(isFree (x1,n1) black)&& (isFree (x1,(n1+1)) black)&& (isFree (x1,(n1+1)) white)) || (not(isFree (x1,n1) white)&& (isFree (x1,(n1+1)) white)&& (isFree (x1,(n1+1)) black))) && x1==x2 
												&& (n2-n1==2) = True
												| (not(isFree (x1,n1) white)) && ((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) 
													&& withinBoard (x2,n2) && x1==x2 &&
												((n1 == 2))&&  ((not(isFree (x1,n1) black)&& (isFree (x1,(n1+1)) black)) || (not(isFree (x1,n1) white)&& (isFree (x1,(n1+1)) white))) && x1==x2 
												&& (n2-n1==1)= True
												| (not(isFree (x1,n1) black)) &&((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) 
												&& withinBoard (x2,n2) && x1==x2 && ((not(isFree (x1,n1) black)&& (isFree (x1,(n1-1)) black)&& (isFree (x1,(n1-1)) white)) || (not(isFree (x1,n1) white)&& (isFree (x1,(n1+1)) white)&& (isFree (x1,(n1+1)) black))) && (n1==7) && (n1-n2 ==2) = True
												| (not(isFree (x1,n1) black)) &&((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) 
												&& withinBoard (x2,n2) && x1==x2 && (((not(isFree (x1,n1) black)&& isFree (x1,(n1-1)) black) || (not(isFree (x1,n1) white)&& isFree (x1,(n1-1)) white)) && (n1==7)) && (n1-n2 ==1) = True
												
												| (isFree (x2,n2) black && isFree (x2,n2) white) && withinBoard (x2,n2) && x1==x2 && n1-n2 ==1 && (not(isFree (x1,n1) black)) = True
												| (isFree (x2,n2) black && isFree (x2,n2) white) && withinBoard (x2,n2) && x1==x2 && n2-n1 ==1 && (not(isFree (x1,n1) white)) = True
												| (not(isFree (x1,n1) white))&&( (x2== (succ x1)) && (n2==(n1+1)))	= ((not(isFree (x1,n1) black)&&(not(isFree (x2,n2) white)))|| (not(isFree (x2,n2) black)&&(not(isFree (x1,n1) white))) )
												| (not(isFree (x1,n1) white))&&( (x2== (pred x1)) && (n2==(n1+1)))	= ((not(isFree (x1,n1) black)&&(not(isFree (x2,n2) white)))|| (not(isFree (x2,n2) black)&&(not(isFree (x1,n1) white))) )
												| (not(isFree (x1,n1) black))&&( (x2== (succ x1)) && (n2==(n1-1)))	= ((not(isFree (x1,n1) black)&&(not(isFree (x2,n2) white)))|| (not(isFree (x2,n2) black)&&(not(isFree (x1,n1) white))) )
												| (not(isFree (x1,n1) black))&&( (x2== (pred x1)) && (n2==(n1-1)))	= ((not(isFree (x1,n1) black)&&(not(isFree (x2,n2) white)))|| (not(isFree (x2,n2) black)&&(not(isFree (x1,n1) white))) )
												
												| otherwise= False	
isLegal (N (x1,n1)) (p,white,black) (x2,n2)  | 	 withinBoard (x2,n2)=((not(isFree (x1,n1) black)&& isFree (x2,n2) black) || (not(isFree (x1,n1) white)&& isFree (x2,n2) white)) && (((x2== succ(succ x1)) && n1==n2+1)||((x1== succ(succ x2)) && n1==n2+1)||((x1== succ(succ x2)) && n1==n2-1)||((x2== succ(succ x1)) && n1==n2-1)||((x1== succ x2) && n1==n2+2)|| ((x2==succ x1) && (n1==n2+2))||((x1== succ x2) && n1==n2-2)|| ((x2==succ x1) && (n1==n2-2)))
											 | otherwise = False
												


--not (isFree (x1,n1) white) &&  (isFree (x2,n2) white)


	
suggestMove :: Piece -> Board -> [Location]
suggestMove (p) board = 
	filter (isLegal (p) board) [(y,x) | x<-[1,2,3,4,5,6,7,8],y<-['a','b','c','d','e','f','g','h']]		 									

existLocation :: Location -> [Location] -> Bool
existLocation _ [] = False
existLocation (c1,n1) ((c2,n2):t) = 
	if c1==c2 && n1==n2 then True 
	else existLocation (c1,n1) t

existPiece :: Piece -> [Piece] -> Bool
existPiece _ [] = False
existPiece piece1 (piece2:t) = 
	if piece1==piece2 then True 
	else existPiece piece1 t
	
existLocPieces :: Location -> [Piece] -> Bool
existLocPieces _ [] = False
existLocPieces (c1,n1) ((P (c2,n2)):t) = 
	if c1==c2 && n1==n2 then True else existLocPieces (c1,n1) t
existLocPieces (c1,n1) ((B (c2,n2)):t) = 
	if c1==c2 && n1==n2 then True else existLocPieces (c1,n1) t
existLocPieces (c1,n1) ((Q (c2,n2)):t) = 
	if c1==c2 && n1==n2 then True else existLocPieces (c1,n1) t
existLocPieces (c1,n1) ((K (c2,n2)):t) = 
	if c1==c2 && n1==n2 then True else existLocPieces (c1,n1) t
existLocPieces (c1,n1) ((R (c2,n2)):t) = 
	if c1==c2 && n1==n2 then True else existLocPieces (c1,n1) t
existLocPieces (c1,n1) ((N (c2,n2)):t) = 
	if c1==c2 && n1==n2 then True else existLocPieces (c1,n1) t
put :: Piece -> Piece -> [Piece] -> [Piece]
put piece1 piece2 (piece:t) = 
	if piece1==piece then piece2:t
	else piece:put piece1 piece2 t
remov :: Location -> [Piece] -> [Piece]
remov (c1,n1) ((P (c2,n2)):t) = 
	if c1==c2 && n1==n2 then t
	else (P (c2,n2)):(remov (c1,n1) t)
remov (c1,n1) ((N (c2,n2)):t) = 
	if c1==c2 && n1==n2 then t
	else (N (c2,n2)):(remov (c1,n1) t)
remov (c1,n1) ((Q (c2,n2)):t) = 
	if c1==c2 && n1==n2 then t
	else (Q (c2,n2)):(remov (c1,n1) t)
remov (c1,n1) ((B (c2,n2)):t) = 
	if c1==c2 && n1==n2 then t
	else (B (c2,n2)):(remov (c1,n1) t)
remov (c1,n1) ((K (c2,n2)):t) = 
	if c1==c2 && n1==n2 then t
	else (K(c2,n2)):(remov (c1,n1) t)
remov (c1,n1) ((R (c2,n2)):t) = 
	if c1==c2 && n1==n2 then t
	else (R (c2,n2)):(remov (c1,n1) t)

move :: Piece -> Location -> Board -> Board
move (P (c1,n1)) (c2,n2) (White,l1,l2) = 
	if existPiece (P (c1,n1)) l2 then error "This is White player's turn, Black can't move."
	else if not(existPiece (P (c1,n1)) l1) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (P (c1,n1)) (White,l1,l2)) && existLocPieces (c2,n2) l2
	then (Black,put (P (c1,n1)) (P (c2,n2)) l1,remov (c2,n2) l2)
	else if existLocation (c2,n2) (suggestMove (P (c1,n1)) (White,l1,l2))
	then (Black,put (P (c1,n1)) (P (c2,n2)) l1,l2)
	else error ("Illegal move for piece "++show (P (c1,n1)))
	
move (P (c1,n1)) (c2,n2) (Black,l1,l2) = 
	if existPiece (P (c1,n1)) l1 then error "This is Black player's turn, White can't move."
	else if not(existPiece (P (c1,n1)) l2) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (P (c1,n1)) (Black,l1,l2)) && existLocPieces (c2,n2) l1
	then (White,remov (c2,n2) l1,put (P (c1,n1)) (P (c2,n2)) l2)
	else if existLocation (c2,n2) (suggestMove (P (c1,n1)) (Black,l1,l2))
	then (White,l1,put (P (c1,n1)) (P (c2,n2)) l2)
	else error ("Illegal move for piece "++show (P (c1,n1)))
	
move (N (c1,n1)) (c2,n2) (White,l1,l2) = 
	if existPiece (N (c1,n1)) l2 then error "This is White player's turn, Black can't move."
	else if not(existPiece (N (c1,n1)) l1) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (N (c1,n1)) (White,l1,l2)) && existLocPieces (c2,n2) l2
	then (Black,put (N (c1,n1)) (N (c2,n2)) l1,remov (c2,n2) l2)
	else if existLocation (c2,n2) (suggestMove (N (c1,n1)) (White,l1,l2))
	then (Black,put (N (c1,n1)) (N (c2,n2)) l1,l2)
	else error ("Illegal move for piece "++show (N (c1,n1)))
	
move (N (c1,n1)) (c2,n2) (Black,l1,l2) = 
	if existPiece (N (c1,n1)) l1 then error "This is Black player's turn, White can't move."
	else if not(existPiece (N (c1,n1)) l2) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (N (c1,n1)) (Black,l1,l2)) && existLocPieces (c2,n2) l1
	then (White,remov (c2,n2) l1,put (N (c1,n1)) (N (c2,n2)) l2)
	else if existLocation (c2,n2) (suggestMove (N (c1,n1)) (Black,l1,l2))
	then (White,l1,put (N (c1,n1)) (N (c2,n2)) l2)
	else error ("Illegal move for piece "++show (N (c1,n1)))
	
move (B (c1,n1)) (c2,n2) (White,l1,l2) = 
	if existPiece (B (c1,n1)) l2 then error "This is White player's turn, Black can't move."
	else if not(existPiece (B (c1,n1)) l1) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (B (c1,n1)) (White,l1,l2)) && existLocPieces (c2,n2) l2
	then (Black,put (B (c1,n1)) (B (c2,n2)) l1,remov (c2,n2) l2)
	else if existLocation (c2,n2) (suggestMove (B (c1,n1)) (White,l1,l2))
	then (Black,put (B (c1,n1)) (B (c2,n2)) l1,l2)
	else error ("Illegal move for piece "++show (B (c1,n1)))
	
move (B (c1,n1)) (c2,n2) (Black,l1,l2) = 
	if existPiece (B (c1,n1)) l1 then error "This is Black player's turn, White can't move."
	else if not(existPiece (B (c1,n1)) l2) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (B (c1,n1)) (Black,l1,l2)) && existLocPieces (c2,n2) l1
	then (White,remov (c2,n2) l1,put (B (c1,n1)) (B (c2,n2)) l2)
	else if existLocation (c2,n2) (suggestMove (B (c1,n1)) (Black,l1,l2))
	then (White,l1,put (B (c1,n1)) (B (c2,n2)) l2)
	else error ("Illegal move for piece "++show (B (c1,n1)))

move (R (c1,n1)) (c2,n2) (White,l1,l2) = 
	if existPiece (R (c1,n1)) l2 then error "This is White player's turn, Black can't move."
	else if not(existPiece (R (c1,n1)) l1) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (R (c1,n1)) (White,l1,l2)) && existLocPieces (c2,n2) l2
	then (Black,put (R (c1,n1)) (R (c2,n2)) l1,remov (c2,n2) l2)
	else if existLocation (c2,n2) (suggestMove (R (c1,n1)) (White,l1,l2))
	then (Black,put (R (c1,n1)) (R (c2,n2)) l1,l2)
	else error ("Illegal move for piece "++show (R (c1,n1)))
	
move (R (c1,n1)) (c2,n2) (Black,l1,l2) = 
	if existPiece (R (c1,n1)) l1 then error "This is Black player's turn, White can't move."
	else if not(existPiece (R (c1,n1)) l2) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (R (c1,n1)) (Black,l1,l2)) && existLocPieces (c2,n2) l1
	then (White,remov (c2,n2) l1,put (R (c1,n1)) (R (c2,n2)) l2)
	else if existLocation (c2,n2) (suggestMove (R (c1,n1)) (Black,l1,l2))
	then (White,l1,put (R (c1,n1)) (R (c2,n2)) l2)
	else error ("Illegal move for piece "++show (R (c1,n1)))
	
move (Q (c1,n1)) (c2,n2) (White,l1,l2) = 
	if existPiece (Q (c1,n1)) l2 then error "This is White player's turn, Black can't move."
	else if not(existPiece (Q (c1,n1)) l1) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (Q (c1,n1)) (White,l1,l2)) && existLocPieces (c2,n2) l2
	then (Black,put (Q (c1,n1)) (Q (c2,n2)) l1,remov (c2,n2) l2)
	else if existLocation (c2,n2) (suggestMove (Q (c1,n1)) (White,l1,l2))
	then (Black,put (Q (c1,n1)) (Q (c2,n2)) l1,l2)
	else error ("Illegal move for piece "++show (Q (c1,n1)))
	
move (Q (c1,n1)) (c2,n2) (Black,l1,l2) = 
	if existPiece (Q (c1,n1)) l1 then error "This is Black player's turn, White can't move."
	else if not(existPiece (Q (c1,n1)) l2) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (Q (c1,n1)) (Black,l1,l2)) && existLocPieces (c2,n2) l1
	then (White,remov (c2,n2) l1,put (Q (c1,n1)) (Q (c2,n2)) l2)
	else if existLocation (c2,n2) (suggestMove (Q (c1,n1)) (Black,l1,l2))
	then (White,l1,put (Q (c1,n1)) (Q (c2,n2)) l2)
	else error ("Illegal move for piece "++show (Q (c1,n1)))
	
move (K (c1,n1)) (c2,n2) (White,l1,l2) = 
	if existPiece (K (c1,n1)) l2 then error "This is White player's turn, Black can't move."
	else if not(existPiece (K (c1,n1)) l1) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (K (c1,n1)) (White,l1,l2)) && existLocPieces (c2,n2) l2
	then (Black,put (K (c1,n1)) (K (c2,n2)) l1,remov (c2,n2) l2)
	else if existLocation (c2,n2) (suggestMove (K (c1,n1)) (White,l1,l2))
	then (Black,put (K (c1,n1)) (K (c2,n2)) l1,l2)
	else error ("Illegal move for piece "++show (K (c1,n1)))
	
move (K (c1,n1)) (c2,n2) (Black,l1,l2) = 
	if existPiece (K (c1,n1)) l1 then error "This is Black player's turn, White can't move."
	else if not(existPiece (K (c1,n1)) l2) then error "Piece doesn't exist."
	else if existLocation (c2,n2) (suggestMove (K (c1,n1)) (Black,l1,l2)) && existLocPieces (c2,n2) l1
	then (White,remov (c2,n2) l1,put (K (c1,n1)) (K (c2,n2)) l2)
	else if existLocation (c2,n2) (suggestMove (K (c1,n1)) (Black,l1,l2))
	then (White,l1,put (K (c1,n1)) (K (c2,n2)) l2)
	else error ("Illegal move for piece "++show (K (c1,n1)))
	