# Important Factor: Survival

# General Linear Mixed Model for survival (fixed + random factors)
glm(survival ~ substrate * wavebreaker)
Random factor: plot, square (ignore for now)
Check for overdispersion
Influence: substrate (3 levels), wavebreaker (2 levels)


Plot #	| Square			                	| Substrate	| Wavebreaker
------------------------------------------------------------------------------------------------------
1		    |				                       	      | BARE		| N
2		    | G2, Y1			                      	| NET		  | N
3		    |			                      	      	| BARE		| N
4		    |				                      	      | BARE		| N
5		    |					                            | BARE		| N
6		    | G2, R1, R2, R3, Y1, Y2, Y3		      | BARE		| N
7		    |					                            | BARE		| N
8		    | G1, G2, G3, R2, R3, Y1, Y3		      | NET		  | N
9		    | G2, G3, R2, Y1, Y2, Y3		          | BARE 		| Y
10		  |					                            | BARE		| Y
11		  | R1, R2, R3, Y3		                	| NET	  	| Y
12		  |				                      	      | BARE		| Y
13		  |				                             	| BARE		| Y
14		  |				                      	      | NET	  	| Y
15		  |				                           	  | OYSTER	| Y
16		  |					                            | OYSTER	| Y
17		  |					                            | OYSTER	| Y
18		  | G1, G2, R2, R3, Y1, Y3	          	| OYSTER	| Y
19		  |			                             		| OYSTER	| N
20		  | G1, G3, R2			            	      | OYSTER	| N
21		  |				                            	| OYSTER	| N
22		  |				                            	| OYSTER	| N
23		  | G1, G2, G3, R1, R2, R3, Y1, Y2, Y3	| BARE		| Y
24		  |					                            | BARE		| Y
25	  	| G1, G2, G3, R1, R2, R3, Y1, Y2, Y3	| NET		  | Y
26		  | G1, G2, G3, R1, R2, R3, Y1, Y2, Y3	| BARE		| Y
27		  |					                            | BARE		| Y
28		  | G1, G2, R1, R2, R3, Y1, Y3		      | NET		  | Y
29		  |					                            | BARE		| N
30		  |				                            	| BARE		| N
31		  | G1, G2, G3, R1, R2, R3, Y1, Y2, Y3	| NET	  	| N
32	  	| G1, G3, R1, R2, R3, Y1, Y2		      | BARE		| N
33		  |					                            | NET	  	| N
34		  |				                            	| BARE		| N
35		  | G1, G2, G3, R1, R2, R3, Y1, Y2, Y3	| BARE		| N
36		  |					                            | BARE		| N



------------------------------------------------------------------------------------------------------------
Substrate	| Wavebreaker	| Sampled		| Partially Sampled	| To Sample
------------------------------------------------------------------------------------------------------------
Bare		  | Y		        | 23, 26		| 9 [6]		         	| 10, 12, 13, 24, 27
Bare		  | N		        | 35			  | 6 [7], 32 [7] 	  | 1, 3, 4, 5, 7, 29, 30, 34, 36
Net		    | Y		        | 25			  | 11 [4], 28 [7]	  | 14
Net 		  | N		        | 31		  	| 2 [2], 8 [7]		  | 33
Oyster		| Y		        |			      | 18 [6]		        | 15, 16, 17
Oyster		| N		        |			      | 20 [3]		        | 19, 21, 22


HIGH PRIORITY: 18, 20


Monday 9AM:    Bake crucibles (100)
	             Weigh samples from last week
Monday 10AM:   Prepare samples (2 samples)
Monday 11AM:   Weigh crucibles 
Monday 1PM:    Place samples into oven
	             Weigh samples from last week again

Tuesday 9AM:   Ash samples from last week
Tuesday 1PM:   Weigh ashed samples from last week

Wednesday 1PM: Weigh samples
Wednesday 4PM: Weigh samples again

Thursday 9AM:  Ash samples
	             Bake crucibles (100)
Thursday 11AM: Weigh crucibles
Thursday 1PM:  Weigh ashed samples
Thursday 2PM:  Prepare samples (2 samples)
Thursday 4PM:  Place samples into oven



