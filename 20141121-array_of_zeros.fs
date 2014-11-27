open System.Diagnostics

(*
   http://programmingpraxis.com/2014/11/21/an-array-of-zeroes/
   
   You are given an array of integers. Write a program that moves all non-zero integers to the left end of the array, and all zeroes to the right end of the array. Your program should operate in place. The order of the non-zero integers doesn’t matter. As an example, given the input array [1,0,2,0,0,3,4], your program should permute the array to [1,4,2,3,0,0,0] or something similar, and return the value 4.
*)
   
let shuffle (ns:int array) =
  let rec aux = function
    | (i, j) when i >= j -> ns
    | (i, j)             -> if ns.[i] = 0 then
                              let t = ns.[i]
                              ns.[i] <- ns.[j]
                              ns.[j] <- t
                              aux (i+1, j+1)
                            else aux (i+1, j)
  aux (0, (Array.length ns)-1)

Debug.Assert(
  (shuffle [|1;0;2;0;3;0;4|]).[3..] = [|0;0;0|])

Debug.Assert(
  (shuffle [||]) = [||])

Debug.Assert(
  (shuffle [|1;2;3;4;0;0;0|]).[3..] = [|0;0;0|])
