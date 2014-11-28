
(*
   http://programmingpraxis.com/2014/11/14/dawkins-weasel/
*)

let alphabet = [|' '; 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'|]

let rng = new System.Random()

type carray = char array

let inline randomP t =
  rng.Next(100) <= t

let inline randomFromAlphabet () =
  alphabet.[rng.Next(0, (Array.length alphabet))]
  
let mutate (cs:carray) :carray =
  cs |> Array.map (fun c -> match (randomP 5) with | true -> randomFromAlphabet() | _ -> c)

let hammDistance (a:carray) (b:carray) :float =
  let matches =
    Array.zip a b
    |> Array.fold (fun acc (ac, bc) -> if (ac = bc) then acc+1 else acc) 0
  float matches / float (Array.length a)

let nextFittest (target:carray) (s:carray) :carray =
  [ for i in 1..100 do yield mutate s]
  |> List.fold (fun (r,h) c -> let d = hammDistance target c
                               if d > h then (c,d) else (r,h)) ([||],0.0)
  |> fst
  
let evolveTo (phrase:string) :string seq =
  let initial = Array.init (phrase.Length) (fun n -> randomFromAlphabet() )
  let target  = phrase.ToCharArray()
  let rec aux (gen:carray) =
    let gens = new System.String(gen)
    if (gens = phrase) then Seq.empty
    else seq { yield gens
               yield! (aux (nextFittest target gen)) }
  aux initial
  
evolveTo "METHINKS IT IS LIKE A WEASEL"
|> Seq.iteri (fun i s -> printfn "%d: %s" i s)
