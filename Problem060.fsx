(*
 *  Prime pair sets
 *
 *  The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating
 *  them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and
 *  1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four
 *  primes with this property.
 *
 *  Find the lowest sum for a set of five primes for which any two primes concatenate to produce
 *  another prime.
 *)

open System
open System.Collections.Generic

module Problem060 =
    type Primo () =
        let knownPrimes = new SortedSet<int>()

        let mutable lastValue = 0

        let testPrime (n : int) =
            let max = Math.Sqrt(float n) |> int
            let rec test (e : IEnumerator<int>) =
                if e.MoveNext() then
                    let p = e.Current
                    if p > max then
                        true
                    elif n % p = 0 then
                        false
                    else
                        test e
                else
                    true
            test (knownPrimes.GetEnumerator())

        member this.Next
            with get () =
                let rec next cur =
                    let n = cur + 1
                    match testPrime n with
                    | true -> knownPrimes.Add n |> ignore
                              n
                    | _ -> next n
                match knownPrimes.Count with
                | 0 -> knownPrimes.Add 2 |> ignore
                       2
                | _ -> next knownPrimes.Max

        member this.IsPrime n =
            match n with
            | x when x > knownPrimes.Max -> false
            | _ -> knownPrimes.Contains(n)

    let splits n =
        let rec split r = seq {
            let nr = r * 10
            if nr < n then
                match n % nr with
                | x when x < r -> ()
                | x -> yield (n / nr, x)
                yield! split nr
        }
        split 1


    let primo = new Primo()
    let rec proov () =
        match primo.Next with
        | x when x > 10000 -> ()
        | x -> ()

    [ 0 .. 9 ] |> Seq.iter (fun _ -> printfn "%d" primo.Next)
    printfn "2 is prime: %b" (primo.IsPrime 2)
    printfn "3 is prime: %b" (primo.IsPrime 3)

    splits 1709 |> Seq.iter (fun (x, y) -> printfn "(%d, %d)" x y)

    ()
