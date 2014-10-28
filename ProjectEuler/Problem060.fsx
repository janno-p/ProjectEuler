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

    let knownPrimes = new SortedSet<int>();

    let primeSeq = seq {
        let rec checkIfPrime (ps: IEnumerator<int>) value sqrtValue =
            if ps.MoveNext() then
                match ps.Current with
                | n when n >= sqrtValue -> true
                | n when value % n = 0 -> false
                | _ -> checkIfPrime ps value sqrtValue
            else true
        knownPrimes.Add(2) |> ignore
        yield 2
        knownPrimes.Add(3) |> ignore
        yield 3
        let lastValue = ref 3
        while true do
            lastValue := !lastValue + 2
            if checkIfPrime (knownPrimes.GetEnumerator()) !lastValue ((Math.Sqrt(float !lastValue) |> int) + 1) then
                knownPrimes.Add(!lastValue) |> ignore
                yield !lastValue
    }

    let splits prime =
        let rec split rank = seq {
            let nr = rank * 10
            if nr < prime then
                match prime % nr with
                | x when x < rank -> ()
                | x -> let t = prime / nr
                       if knownPrimes.Contains(t) && knownPrimes.Contains(x) then
                            yield (prime / nr, x)
                yield! split nr
        }
        split 1 |> Seq.toArray

    let addToPrimeSet (pset: Dictionary<'T,SortedSet<int>>) key value =
        match pset.TryGetValue key with
        | true, vset -> vset.Add(value) |> ignore
        | _ -> pset.[key] <- new SortedSet<int>([value])

    let primeSets1 = Dictionary<int, SortedSet<int>>()
    let primeSets2 = Dictionary<int * int, SortedSet<int>>()
    let primeSets3 = Dictionary<int * int * int, SortedSet<int>>()
    let primeSets4 = Dictionary<int * int * int * int, SortedSet<int>>()

    let addPrimeSet1 a b =
        let addPs1 = addToPrimeSet primeSets1
        addPs1 a b
        addPs1 b a

    let addPrimeSet2 a b =
        let addPs2 = addToPrimeSet primeSets2
        Set.intersect (set primeSets1.[a]) (set primeSets1.[b])
        |> Set.iter (fun x -> addPs2 (min a b, max a b) x
                              addPs2 (min a x, max a x) b
                              addPs2 (min b x, max b x) a)

    primeSeq
    |> Seq.take 1000
    |> Seq.map (fun x -> (x, x |> splits))
    |> Seq.filter (fun (_, y) -> y.Length > 0)
    |> Seq.tryFind (fun (x, y) ->
        y |> Seq.iter (fun (a, b) -> addPrimeSet1 a b
                                     addPrimeSet2 a b)
        primeSets3 |> Seq.isEmpty |> not
        )

    primeSets2 |> Seq.iter (fun x -> printfn "%O: %A" x.Key (x.Value |> Seq.toArray))
