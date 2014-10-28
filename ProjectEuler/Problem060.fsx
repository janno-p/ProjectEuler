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

    let knownPrimes = new SortedSet<int>([2; 3]);

    let rec checkIfPrime (ps: IEnumerator<int>) value sqrtValue =
        if ps.MoveNext() then
            match ps.Current with
            | n when n >= sqrtValue -> true
            | n when value % n = 0 -> false
            | _ -> checkIfPrime ps value sqrtValue
        else true

    let rec isPrime num =
        let maxPrime = knownPrimes.Max
        match num > maxPrime with
        | true ->
            let rec findNextPrime checkNum =
                match checkIfPrime (knownPrimes.GetEnumerator()) checkNum ((Math.Sqrt(float checkNum) |> int) + 1) with
                | true -> knownPrimes.Add(checkNum) |> ignore
                | _ -> findNextPrime (checkNum + 2)
            findNextPrime (maxPrime + 2)
            isPrime num
        | _ -> knownPrimes.Contains(num)

    let primeSeq = seq {
        yield 2
        yield 3
        let lastValue = ref 3
        while true do
            lastValue := !lastValue + 2
            if isPrime !lastValue then
                yield !lastValue
    }

    let combineNum a b =
        let rec getDec d =
            match d * 10 with
            | x when x < b -> getDec x
            | x -> x
        a * (getDec 1) + b

    let splits prime =
        let rec split rank = seq {
            let nr = rank * 10
            if nr < prime then
                match prime % nr with
                | x when x < rank -> ()
                | x ->
                    let t = prime / nr
                    if (isPrime t) && (isPrime x) && (isPrime (combineNum x t)) then
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

    let addPrimeSet3 a b c d =
        let mkKey k1 k2 k3 =
            let s = set([k1; k2; k3]) |> Set.toArray
            (s.[0], s.[1], s.[2])
        addToPrimeSet primeSets3 (mkKey a b c) d
        addToPrimeSet primeSets3 (mkKey a c d) b
        addToPrimeSet primeSets3 (mkKey a b d) c
        addToPrimeSet primeSets3 (mkKey b c d) a
        Set.intersectMany [(set primeSets1.[a]); (set primeSets1.[b]); (set primeSets1.[c]); (set primeSets1.[d])]
            |> Set.iter (ignore)

    let addPrimeSet2 a b c =
        let mkKey x y = (min x y, max x y)
        addToPrimeSet primeSets2 (mkKey a b) c
        addToPrimeSet primeSets2 (mkKey a c) b
        addToPrimeSet primeSets2 (mkKey b c) a
        Set.intersectMany [(set primeSets2.[mkKey a b]); (set primeSets2.[mkKey a c]); (set primeSets2.[mkKey b c])]
            |> Set.iter (addPrimeSet3 a b c)

    let addPrimeSet1 a b =
        addToPrimeSet primeSets1 a b
        addToPrimeSet primeSets1 b a
        match a, b with
        |   3, 7 -> printfn "(  37)   3: %A" (primeSets1.[3] |> Seq.toArray)
                    printfn "(  37)   7: %A" (primeSets1.[7] |> Seq.toArray)
        |   7, 3 -> printfn "(  73)   3: %A" (primeSets1.[3] |> Seq.toArray)
                    printfn "(  73)   7: %A" (primeSets1.[7] |> Seq.toArray)
        | 109, 3 -> printfn "(1093) 109: %A" (primeSets1.[109] |> Seq.toArray)
                    printfn "(1093)   3: %A" (primeSets1.[3] |> Seq.toArray)
        | 109, 7 -> printfn "(1097) 109: %A" (primeSets1.[109] |> Seq.toArray)
                    printfn "(1097)   7: %A" (primeSets1.[7] |> Seq.toArray)
        | _ -> ()
        Set.intersect (set primeSets1.[a]) (set primeSets1.[b])
            |> Set.iter (addPrimeSet2 a b)

    primeSeq
    |> Seq.take 10000
    |> Seq.map (fun x -> (x, x |> splits))
    |> Seq.filter (fun (_, y) -> y.Length > 0)
    |> Seq.tryFind (fun (_, y) ->
        y |> Seq.iter (fun (a, b) -> addPrimeSet1 a b)
        primeSets3 |> Seq.isEmpty |> not
        )

    //primeSets3 |> Seq.iter (fun x -> printfn "%O: %A" x.Key (x.Value |> Seq.toArray))

    //printfn "%d" knownPrimes.Max
