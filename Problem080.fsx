module Problem080 =

    open System.Numerics

    let groups n = seq {
        let rec fullpart n = seq {
            match n / 100 with
            | 0 -> yield n
            | x -> yield! fullpart x
                   yield n % 100 }
        yield! fullpart n
        while true do yield 0 }

    let sqrtDigits n = seq {
        let rec digits remainder (groups : int seq) current = seq {
            let minuend = remainder * (bigint 100) + (bigint (Seq.head groups))
            let subtrahend = current * (bigint 2) * (bigint 10)
            let digit = [0..9] |> Seq.takeWhile (fun x -> (subtrahend + (bigint x)) * (bigint x) <= minuend) |> Seq.last
            yield digit
            yield! digits (minuend - (subtrahend + (bigint digit)) * (bigint digit)) (groups |> Seq.skip 1) (current * (bigint 10) + (bigint digit)) }
        let groups = groups n
        let group = Seq.head groups
        let first = int (sqrt (float group))
        let remainder = (bigint group) - (bigint first) * (bigint first)
        yield first
        yield! digits remainder (groups |> Seq.skip 1) (bigint first) }

    let isIrrational n =
        let sq = int (sqrt (float n))
        not (sq * sq = n)

    [1..100]
    |> Seq.filter isIrrational
    |> Seq.map (fun n -> sqrtDigits n |> Seq.take 100 |> Seq.sum)
    |> Seq.sum
    |> printfn "%d"
