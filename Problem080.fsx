module Problem080 =
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
            let minuend = remainder * 100I + (bigint (Seq.head groups))
            let subtrahend = current * 2I * 10I
            let digit = [0I..9I] |> Seq.takeWhile (fun x -> (subtrahend + x) * x <= minuend) |> Seq.last
            yield int digit
            yield! digits (minuend - (subtrahend + digit) * digit) (groups |> Seq.skip 1) (current * 10I + digit) }
        let groups = groups n
        let group = bigint (Seq.head groups)
        let first = bigint (sqrt (float group))
        yield int first
        yield! digits (group - first * first) (groups |> Seq.skip 1) first }

    let isIrrational n =
        let sq = int (sqrt (float n))
        not (sq * sq = n)

    [1..100]
    |> Seq.filter isIrrational
    |> Seq.map (fun n -> sqrtDigits n |> Seq.take 100 |> Seq.sum)
    |> Seq.sum
    |> printfn "%d"
