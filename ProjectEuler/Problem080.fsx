(*
 *  Square root digital expansion
 *
 *  It is well known that if the square root of a natural number is not an integer, then it is
 *  irrational. The decimal expansion of such square roots is infinite without any repeating pattern
 *  at all.
 *
 *  The square root of two is 1.41421356237309504880..., and the digital sum of the first one
 *  hundred decimal digits is 475.
 *
 *  For the first one hundred natural numbers, find the total of the digital sums of the first one
 *  hundred decimal digits for all the irrational square roots.
 *)

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
