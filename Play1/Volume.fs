open System
open System.IO
open FsCheck
open FsCheck.Xunit

[<Measure>] type m
[<Measure>] type Area = m^2
[<Measure>] type Volume = Area*m 

let f (h:decimal<m>) = h
let g (a: decimal<m>) (b: decimal<m>):decimal<m^2> = (a*a + a*b + b*b) / 3m
let V h a b = f h * g a b

let VolumeNoHeightis0(a:decimal<m>) = 
    (a <= 10000000000m<m> && a >= 0m<m>) ==> (lazy (
    V 0m<m> a a = 0m<m^3>))

let RoundM3 (cubic:decimal<m^3>): decimal<m^3> = Math.Round(cubic * 1m<m^-3>,10)*1m<m^3> 

let VolumeAIsBEqualssquare((a:decimal<m>), (h:decimal<m>)) = 
        RoundM3 (V h a a) = RoundM3( a * a * h )

let VolumeA0IsPyramid ((h:decimal<m>), (b:decimal<m>)) = 
        RoundM3(V h 0m<m> b) = RoundM3( b * b * h / 3m )
    
Check.Quick VolumeNoHeightis0

let twoDecimals = Arb.generate<decimal<m>> |> Gen.suchThat ((<) 0m<m>) |> Gen.suchThat ((>) 1000000m<m>) |> Gen.two |> Arb.fromGen
Check.Quick (Prop.forAll twoDecimals VolumeAIsBEqualssquare)
Check.Quick (Prop.forAll twoDecimals VolumeA0IsPyramid)
Xunit.Assert.Equal(V 2m<m> 2m<m> 2m<m>, 8m<m^3>)
Console.ReadLine() |> ignore