module Utils.Date 
    ( toISODateString
    )
    where


import Date
import String


toISODateString : Date.Date -> String
toISODateString date =
  let
    year =
      Date.year date

    month =
      case Date.month date of
        Date.Jan -> 1
        Date.Feb -> 2
        Date.Mar -> 3
        Date.Apr -> 4
        Date.May -> 5
        Date.Jun -> 6
        Date.Jul -> 7
        Date.Aug -> 8
        Date.Sep -> 9
        Date.Oct -> 10
        Date.Nov -> 11
        Date.Dec -> 12

    day =
      Date.day date

    zeroPad2 =
      String.padLeft 2 '0'
  in
    (toString year)
    ++ "-" ++ (zeroPad2 (toString month))
    ++ "-" ++ (zeroPad2 (toString day))
