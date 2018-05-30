toUpper :: Char -> Char
toUpper a = if (fromEnum a) < 97 || (fromEnum a > 122) then ' '
            else toEnum(fromEnum (a) - 32)

toLower :: Char -> Char
toLower a = if (fromEnum a) < 65 || (fromEnum a > 90) then ' '
            else toEnum((fromEnum a) + 32)

isDigit :: Char -> Bool
isDigit a = if (fromEnum a) < 48 || (fromEnum a) > 57 then False
            else True

romanDigit :: Char -> String
romanDigit a = if a == '1' then "I"
               else if a == '2' then "II"
               else if a == '3' then "III"
               else if a == '4' then "IV"
               else if a == '5' then "V"
               else if a == '6' then "VI"
               else if a == '7' then "VII"
               else if a == '8' then "VIII"
               else if a == '9' then "IX"
               else " "