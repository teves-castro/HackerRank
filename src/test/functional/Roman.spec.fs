module RomanNumeralsV2

    // ==========================================
    // Types
    // ==========================================

    type RomanDigit = 
        | I | II | III
        | IV | V 
        | IX | X | XX | XXX
        | XL | L 
        | XC | C | CC | CCC
        | CD | D 
        | CM | M | MM | MMM | MMMM
    type RomanNumeral = RomanNumeral of RomanDigit list 

    // ==========================================
    // Output logic
    // ==========================================

    /// Converts a single RomanDigit to an integer
    let digitToInt =
        function
        | I -> 1 | II -> 2 | III -> 3
        | IV -> 4 | V -> 5
        | IX -> 9 | X -> 10 | XX -> 20 | XXX -> 30
        | XL -> 40 | L -> 50 
        | XC -> 90 | C -> 100 | CC -> 200 | CCC -> 300
        | CD -> 400 | D -> 500 
        | CM -> 900 | M -> 1000 | MM -> 2000 | MMM -> 3000 | MMMM -> 4000

    let digitsToInt = List.map digitToInt

    /// converts a RomanNumeral to an integer
    let toInt (RomanNumeral digits) = digitsToInt digits

    // ==========================================
    // Input logic
    // ==========================================

    type ParsedChar = 
        | Digit of RomanDigit 
        | BadChar of char

    let rec toRomanDigitListRec charList = 
        match charList with
        // match the longest patterns first

        // 4 letter matches
        | 'M'::'M'::'M'::'M'::ns -> 
            Digit MMMM :: (toRomanDigitListRec ns)

        // 3 letter matches
        | 'I'::'I'::'I'::ns -> 
            Digit III :: (toRomanDigitListRec ns)
        | 'X'::'X'::'X'::ns -> 
            Digit XXX :: (toRomanDigitListRec ns)
        | 'C'::'C'::'C'::ns -> 
            Digit CCC :: (toRomanDigitListRec ns)
        | 'M'::'M'::'M'::ns -> 
            Digit MMM :: (toRomanDigitListRec ns)

        // 2 letter matches
        | 'I'::'I'::ns -> 
            Digit II :: (toRomanDigitListRec ns)
        | 'X'::'X'::ns -> 
            Digit XX :: (toRomanDigitListRec ns)
        | 'C'::'C'::ns -> 
            Digit CC :: (toRomanDigitListRec ns)
        | 'M'::'M'::ns -> 
            Digit MM :: (toRomanDigitListRec ns)

        | 'I'::'V'::ns -> 
            Digit IV :: (toRomanDigitListRec ns)
        | 'I'::'X'::ns -> 
            Digit IX :: (toRomanDigitListRec ns)
        | 'X'::'L'::ns -> 
            Digit XL :: (toRomanDigitListRec ns)
        | 'X'::'C'::ns -> 
            Digit XC :: (toRomanDigitListRec ns)
        | 'C'::'D'::ns -> 
            Digit CD :: (toRomanDigitListRec ns)
        | 'C'::'M'::ns -> 
            Digit CM :: (toRomanDigitListRec ns)

        // 1 letter matches
        | 'I'::ns -> 
            Digit I :: (toRomanDigitListRec ns)
        | 'V'::ns -> 
            Digit V :: (toRomanDigitListRec ns)
        | 'X'::ns -> 
            Digit X :: (toRomanDigitListRec ns)
        | 'L'::ns -> 
            Digit L :: (toRomanDigitListRec ns)
        | 'C'::ns -> 
            Digit C :: (toRomanDigitListRec ns)
        | 'D'::ns -> 
            Digit D :: (toRomanDigitListRec ns)
        | 'M'::ns -> 
            Digit M :: (toRomanDigitListRec ns)

        // bad letter matches
        | badChar::ns -> 
            BadChar badChar :: (toRomanDigitListRec ns)

        // 0 letter matches
        | [] -> 
            []

    let toRomanDigitList (s:string) = 
        s.ToCharArray() 
        |> List.ofArray 
        |> toRomanDigitListRec

    /// Convert a string to a RomanNumeral
    /// Does not validate the input.E.g. "IVIV" would be valid
    let toRomanNumeral s = 
        toRomanDigitList s
        |> List.choose (
            function 
            | Digit digit -> 
                Some digit 
            | BadChar ch -> 
                eprintfn "%c is not a valid character" ch
                None
            )
        |> RomanNumeral

    // ==========================================
    // Validation logic
    // ==========================================

    // check for validity
    let rec isValidDigitList digitList =
        match digitList with

        // empty list is valid
        | [] -> true

        // a following digit that is equal or larger is an error
        | d1::d2::_ 
            when d1 <= d2  -> 
                false

        // A single digit is always allowed
        | _::ds -> 
            // check the remainder of the list
            isValidDigitList ds 

    // top level check for validity
    let isValid (RomanNumeral digitList) =
        isValidDigitList digitList
