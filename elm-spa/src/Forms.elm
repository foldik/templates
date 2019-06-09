module Forms exposing (ValidationResult(..), Value, formParam, getValue, notEmpty, setValue, validate)


type alias Value a =
    ( a, List (a -> ValidationResult), ValidationResult )


type ValidationResult
    = Valid
    | Invalid String


formParam : a -> List (a -> ValidationResult) -> Value a
formParam value validations =
    ( value, validations, Valid )


getValue : Value a -> a
getValue ( value, _, _ ) =
    value


setValue : a -> Value a -> Value a
setValue value ( _, validations, validationResult ) =
    ( value, validations, Valid )


validate : Value a -> Value a
validate ( value, validations, _ ) =
    let
        validationResult =
            List.map (\rule -> rule value) validations
                |> List.map
                    (\result ->
                        case result of
                            Invalid reason ->
                                reason

                            Valid ->
                                ""
                    )
                |> List.filter
                    (\message ->
                        case message of
                            "" ->
                                False

                            innerMessage ->
                                True
                    )
    in
    if List.isEmpty validationResult then
        ( value, validations, Valid )

    else
        ( value, validations, Invalid (String.join "" validationResult) )


notEmpty : String -> ValidationResult
notEmpty =
    \value ->
        if String.length value == 0 then
            Invalid "Must not be empty"

        else
            Valid
