import L4v1

myAgreement =
  Agreement { agreementDate = "Jun 30"
            , parties = ["Alice","Bob"]
            , effectiveDate = UponSignature
            , clauses = [
                Clause { precondition=ToBool (\a -> \b -> True)
                       , responsibleParty="Alice"
                       , after=Immediately
                       , within=86400
                       , consequent=Fulfilled
                       , reparation=Breach
                       , condition=Payment "Bob" "SGD" 10
                       } ,
                Clause { precondition=ToBool (\a -> \b -> True)
                       , responsibleParty="Bob"
                       , after=Immediately
                       , within=86400
                       , consequent=Fulfilled
                       , reparation=Breach
                       , condition=Transfer "Alice" "Clause the Octopus"
                       }
                ]
            }  
  
main = do
  putStrLn (show myAgreement)
