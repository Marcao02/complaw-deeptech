# companyDelta

```
20171009-14:49:24 mengwong@venice2:~/non-db-src/l/compiler/companyDelta% stack build && stack exec companyDelta-exe  -- examples/case1/before.json examples/case1/after.json | ppsh
companyDelta-0.1.0.0: unregistering (local file changes: src/CDiff.hs)
companyDelta-0.1.0.0: build
Preprocessing library companyDelta-0.1.0.0...
[2 of 2] Compiling CDiff            ( src/CDiff.hs, .stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/CDiff.o )
Preprocessing executable 'companyDelta-exe' for companyDelta-0.1.0.0...
[1 of 1] Compiling Main             ( app/Main.hs, .stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/companyDelta-exe/companyDelta-exe-tmp/Main.o ) [CDiff changed]
Linking .stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/companyDelta-exe/companyDelta-exe ...
companyDelta-0.1.0.0: copy/register
Installing library in
/Users/mengwong/non-db-src/l/compiler/companyDelta/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/lib/x86_64-osx-ghc-8.0.2/companyDelta-0.1.0.0-2UBEc6mCwzuA4V4iW0oz4N
Installing executable(s) in
/Users/mengwong/non-db-src/l/compiler/companyDelta/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/bin
Registering companyDelta-0.1.0.0...
Node
  { rootLabel =
      Update
        CompanyState
          { holders =
              [ Holder
                  { fullname = "Alice"
                  , idtype = "passport"
                  , idnum = "E1111111B"
                  , nature = OtherNature "natural"
                  , gender = Female
                  }
              , Holder
                  { fullname = "Bob"
                  , idtype = "nric"
                  , idnum = "S2222222B"
                  , nature = OtherNature "natural"
                  , gender = Male
                  }
              ]
          , securities =
              [ Security
                  { name = "ordinary shares" , measure = OtherMeasure "ByUnit" }
              ]
          , company =
              Company
                { name = "Acme Potato Pte. Ltd."
                , jurisdiction = "Singapore"
                , idtype = "UEN"
                , idnum = "UEN11111111A"
                }
          , holdings =
              [ Holding
                  { holder = "Alice"
                  , holds =
                      [ HeldSecurity
                          { securityName = "ordinary shares"
                          , units = Just 100.0
                          , money = Nothing
                          , description = Nothing
                          }
                      ]
                  }
              , Holding
                  { holder = "Bob"
                  , holds =
                      [ HeldSecurity
                          { securityName = "ordinary shares"
                          , units = Just 200.0
                          , money = Nothing
                          , description = Just "Bob is added"
                          }
                      ]
                  }
              ]
          }
        "changed CompanyState"
  , subForest =
      [ Node
          { rootLabel =
              Update
                [ Holder
                    { fullname = "Alice"
                    , idtype = "passport"
                    , idnum = "E1111111B"
                    , nature = OtherNature "natural"
                    , gender = Female
                    }
                , Holder
                    { fullname = "Bob"
                    , idtype = "nric"
                    , idnum = "S2222222B"
                    , nature = OtherNature "natural"
                    , gender = Male
                    }
                ]
                "changed Holders"
          , subForest =
              [ Node
                  { rootLabel =
                      Create
                        Holder
                          { fullname = "Bob"
                          , idtype = "nric"
                          , idnum = "S2222222B"
                          , nature = OtherNature "natural"
                          , gender = Male
                          }
                        "from Nothing"
                  , subForest = []
                  }
              ]
          }
      , Node
          { rootLabel =
              Update
                [ Holding
                    { holder = "Alice"
                    , holds =
                        [ HeldSecurity
                            { securityName = "ordinary shares"
                            , units = Just 100.0
                            , money = Nothing
                            , description = Nothing
                            }
                        ]
                    }
                , Holding
                    { holder = "Bob"
                    , holds =
                        [ HeldSecurity
                            { securityName = "ordinary shares"
                            , units = Just 200.0
                            , money = Nothing
                            , description = Just "Bob is added"
                            }
                        ]
                    }
                ]
                "changed Holdings"
          , subForest =
              [ Node
                  { rootLabel =
                      Create
                        [ HeldSecurity
                            { securityName = "ordinary shares"
                            , units = Just 200.0
                            , money = Nothing
                            , description = Just "Bob is added"
                            }
                        ]
                        "from Nothing"
                  , subForest = []
                  }
              ]
          }
      ]
  }
```

