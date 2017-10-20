# companyDelta

Given two snapshots of a Company State, what corporate actions are required to transition the company from the first state to the second?

## Modeling Company State

We model the state of a company as a data structure.

A Company at a given time consists of
- a set of shareholders
- a set of securities
- relations representing which shareholders hold how many securities
- agreements with parties
- the company constitution
- simple metadata like
  - the office address
  - legal jurisdiction
  - company number
  - list of directors
  - who is the corporate secretary

## Modeling Differences

The differences between two states of a company are represented by a `Tree Diff`. At the highest level of the tree, the root node represents all the differences between old and new. At lower levels of the tree, the differences are represented by finger-grained chunks. The leaves of the tree represent the finest-grained differences.

## Modeling State Transitions

Let a state be a node. Let a pair of states be two nodes in a graph.

The diff between two states is represented by a path of edges between the two nodes.

The full path between the start and end states represents the full Diff tree.

Subpaths represent Diff subtrees. Each edge is an atomic corporate action, represented by a piece of Paperwork.

For example:
- directors resolution to change the address of the company
- members resolution to appoint a board of directors
- execution of an agreement
- sending a notice to shareholders about a meeting
- sending an instruction to the corporate secretary
- directors resolution to appoint a different corporate secretary

## Ordering State Transition Dependencies

To be a legal sequence, a path of corporate actions may be required to obey an ordering.

For example, the correct procedure for issuing new shares is:

1. (directors resolution) directors agree to issue new shares
2. (directors resolution) directors request permission from members to issue new shares
3. (members resolutions) members either hold an extraordinary general meeting or pass resolutions by written means approving the directors' plan
4. (company notice to debtholders) if the new share issue triggers any convertible debt, company informs debtholders and revises the shareholder roster accordingly
4. (company notice to shareholders) directors, writing on behalf of company, offer existing members pro-rata rights in the new share issue
5. (company notice to shareholders) directors offer existing members excess rights in the new share issue
6. (company notice to non-members) directors offer non-members participation in the new share issue
7. (company generates and executes agreement) directors sign investment agreement with participating investors
8. (company generates and executes agreement) new investors sign shareholders agreement
9. (company generates and executes agreement) old investors sign deed of ratification and accession of shareholders agreement

We use a dependency tree to model the sequence of these corporate actions. We collect nodes into node-groups, which we call dependency stages. Every document within a given dependency stage must be signed before documents within the next dependency stage can be signed.

## Aggregating State Transitions

We combine actions of the same type within the same dependency stage, into an aggregate document.

For example, if a dependency stage contains three directors resolutions, the software should group those three directors resolutions into a single document for signature purposes.

## External Integrations

Initially we hardcode the dependencies.

It may be possible to represent the dependency rules using something like [SBVR](https://en.wikipedia.org/wiki/Semantics_of_Business_Vocabulary_and_Business_Rules).

After we do that we can generate the legal sequence (using an SMT solver?) as a model that satisfies the rule constraints.


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
      Diff
        Update
        Just
        CompanyState
          { parties =
              [ Party
                  { fullname = "Alice"
                  , idtype = "passport"
                  , idnum = "E1111111B"
                  , nature = OtherNature "natural"
                  , gender = Female
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
                , address = "1 Corporate Park, Singapore 139951"
                , idtype = "UEN"
                , idnum = "UEN11111111A"
                , constitution =
                    [ ConExp
                        { title = "Constitution Default"
                        , body = "The default constitution shall be substituted here."
                        }
                    ]
                , directors = [ "Alice" ]
                , secretary = "Alice"
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
              ]
          , agreements =
              [ Contract
                  { parties = [ "Alice" , "Company" ]
                  , dated = 1970 (-01) (-01)
                  , title = "shareholdersAgreement"
                  , singleton = True
                  }
              ]
          }
        Just
        CompanyState
          { parties =
              [ Party
                  { fullname = "Alice"
                  , idtype = "passport"
                  , idnum = "E1111111B"
                  , nature = OtherNature "natural"
                  , gender = Female
                  }
              , Party
                  { fullname = "Bob"
                  , idtype = "nric"
                  , idnum = "S2222222B"
                  , nature = OtherNature "natural"
                  , gender = Male
                  }
              , Party
                  { fullname = "Carol"
                  , idtype = "nric"
                  , idnum = "S3333333C"
                  , nature = OtherNature "natural"
                  , gender = Female
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
                , address = "1 Corporate Park, Singapore 139951"
                , idtype = "UEN"
                , idnum = "UEN11111111A"
                , constitution =
                    [ ConExp
                        { title = "Constitution Default"
                        , body = "The default constitution shall be substituted here."
                        }
                    ]
                , directors = [ "Alice" ]
                , secretary = "Alice"
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
              , Holding
                  { holder = "Carol"
                  , holds =
                      [ HeldSecurity
                          { securityName = "ordinary shares"
                          , units = Just 300.0
                          , money = Nothing
                          , description = Just "Carol is added"
                          }
                      ]
                  }
              ]
          , agreements =
              [ Contract
                  { parties = [ "Company" , "Alice" , "Bob" , "Carol" ]
                  , dated = 1970 (-01) (-02)
                  , title = "shareholdersAgreement"
                  , singleton = True
                  }
              ]
          }
        changed
        CompanyState
  , subForest =
      [ Node
          { rootLabel =
              Diff
                Update
                Just
                [ Party
                    { fullname = "Alice"
                    , idtype = "passport"
                    , idnum = "E1111111B"
                    , nature = OtherNature "natural"
                    , gender = Female
                    }
                ]
                Just
                [ Party
                    { fullname = "Alice"
                    , idtype = "passport"
                    , idnum = "E1111111B"
                    , nature = OtherNature "natural"
                    , gender = Female
                    }
                , Party
                    { fullname = "Bob"
                    , idtype = "nric"
                    , idnum = "S2222222B"
                    , nature = OtherNature "natural"
                    , gender = Male
                    }
                , Party
                    { fullname = "Carol"
                    , idtype = "nric"
                    , idnum = "S3333333C"
                    , nature = OtherNature "natural"
                    , gender = Female
                    }
                ]
                changed
                Holders
          , subForest =
              [ Node
                  { rootLabel =
                      Diff
                        Create
                        Nothing
                        Just
                        Party
                          { fullname = "Bob"
                          , idtype = "nric"
                          , idnum = "S2222222B"
                          , nature = OtherNature "natural"
                          , gender = Male
                          }
                        from
                        Nothing
                  , subForest = []
                  }
              , Node
                  { rootLabel =
                      Diff
                        Create
                        Nothing
                        Just
                        Party
                          { fullname = "Carol"
                          , idtype = "nric"
                          , idnum = "S3333333C"
                          , nature = OtherNature "natural"
                          , gender = Female
                          }
                        from
                        Nothing
                  , subForest = []
                  }
              ]
          }
      , Node
          { rootLabel =
              Diff
                Update
                Just
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
                ]
                Just
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
                , Holding
                    { holder = "Carol"
                    , holds =
                        [ HeldSecurity
                            { securityName = "ordinary shares"
                            , units = Just 300.0
                            , money = Nothing
                            , description = Just "Carol is added"
                            }
                        ]
                    }
                ]
                changed
                Holdings
          , subForest =
              [ Node
                  { rootLabel =
                      Diff
                        Create
                        Nothing
                        Just
                        [ HeldSecurity
                            { securityName = "ordinary shares"
                            , units = Just 200.0
                            , money = Nothing
                            , description = Just "Bob is added"
                            }
                        ]
                        from
                        Nothing
                  , subForest = []
                  }
              , Node
                  { rootLabel =
                      Diff
                        Create
                        Nothing
                        Just
                        [ HeldSecurity
                            { securityName = "ordinary shares"
                            , units = Just 300.0
                            , money = Nothing
                            , description = Just "Carol is added"
                            }
                        ]
                        from
                        Nothing
                  , subForest = []
                  }
              ]
          }
      , Node
          { rootLabel =
              Diff
                Update
                Just
                [ Contract
                    { parties = [ "Alice" , "Company" ]
                    , dated = 1970 (-01) (-01)
                    , title = "shareholdersAgreement"
                    , singleton = True
                    }
                ]
                Just
                [ Contract
                    { parties = [ "Company" , "Alice" , "Bob" , "Carol" ]
                    , dated = 1970 (-01) (-02)
                    , title = "shareholdersAgreement"
                    , singleton = True
                    }
                ]
                changed
                Contracts
          , subForest =
              [ Node
                  { rootLabel =
                      Diff
                        Update
                        Just
                        Contract
                          { parties = [ "Alice" , "Company" ]
                          , dated = 1970 (-01) (-01)
                          , title = "shareholdersAgreement"
                          , singleton = True
                          }
                        Just
                        Contract
                          { parties = [ "Company" , "Alice" , "Bob" , "Carol" ]
                          , dated = 1970 (-01) (-02)
                          , title = "shareholdersAgreement"
                          , singleton = True
                          }
                        changed
                        Contract
                  , subForest =
                      [ Node
                          { rootLabel =
                              Diff
                                Update
                                Just
                                [ "Alice" , "Company" ]
                                Just
                                [ "Company" , "Alice" , "Bob" , "Carol" ]
                                changed
                                PartyNames
                          , subForest =
                              [ Node
                                  { rootLabel = Diff Create Nothing Just "Bob" from Nothing
                                  , subForest = []
                                  }
                              , Node
                                  { rootLabel = Diff Create Nothing Just "Carol" from Nothing
                                  , subForest = []
                                  }
                              ]
                          }
                      , Node
                          { rootLabel =
                              Diff Replace Just "1970-01-01" Just "1970-01-02" change string
                          , subForest = []
                          }
                      ]
                  }
              ]
          }
      ]
  }
```

