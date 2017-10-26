# companyDelta

Given two snapshots of a Company State, what corporate actions are required to transition the company from the first state to the second?

## Modeling Company State

We model the state of a company as a data structure.

A Company at a given time consists of
- a list of parties, which represent the other people in the universe of discourse
- a list of securities (in the abstract), each of which has a name
- a list of holdings, representing which parties hold how many of which concrete securities
- a list of agreements with various parties
- simple metadata like
  - the office address
  - legal jurisdiction
  - company number
  - list of directors
  - who is the corporate secretary
  - company constitution (ha, ha, we said "simple" metadata but this isn't that simple.)

## Modeling Differences

The differences between two states of a company are represented by a `Tree Diff`. At the highest level of the tree, the root node represents all the differences between old and new. At lower levels of the tree, the differences are represented by finer-grained chunks. The leaves of the tree represent the finest-grained differences: if a date changes, we just record a Replace.

## Modeling the State Transition Graph

Think Git for a moment: each commit is a node in an ever-expanding graph of repository states.

Now recall the Possible Worlds Hypothesis. Every state of the universe branches to even more states.

It's the same with modeling a company. Each state of the company is a node in the ever-expanding graph of possible states. For example, at any given node, it is possible, though unlikely, that Elon Musk could become a director of the company; that transition would be represented by a new node, where the state of the company includes Elon Musk on the list of directors, where previously he was not on the list. Unlikely, but not impossible, because Elon Musk is known to get bored with his existing portfolio of companies and occasionally seek new startups to run.

The vast majority of nodes in the graph of a given company are virtual, like virtual particles appearing and disappearing in the froth of spacetime.

For every actual company there is only one actual path through the graph of its history, that takes it to the current state.

From the current state, we can branch to possible new states.

So, back to the graph. Let a company state be a node. Let a pair of before & after states be two connected nodes in a graph. Before and after, start to finish, the two sides of any Diff.

The difference between two states may be represented by a path of edges between the two nodes. Intermediate nodes represent incremental differences along the way from the start to the finish.

The full path between the start and end states represents the full Diff tree discussed previously. Diff subtrees correspond to subpaths.

Each edge between nodes is an atomic corporate action. Every corporate action takes the form of Paperwork.

Examples of paperwork:
- members resolution to appoint a board of directors
- directors resolution to change the address of the company
- directors resolution to appoint a different corporate secretary
- execution of an agreement with one or more other parties
- filing some record of a happening with the government (which we model as a notice)
- sending an instruction to the corporate secretary (which we also model as a notice)
- sending a notice to shareholders about a meeting

Note that some of these paperwork examples are compound: sending a notice to shareholders involves one notice for each shareholder. If a company has five shareholders, then five notices are needed. In the graph, a parent node fans out to five edges, which converge back to the next state. The parent node has one inbound edge, which is of type Paperwork: "directors resolution to notify the shareholders." The parent node is of type State: "directors have resolved to notify the shareholders". Each outbound edge from that parent node is of type Paperwork: "send a notification to shareholder A/B/C/D/E." The single child node, which has five incoming edges, is of type State: "notifications have been sent to all shareholders A/B/C/D/E."

Note also that the Before and After of any given Diff always map to two nodes in the graph, but any two nodes in the graph do not always map to a Diff. This is because the tree of diffs between company states does not suffer to observe every little intermediate state. The state transition graph goes even finer-grained than the Diff Tree. The graph contains plenty of little intermediate nodes which together form a sequence that get you from one company state to another, but which on their own do not move the needle.

## Ordering State Transition Dependencies

To achieve a given diff transition between two nodes, one or more pieces of paperwork may be required: we contemplate a path of length N >= 1.

To be a legal sequence, a path of corporate actions may be required to obey an ordering. Path A-C-B-D may be illegal, while path A-B-C-D is legal.

For example, the correct procedure for issuing new shares is:

1. (directors resolution) directors agree to issue new shares
2. (directors resolution) directors request permission from members to issue new shares
3. (members resolutions) members either hold an extraordinary general meeting or pass resolutions by written means approving the directors' plan
4. (company notice to debtholders) if the new share issue triggers any convertible debt, company informs debtholders and revises the shareholder roster accordingly
5. (company notice to shareholders) directors, writing on behalf of company, offer existing members pro-rata rights in the new share issue
6. (company notice to non-members) directors offer non-members participation in the new share issue
7. (company generates and executes agreement) directors sign investment agreement with participating investors
8. (company generates and executes agreement) new investors sign shareholders agreement
9. (company generates and executes agreement) old investors sign deed of ratification and accession of shareholders agreement

We use a dependency tree to model the sequence of these corporate actions. Every piece of paperwork is an edge between one node and another. All paperwork within a given dependency stage must be signed before documents within the next dependency stage can be signed.

## Aggregating State Transitions

Sometimes we combine multiple edges into an aggregate document.

For example, if a sequence contains three directors resolutions, the software should group those three directors resolutions into a single document for signature purposes.

However, if notices need to go to different shareholdes, then each edge is a separate document, even though those edges all lie between the same two nodes.

## Signatures

Each piece of Paperwork may have zero or more signatories. You can think of these signatories as a special kind of children to each node -- each party has to sign, before the paperwork can be considered executed. The specific mode of execution depends on the (sub)type of paperwork. If it's a notice, then the notice has to be signed and sent. If it's a deed, it has to be witnessed. And so on.

## Blowing Minds with Graph Theory

"Every good idea will be discovered twice: once by a logician and once by a computer scientist." - Philip Wadler

We get to reuse important ideas from graph theory like topological sorts.

Once all the paperwork is generated, what can we start signing? If the documents lie along a single path then the execution sequence is straightforward. If the graph of execution looks more like a bushy tree then we can start executing documents in parallel.

Dependency graphs are not a new idea to anyone who has ever come across a Makefile or package management.

## Temporal Considerations

Sometimes the rules require that a certain edge be traversed within a certain deadline of some other edge. The deadline can be pre or post.

For example, one way for a general meeting of the shareholders to be valid, is that a notice to shareholders is sent at least 14 days before the meeting. That's a pre deadline.

If a company edits its list of directors, then it has to file that change with the government within N days after the edit happens. That's a post deadline.

## Rule Alternatives

Sometimes there are multiple paths to validity. 

## Outputting the Graph

We want to display the graph to the user in the form of a bunch of D3 code that renders in a web browser.

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

