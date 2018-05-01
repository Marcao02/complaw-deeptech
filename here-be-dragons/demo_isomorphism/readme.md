## Why

We'll use this in presentations, as well as in future funding proposals, e.g. for:

* Further material for NRF.
* [Tezos VC fund (no details yet)](https://www.coindesk.com/tezos-launch-50-million-venture-fund-boost-blockchain-growth/)


## Questions to address in comments on this commit

* Does any of the below seem like it's more trouble than it's worth?
* Suggested modifications of below proposal.

## Candidate plan 1
For two examples from `../linear_state_machine_language`, which I propose to be:

* `examplesLSM4/monster_burger.LSM`
* `examplesLSM4/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.LSM`

we're going to make a browser demo (I can do all of if if nobody else has time), that shows, in 2 or 4 panes, the L4 code, together with 1 or 3 other views associated with it, which can be selected from:

1. English NLG mockup
2. Chinese (or whatever language we have proficiency with on the team) NLG mockup
3. Generated BPMN diagram mockup (note: I haven't yet thought about how to translate to BPMN the "must later" and "may later" obligations/permissions used in `hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.LSM`)
4. Generated Viper/Michelson/etc smart contract mockup

By "mockup" I mean that we don't have to write the code that generates those views right now; that would be a little premature I think.

The only functionality that it really needs to have is highlighting of the relevant part of each other "generated" view when you hover over any part of one of the views. The mapping will be hard-coded.

## Candidate plan 2
This builds on the previous plan. For the same two examples, we demonstrate isomorphism between some depiction of execution trace in each of the views.

1. For the English contract, probably a sequence of sentences will suffice.
2. Ditto for the second natural language.
3. For BPMN, we show this by highlighting the "active" parts of the BPMN diagram.
4. For Viper/Michelson/etc, we show this my moving a line/block highlighting around the program. We note that we will ultimately use the language's debugger for this purpose.




