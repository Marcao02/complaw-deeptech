A bit about LSM version 3, which is in the works. Retroactively naming the previous version 2, from when explicit deontic keywords were introduced. LSM2 is reducible to LSM1 with a small increase in the number of states, and LSM3 will be reducible to LSM2 with a small-to-moderate increase in the number of states, though that’s not a design goal. _The main purpose of LSM3 is to be better source for NLG._

### Most-significantly changed examples:

* [hvitved\_lease.LSM](https://github.com/legalese/complaw-deeptech/blob/master/linear_state_machine_language/examplesLSM3/hvitved_lease.LSM)
* [hvitved\_master\_sales\_agreement\_simplified.LSM](https://github.com/legalese/complaw-deeptech/blob/master/linear_state_machine_language/examplesLSM3/hvitved_master_sales_agreement_simplified.LSM)
* [hvitved\_master\_sales\_agreement\_full\_with\_ids.LSM](https://github.com/legalese/complaw-deeptech/blob/master/linear_state_machine_language/examplesLSM3/hvitved_master_sales_agreement_full_with_ids.LSM)


### Major changes to language:

####`EventState` declarations are replaced by three types of declarations: `Action`, `Event&State`, and `Action&State`.

Each former EventState declaration becomes either an `Event&State` or an `Action&State`. The main difference between them is that an `Event&State` is initiated by the environment (including time passing) whereas an `Action&State` is initiated by an actor. Every EventState in the LSM2 examples already falls into exactly one of these two categories. This just makes it explicit.

As I hope its name suggests, an `Event&State` defines two things: an event, and the state that the LSM arrives in after the event happens. Likewise, an `Action&State` defines an action, and the state that the LSM arrives in after some actor performs the action.

By the way, I can easily be persuaded to change the naming of anything. I hate naming things and I don’t think I’ve very good at it.

`Action` is a new kind of declaration. As hopefully its name suggests, it defines just the `Action` part of an `Action&State`. When an actor does an `Action`, an `Entrance` code block gets executed (same as for `Event&State` and `Action&State`), but there is no state change.

Additionally (and this is what `Action` was introduced for), an `Action` declaration can have inside it a statement of the form:

`(‹Actor labels› (may do|must do|may do if ‹condition›|must do if ‹condition›) in states ‹set of Event&State and Action&State labels›`

This makes the `Action` applicable in each of the named states. For an example, from [hvitved_lease.LSM](https://github.com/legalese/complaw-deeptech/blob/master/linear_state_machine_language/examples/hvitved_lease.LSM), the redundancy in the two EventStates `Request_Termination_At_Rent_Or_Before` and `Request_Termination_After_Rent` is replaced by a single `Action` (and those states are deleted):

```
(Action Request_Termination
	((Tenant Landlord) may do if (not lease_terminated) in states (Month_Started RentToBePaid PayRent))

	(Entrance
		(lease_terminated := true)
	)
)
```

#### More than 2 Actors allowed
Actor labels are replaced by `ActorRole` labels, and now an actor is specified using one of the following forms, where `Bidder` is an `ActorRole`:

* `Bidder` - in case there is only one
* `(Bidder 5)`
* `(Bidder i)`
* `(Bidder i where ‹P(i)›)`
where P is a relation on ℕ.

The latter two forms say something about any number of actors.

In version 3.0.0 of LSM, we will take the number of actors to be fixed at contract start time, given as a contract parameter. With experience we will probably find it useful to allow actors to be dynamically added and removed, in which case we’ll add that in 3.X.0 for some X > 0.

This feature enables us to model some interesting smart contracts, such as http://solidity.readthedocs.io/en/develop/solidity-by-example.html#voting.

A (seemingly) much more advanced feature along those lines would be to allow the contract to apply and remove `ActorRoles` from actors, rather than have every actor be in a unique fixed `ActorRole`.



<!-- Additionally, we specify that only `Event&State` references can occur in the ~~`Fallback`~~`FallbackStates` block. -->