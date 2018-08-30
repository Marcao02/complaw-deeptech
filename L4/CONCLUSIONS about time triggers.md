1.
`immediately` does not literally mean immediately. It means within last_event_td + ε, where ε is a contract parameter.

2.
`must` can only be used with an upper bound, though it can be strict or non-strict.

3.
**USE** +1 timeunit for (must A within t). I was confused about that being problematic with real valued time.
(may A within t)
(breach at (t+1))

Whether after\_* trigger types are needed for time trigger events is awkward. On one hand, if we can say that something must be done at a time up to and including t, then it's not clear how we specify when the breach happens, without

(a) 
making time discrete, which is bad for FV, or

(b) 
using an after\_* trigger... which makes it slightly nondeterministic

Other options:

(c) 
use semantics analogous to how `immediately` is handled. So `(must A within t)` becomes

```
(may A (next\_event\_td ≤ t))
(breach (t < next\_event\_td ≤ t + ε))
```		
But is that any better than after\_*? That would look like
		`(breach (t < next_event_td ))`.
	I don't think it's any better.
	
(d) 
or could just forbid `(must A within t)` and don't have any sort of after-trigger

(e) do it at exactly `t + ε` in the execution implementation, but FV only knows it happens in `(t, t+ε]`. The LSM execution spec could say that the contract should do the event ASAP (and within `t+ε`), or else should have a deterministic way of choosing a timestamp within `(t, t+ε]`.

(f) A *timetrigger*-transition *can* happen "after" an actor-transition but at the same timestamp.

(g)
I could recommend (d), but implement (e).

