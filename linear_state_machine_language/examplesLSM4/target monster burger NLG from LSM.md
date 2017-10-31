
* Upon `the Customer` `ordering the monster burger`:
	* `the Restaurant` should `serve the monster burger` within 15 minutes, and
	* otherwise `the Restaurant` `violates its prompt serve gaurantee`.

* Upon `serving the monster burger`:
	* `The challenge end time` is defined as the current time plus 1 hour, and
	* `The Customer's amount owing` is then $50, and
	* then, `the challenge continues`.

* Whenever `the challenge continues`:
	* `The Customer` may `announce finishing the monster burger` before `the challenge end time`, and
	* otherwise, `the challenge period is over` (if the time reaches `the challenge end time`).

* Upon `the end of the challenge period`:
	* `the Restaurant` should `check that the Customer has finished` within 10 minutes, and
	* otherwise, `the Restaurant` `violates its prompt-check guarantee`.

* Upon `the Restaurant` `having checked that the Customer has finished`:
	* `the Restaurant` may `confirm the Customer is finished` immediately, or
	* `the Restaurant` may `disconfirm the Customer is finished` immediately

* If
	`the Restaurant` `violates its prompt-check guarantee` or
	`the Restaurant` `violates its prompt-serve guarantee` or
	`the Restaurant` `confirms the Customer is finished`
  	then `the Customer's amount owing` is $0, and `the contract` is fulfilled.

* Upon `the Restaurant` `having disconfirmed the Customer is finished`:
	* `the Customer` must `pay the bill` if the time reaches `the end of the challenge period`, and
	* otherwise, `the challenge continues` (if the time is before `the end of the challenge period`).

* Upon `the Customer` `paying the bill`, `the Customer` `transfers` `the amount owing` to `the Restaurant`. `The amount owing` is then $0. Then `the contract` is fulfilled.

