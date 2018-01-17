Let’s say, for concreteness, that the timeint unit is specified to be minutes (although I expect hours to be the most common for legal contracts). For blockchain contracts it would probably usually be milliseconds.

Every contract that’s ready for execution has a datetime contract_start_datetime, which has minutes precision, which can be timezone aware.

Each `event` is instantaneous (an “event” with duration is modelled by two such instantaneous `event`s, for the start and end), and has an associated “timestamp” t such that we know the real-world “event” happened between t and t+1 minutes since the start of the contract. There can be multiple `event`s in a contract execution with timestamp t, and the formal model can’t assign more-precise time units to them, but the formal model does have a total order of all the `event`s with timestamp t. When we need to model two “events” as truly-simultaneous, we use one `event` to model their cooccurrence.

We are able to use datetime and timedelta *literals* _up to minutes precision_ (recall simplifying assumption above) with timezones (eventually), but these get translated to timestamps internally, by functions datetime2timeint and timedelta2timeint. There are also translations in the opposite direction: timeint2timedelta and timeint2datetime, which are related by timeint2datetime(t) = contract_start_datetime + timeint2timedelta(t); those translations are used to present timeints to humans.

All of this ignores relativity, of course.

So, where have I gone wrong?