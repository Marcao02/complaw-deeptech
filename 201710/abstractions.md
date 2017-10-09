# Leaky Abstractions for L4

a series of increasingly complicated things

https://www.joelonsoftware.com/2002/11/11/the-law-of-leaky-abstractions/

also known as a "stack"

## about the notation

think of the following as pseudo-JSON or pseudo-YAML or
pseudo-XML -- it doesn't really matter, they're all
basically dictionaries of typed key/value pairs.

the pseudocode below omits
- a schema defining syntactic rules for well-formed keys & values
- quotation marks
- separator commas

## purpose

We show how a nontechnical business end-user might draft a very simple contract of sale by filling in the blanks.

We then seduce the end-user into adding complications to the contract via sequence of individually simple steps.

An end-user may stop complicating the contract at any time, of course.

### informal language spec

- symbol bindings can be mutable or immutable.
- mutable _variable_ bindings are declared with `var`
- immutable _value_ bindings are declared with `val`
- dictionaries look like `{ key: value, ... }`
- record extension syntax reads like `foo = bar ** { key: value, ... }`
- if you want to disinherit a particular key, say `foo = bar - [key1,key2,...]`

# A Sale Contract

One of the most common types of agreements: I have something you want; you have something I want; let's trade! Adam Smith would be proud.

## High Level Formulations

Nontechnical business end-users are expected to be able to write this sort of thing, possibly with the help of a web app form UI.

We call this a "high level" or "front end" contract syntax.

### Sale Contract, Simple

```
simpleSale = {
  buyer: Alice
  seller: Bob
  price: USD 100
  item: potato
  agreementDate: 1 Jan 1970
  jurisdiction: Delaware, USA
  contractType: sale
}
```

This means: on the agreementDate, the parties exchanged price for item, leaving the agreement as a record of the event.

This is an **executed** contract.

### Sale Contract, executory

```
futureSale = simpleSale ** {
  closingDate: 5 Jan 1970
}
```

This means: on the agreementDate, the parties agree to, in future on the closingDate, exchange price for item.

This is an **executory** contract.

### Sale Contract, multiple deadlines

```
datedSale = simpleSale - [closingDate] ** {
  paymentDate: 10 Jan 1970
  deliveryDate: 20 Jan 1970
}
```

The parties agree that instead of doing everything on the closingDate, first the payment will be made by one date, and then the delivery will be made by another date.

### Sale Contract, Party Details

```
partySale = datedSale ** {
  buyer:  { name: Alice,      type: naturalPerson, firstName: Alice, lastName: Andromeda, jurisdiction: USA,      idType: passport,   idString: H00000011A }
  seller: { name: "Bob Inc.", type: corporation,                                          jurisdiction: Delaware, idType: corpNumber, idString: C00000022B }
}
```

We have a little more information about the parties.

### Sale Contract, multipart

This corresponds to the printer master agreement from Hvitved.

```
multiSale = simpleSale - [item] ** {
  items:  { name: printer,
            totalCount: 500,
            unitPrice: USD 100,
            deliveryDeadline := orderDate + 14 days,
            paymentDeadline  := deliveryDate + 60 days
            }
  contractType: PARENT.contractType ,, multipart
  expirationDate: agreementDate + 1 year
}
```

Note the `PARENT` syntax to refer to the prototype's contractType.

Note the `,,` operator which appends to the contractType value, exposing its underlying nature -- an implicit array.

For a period of 1 year starting from the agreement date, the buyer may lodge one or more orders with the seller, where each order is for some number of items, so long as the total number of items ordered is less than 500. After receiving an order, the seller has 14 days to deliver the goods; after receiving the goods, the buyer has 60 days to pay.

Note the `:=` syntax to denote a per-order computation.

### Sale Contract, financing

```
financing = simpleSale ** {
  contractType: PARENT.contractType ,, financing
  interest: 6%
  term: agreementDate + 24 months
  minimumPayment: { period: monthly, slope: linear }
}
```

Up to now, sales agreements have shown payments in full of the relevant amount owed.

With financing, any unpaid balance will be charged 6% interest, and the minimum monthly payments will be calculated as a linear term loan formula.


## Low Level Formulations

A high level formulation expands programmatically, largely via fill-in-the blanks, to a "low level" or "back end" formulation.

The low level formulation looks more programmery, with types and so on.

This is a pretty straightforward template expansion exercise.

The low level template for a `sale + multipart` agreement can be found at 

`linear_state_machine_language/examplesLSM4/hvitved_master_sales_agreement_full_with_ids_and_obligation_objects.LSM`

It should be fairly obvious how the key/values from the high level "JSON" are spliced in to the low level template -- we substitute into `ContractParams` and `GlobalVars`.

## Intermixing High and Low Level Formalisms

How does one customize the contract -- beyond the high-level key/value interface?

Suppose a heretofore unknown ocean-dwelling species of human, *Homo Sapiens Aquaticus*, emerge from the seas and communicate to the Legalese team that they have a special requirement.

It is the custom of their people that any obligations incurred when the moon is waxing gibbous, shall receive an extra 7 days grace period for payment.

They want to know how to modify the sale agreement to suit.

### Customizing the low level template

We tweak the part of the low-level contract that says:
```
( Customer must later and (within 14D)
                do (PayBill quantity orderid) )
```
to include the lunar offset.

### Customizing the high level template

We tweak the `multiSale.items.paymentDeadline` attribute to include the lunar offset.

### Writing a contract transformator

We write a contract transformator that applies at some consistent stage of the contract lifecycle to perform the lunar offset.

## Feature invariants

all of these high level contracts can be compiled to natural language, as a term sheet.

all of these high level contracts can be filled into a low level contract and then compiled to natural language, as definitive documentation.


# about the language

## two aspects: Control vs Domain

the Control aspect of the language is common to all L4 sublanguages, and expresses the alethic and modal logics of contracts (including temporal, deontic, and epistemic concepts).

the Domain aspect reflects the peculiarities of the subdomain in question; extensible libraries of sub-domain-specific functionality represent mundane particulars like interest rates, debt, equity, conversion, regulatory compliance rules.

Both Control and Domain aspects are formulated using Ontologies: Control Ontologies and Domain Ontologies.

## Rules

A primitive Rule system is provided, from which more complex Control and Domain rules may be built up.

There are two basic types of rules.

### Noun Rules

An X counts-as a Y if rule Z is satisfied.

In prolog-like syntax:
```
% rule Z
Y(X) :- ...
```

A human is a British citizen if [(some predicate expression is true)](http://opim.wharton.upenn.edu/~sok/papers/s/p370-sergot.pdf).

Searle would call these "constitutive" rules.

The dominant style of reasoning is backward-chaining, and the dominant programming paradigm is logic programming.

### Verb Rules

Searle would call these "regulative" rules.

The dominant programming paradigm is imperative/procedural/object-oriented.

Expressed either directly or at one remove, in the form of a specification.


