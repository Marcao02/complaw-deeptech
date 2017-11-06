# SMTLIB solutions to SAFE and KISS interactions

If a startup has raised money using a mix of Safe and KISS instruments, what happens at conversion time?

This Z3 SMTlib2 code works through any scenario in which a startup raises using both SAFE and and KISS instruments, and subsequently raises equity and needs to do the conversion.

## Usage

Copy the safe-example1.z3 to your particular situation. Fill in the numbers.

Safes are always excluded from the KISS's fully-diluted capitalization.

A repayable KISS(Debt) is excluded from the Safe's company capitalization.

A nonrepayable KISS(Equity) is included in the Safe's company capitalization.

## Business Implications

A larger capitalization is favorable to a converting investor because they get more shares for the same money.

Inclusion of other securities in an instrument's capitalization is favorable to the investor holding that instrument.

Exclusion of other securities from an instrument's capitalization reduces the holder's resulting percentage.

## Example

```
20171028-18:03:43 mengwong@venice2:~/non-db-src/l/compiler/linear_state_machine_language/examples/safe/smtlib% cat safe-pre.z3 safe-example1.z3 safe-post.z3 | z3 -in | perl -0 -ple 's/\n   //gs' | sort | grep -a define
  (define-fun equityFinancing_dilutePre () Bool false)
  (define-fun equityFinancing_fdc () Real 11000000.0)
  (define-fun equityFinancing_num_shares_subtotal () Real 1100000.0)
  (define-fun equityFinancing_num_shares_total () Real 1320000.0)
  (define-fun equityFinancing_pps () Real (/ 10.0 11.0))
  (define-fun equityFinancing_premoney () Real 10000000.0)
  (define-fun equityFinancing_value () Real 1000000.0)
  (define-fun initialStock () Real 11000000.0)
  (define-fun kiss_cap () Real 2000.0)
  (define-fun kiss_discount () Real 25.0)
  (define-fun kiss_force_include_safe () Bool false)
  (define-fun kiss_has_cap () Bool true)
  (define-fun kiss_num_shares () Real 0.0)
  (define-fun kiss_post () Real 0.0)
  (define-fun kiss_pps () Real (/ 1.0 5500.0))
  (define-fun kiss_pps_fromcap () Real (/ 1.0 5500.0))
  (define-fun kiss_pps_fromdis () Real (/ 15.0 22.0))
  (define-fun kiss_repayable () Bool false)
  (define-fun kiss_value () Real 0.0)
  (define-fun non_kiss_shares () Real 0.0)
  (define-fun non_safe_shares () Real 0.0)
  (define-fun safe_cap () Real 5000000.0)
  (define-fun safe_discount () Real 0.0)
  (define-fun safe_has_cap () Bool true)
  (define-fun safe_num_shares () Real 220000.0)
  (define-fun safe_post () Real 200000.0)
  (define-fun safe_pps () Real (/ 5.0 11.0))
  (define-fun safe_pps_fromcap () Real (/ 5.0 11.0))
  (define-fun safe_pps_fromdis () Real (/ 10.0 11.0))
  (define-fun safe_value () Real 100000.0)
```

## Reference

http://www.ycombinator.com/docs/SAFE_Primer.rtf

## Scenarios

A Safe isn't just a Safe: there are four variants.

A KISS isn't just a KISS: there are two variants.

Some variants of the KISS may interact with some variants of the Safe, and vice versa.

The interaction comes in the computation of the company capitalization, also known as the fully diluted capitalization.

Investors want this number to be as large as possible at the time of conversion because they get more shares for the same price.

### Mutual Indifference

The simplest scenario is that the two instruments do not interact: neither one includes the other as part of the pre-conversion capitalization.

#### The Safe(MFN) and Safe(Discount, No Cap) ignore KISSes

These variants of the Safe do not define a valuation cap, so only the discount is relevant to the conversion of the Safe. These variants do not compute company capitalization, so they are indifferent to the KISS.

#### Safes ignore other Safes.

The Safe variants that do consider a valuation cap expressly exclude other Safes from the capitalization, and also exclude convertible promissory notes.

But they do include warrants and other convertible securities.

So is a KISS a warrant or other convertible security (to be included), or a convertible promissory note (to be excluded)? Or is it something else?

#### The Safe(Cap) and Safe(Cap, No Discount) ignore KISS(Debt)

The Safe(Cap) and Safe(Cap, No Discount) both define:

    “Company Capitalization” means the sum, as of immediately prior to the Equity Financing, of:
     (1) all shares of Capital Stock (on an as-converted basis) issued and outstanding, assuming exercise or conversion of all outstanding vested and unvested options, warrants and other convertible securities, but excluding
        (A) this instrument,
        (B) all other Safes, and
        (C) convertible promissory notes
     ; and
     (2) all shares of Common Stock reserved and available for future grant under any equity incentive or similar plan of the Company, and/or any equity incentive or similar plan to be created or increased in connection with the Equity Financing.

Suppose the other instrument is a convertible promissory note, then (1)(C) says to ignore it, so the computation of the Safe(Cap) is insensitive to the conversion of the other instrument.

But if the other instrument is a warrant or other convertible security, then it is included.

A KISS(Debt) is a convertible promissory note, so it is excluded.

(However, consider that the language says "immediately prior to the Equity Financing". Now, do we believe that immediately prior to the Equity Financing, the KISS(Debt) instruments have already converted? If we take this position, then we would include it.)

#### The Safe(Cap) and Safe(Cap, No Discount) include KISS(Equity)

Does a KISS(Equity) count as a warrant or other convertible security?

If it does, then it is included in the computation of company capitalization.

If it does not, then it is excluded.

A warrant entitles the owner to buy shares at a fixed price. But all the money in a KISS(Equity) and a Safe is paid up front, so it's not a warrant. It's a lot like a warrant, so says everybody, but it's not exactly a warrant.

It's not debt, either, because it's not repayable. (The KISS(Debt) is repayable, so that's a convertible promissory note. But we're not talking about KISS(Debt) right now, we're talking about KISS(Equity).)

I guess that makes the KISS(Equity) an "other convertible security".

So the Safe has to count KISS(Equity) conversion as part of its fully diluted capitalization.

Arguably it shouldn't do, because if you take a step back, Safes and KISSes are both seed-stage instruments hired to do the same job, and they expect to all convert simultaneously upon the next equity round. But a literalist reading of the Safe has the KISS(Equity) included in the fully-diluted capitalization, so that's that.

### Does the KISS include Safes in the fully diluted capitalization?

The KISS defines Fully Diluted Capitalization:

    “Fully-Diluted Capitalization” shall mean the number of shares of outstanding Common Stock of the Company on a fully-diluted basis, including

    (i) conversion or exercise of all securities convertible into or exercisable for Common Stock,

    (ii) exercise of all outstanding options and warrants to purchase Common Stock and,

    in the case of Section 1(b)(i) and 1(b)(iii) only,
    (iii) the shares reserved or authorized for issuance under the Company’s existing stock option plan or any stock option plan created or increased in connection with such transaction;

    but excluding, for this purpose, the conversion contemplated by the applicable provision of Section 2.

So, from the point of view of the KISS, is a Safe a security convertible into Common Stock? Well, it depends. If the startup gets bought, then the Safe could convert to Common. If the startup goes to Series A, though, the Safe converts to Preferred. And presumably the above clause of the KISS is being read in the context of a Series A financing, which triggers the conversion of all outstanding KISS and Safe instruments at the same time.

Let us analyze the situation by examining different temporal sequences.

If the Safe converts before the KISS, then the shares to which the Safe converted would form part of the fully-diluted capitalization.

If the Safe is deemed to convert simultaneously with the conversion of the KISS, or even to convert immediately after the KISS converts, then it is, for the purposes of this paragraph, not a security convertible into Common Stock, because its fate was already sealed by the time the KISS even began to contemplate conversion, by the fact of the Series A occurring.

See also
https://bothsidesofthetable.com/what-all-entrepreneurs-need-to-know-about-prorata-rights-e5883fd21f80

If I had to make a call, I would say that the Safe converts simultaneously with the KISS, and does not convert to Common, and so should not be included in the FDC.

Think about it: if the investor had used a Safe instead of a KISS, then it would be excluded, because siblings Safes exclude one another.

If all the investors had used KISSes instead of Safes, then they would all be excluded, because sibling KISSes exclude one another.

It doesn't make sense that just switching instruments should cause an unexpected bump to the FDC.

### Does the KISS include other KISSes in the fully diluted capitalization?

No, for a couple reasons.

First, because all the KISSes are converting to Preferred, so they don't count as securities convertible into Common Stock.

Second, because the "but excluding, for this purpose" line excludes the current KISS.

Third, because most convertible instruments exclude themselves and their classmates from the capitalization computation. Safes do explicitly. KISSes don't make the exclusion explicit, but exclusion would be consistent with standard practice.

### Mutually Recursive Definitions

For the sake of completeness we will discuss a situation which does not actually arise between Safe and KISS.

The most complicated scenario has two instruments, X and Y, which refer to each other.

X's notion of fully diluted capitalization includes the conversion of Y to stock.

Y's notion of fully diluted capitalization includes the conversion of X to stock.

We see that we have a system of simultaneous equations with two unknowns. It is possible to solve these equations also.

We begin with the following observations:

```
; number of conversion shares resulting from conversion of instrument X
conv_x = value_x / price_x

; price per share for conversion of X
price_x = valcap_x / fdc_x

; fully diluted capitalization from X's point of view
fdc_x = existing + conv_y

; note that we assume that X considers the FDC to include conversion of Y.
; if X did NOT include Y then we could stop here.

conv_y = value_y / price_y

price_y = valcap_y / fdc_y

fdc_y = existing + conv_x
```

Rearranging to isolate conv_x, we end up with

```
conv_x =   ( value_x * existing ) ( value_y + valcap_y )
         / ( valcap_x * valcap_y - value_x * value_y )
```

and once we have `conv_x` we can get `conv_y` from the formulas above.

### Constraints

The product of the valuation caps must be greater than the product of the amounts invested.

If it is not, you will get a negative number of conversion shares.

This is evident from inspection of the denominator of the `conv_x` formula above.

# Comments

## Transitivity

Is a convertible security convertible to Common Stock if it needs to first be converted into some other security?

    X -> Preferred -> Common

Is X convertible to Common?

