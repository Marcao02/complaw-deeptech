# The Schema Extractor

What is a _valid_ JSON object, for the purposes of a given cftemplate?

A _valid_ JSON object contains all the information needed to fill a cftemplate without leaving any blanks behind.

A cftemplate may have a control variable which switches on or off a data variable. If the data variable is switched off by the control variable, then that data variable may be safely omitted from the input JSON object; the JSON will still be valid.

It is possible to reduce any given cftemplate to a _dependency signature_, which can be used to validate any given JSON object. The dependency signature includes all of the control logic and all of the blanks, but no other verbiage.

Why is this more useful than just filling the cftemplate and then rendering the form to see what blanks remain? Because maybe we want to run the validation on the client side, in addition to validating the form on the server side, and we don't want to push the whole cftemplate to the client.

In the short term though it would be okay to just fill the cftemplate with whatever the user provides, then fill the form using the same, and see if any blanks remain.

# Computing the Dependencies

Given a cftemplate, extract the dependency signature.

A cftemplate has logic: the blanks that need filling (data variables) may depend on some of the other filled blanks (control variables).

By contrast, once a cftemplate has been Controlled, the resulting commonform has no logic: every commonform requires the same blanks to be filled.

The logical structure of a cftemplate, by example:
```Blanks1 ...
if-block Exp1 begin
   Blanks2
   if-block ... end
end
Blanks3
```

Blanks1 and Blanks3 occur at the top level and are therefore mandatory.

If-blocks are conditional on `Exp`ressions, and contain passages of text which may define Blanks2, and possibly recursive if-blocks within them.

(If-block includes both if/begin/end and unless/begin/end constructs.)

The Blanks in the body of an if-block depend on the controlling Exp. A Blank that appears only within the body of an if block is _maybe optional_.

If the controlling Exp evaluates to false, then the body is not reached, the Blanks are never filled, and those values can be omitted from the input JSON: they are optional.

As a special case, if we observe that the same Exp is used twice, in both `if` and `unless` blocks occurring at the same level, we can infer that the dependent Blanks are mandatory within the scope of those if/unless block.



