-- Action <2016-11-17> <Notice> {Sender: ...; Receiver: ...; Content: ...; Content-Type: ...}
-- 

data Date -- E.g. 2016-11-17


-- 1. Financial instruments
-- 1.1. maturity date passes

data Action
data IntervalSpec
data Timestamp = AbsoluteTime Date | RelativeTime Action IntervalSpec

-- 1.2. person sends some money to another person

newtype Person = Person String
data Payment  = Payment { amount :: Double, from :: Person, to :: Person }

-- 2. Sales between individuals of physical assets like sugar
-- 2.1. Alice: i ran out of sugar, sorry, lol

type Sender = Person
type Recipient = Person
data Notice = Notice Sender Recipient [Content]
data Content = InvitationToTreat | Offer | Acceptance | Assertion Proposition | Dispute Proposition




-- 2.2. Bob: I offer to sell you up to 10 kilos of sugar for $price per kilo
-- 2.3. Alice: I accept your offer, I will take 2 kilos pls kthnx
-- 2.4. Bob: Please send $price * 2 to this bank account
-- 2.5. Alice: I haz done it
-- 2.6. Bob: I will transfer the sugar now
-- 2.7. Alice: I haz received it kthnx
-- 2.8. Bob: we're done.



-- 3. companies
-- 3.1 directors pass a resolution
-- 3.2. members hold an EGM or AGM
-- 3.3. pass a resolution
