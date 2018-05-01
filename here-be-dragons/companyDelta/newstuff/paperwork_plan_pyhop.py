import pyhop
from typing import NamedTuple, FrozenSet, NewType, Optional, TypeVar

T = TypeVar('T')
def fset(*args:T) -> FrozenSet[T]:
	return FrozenSet(args)

class EntityNature(Enum):
	human = 'human'
	corporate = 'corporate'
	ai = 'ai'

class Gender(Enum):
	male = 'male'
	female = 'female'
	neutral = 'neutral'

class Party(NamedTuple):
	name: str
	idtype: str
	idnum: str
	entityNature: EntityNature
	gender: Gender

Measure = NewType('Measure',str)
byUnit: Measure = 'byUnit'
byMoney: Measure = 'byMoney'
def otherMeasure(s:str) -> Measure:
	return cast(Measure,s)

class Security(NamedTuple):
	name: str
	measure: Measure

class Company(NamedTuple):
	name: str
	jurisdiction: str
	address: str
	idType: str
	idNum: str
	constitution: str # treating this as a single string for now
	directors: FrozenSet[Party]
	secretary: Party
	# directorNames: FrozenSet[str]
	# secretaryName: str


class Holding(NamedTuple):
	party: Party
	# partyName: str
	holds: FrozenSet[HeldSecurity]


class HeldSecurity(NamedTuple):
	security: Security
	units: Optional[SecurityUnits]
	money_spent: Optional[float]
	description Optional[str]

class Agreement(NamedTuple):
	title: str
	body: str
	parties: List[Party]
	# partyNames: List[str]
	# level ??

class CompanyState(NamedTuple):
	company: Company
	agreements: FrozenSet[Agreement]
	shareHolderAgreement: Optional[Agreement]
	holdings: FrozenSet[Holding]
	securities: FrozenSet[Security]
	parties: FrozenSet[Party]



def shareHolderAgreement(company:Company, investor:Party):
	return Agreement("ShareholdersAgreement",
					 "...shareholder agreement body",
					 )

def addExistingInvestor(cs:CompanyState, investor:str, date:str):


def fromTo(src:CompanyState, trg:CompanyState):

