

*****************************************************************************************************
WAIT TO HEAR FROM MENG. I DON'T KNOW ENOUGH ABOUT THE TYPES OF TRANSITIONS TO WORK ON THIS RIGHT NOW.
*****************************************************************************************************

# Could also use JSON schema
'''
(type CompanyState (parties {Party})
				   (securities {Security})
                   (company     Company)
                   (holdings   {Holding})
                   (agreements {Contract})
)

(type Party {  (fullname PartyName),
               (idtype   String),
               (idnum    String),
               (nature   EntityNature),
               (gender   Gender)
             }
)

'''


""" start at json schema...
{
	"title": "CompanyState",
	"type": "object",
	"properties": {
		"parties": {},
		"securities": {},
		"company":

	}
}
"""