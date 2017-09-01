# Voting with delegation and delegation-cycle breaking
# This is based on @DavidKnott's viper/examples/voting/ballot.v.py and 
# https://solidity.readthedocs.io/en/develop/solidity-by-example.html#voting 
# but diverges significantly due to the constraint of bounded gas cost.

# NOT YET TESTED AND PROBALY HAS BUGS.

# Information about voters
voters: public({    
    # `chairperson` called `give_right_to_vote` for this voter
    can_vote : bool,
    # if true, this voter's direct or deligated vote has been counted. initially False.
    counted : bool, 
    # person delegated to, if they called `delegate`. the initial value of 
    # '0x0000000000000000000000000000000000000000' means they have not delegated.    
    delegate: address,     
    # index of the voted proposal, _if_ they directly voted by calling `vote`. 
    # -1 means they haven't directly voted, but possibly they delegated.
    direct_vote: num
}[address])

# This is a type for a list of proposals.
proposals: public({
    # short name (up to 32 bytes)
    name: bytes32,
    # number of accumulated votes
    vote_count: num
}[num])

voter_count: public(num)
chairperson: public(address)

# Setup global variables
def __init__(_proposalNames: bytes32[5]):
    self.chairperson = msg.sender
    self.voter_count = 0
    for i in range(1,5):
        self.proposals[i] = {
            name: _proposalNames[i],
            vote_count: 0
        }

@constant
def hasDirectlyVoted(addr: address) -> bool:
    return self.voters[addr].direct_vote != -1

@constant
def hasDelegated(addr: address) -> bool:
    return self.voters[addr].delegate != 0x0000000000000000000000000000000000000000

@constant
def hasActed(addr: address) -> bool:    
    return self.hasDirectlyVoted(addr) or self.hasDelegated(addr)
    
    
# Give an address the right to vote on this ballot.
# May only be called by `chairperson`.
def give_right_to_vote(addr_of_new_voter: address):    
    # Throws if sender is not chairpers
    assert msg.sender == self.chairperson
    # We use 0x0000000000000000000000000000000000000000 as a flag, so 
    # throw if try to give voting right to it
    assert addr_of_new_voter != 0x0000000000000000000000000000000000000000
    # Throws if already voted or delegated
    assert not self.hasActed(addr_of_new_voter)
    self.voter_count += 1   
    self.voters[addr_of_new_voter].can_vote = True
    # This means they haven't directly voted. The default inital value 
    # of 0 would mean they directly voted for proposal 0.
    self.voters[addr_of_new_voter].direct_vote = -1


# Delegate your vote to the voter at `to_addr`.
def delegate(to_addr: address):    
    # Throws if sender has already directly voted or delegated
    assert not self.hasActed(msg.sender)    
    # Throws if sender tries to delegate their vote to themselves
    assert not msg.sender == to_addr
    # Throws if delegate or sender has not been given the right to vote
    assert self.voters[msg.sender].can_vote and self.voters[to_addr].can_vote
    
    self.voters[msg.sender].delegate = to_addr

# If u has an uncounted delegation to v, and v has not acted or also has an uncounted delegation, then u can change their delegation to another voter w, provided w either directly voted or has a counted delegation. This allows for two things, with (1) being its purpose and (2) being a side effect.
# (1) breaking cycles, and 
# (2) giving a voter one more (less flexible) chance to redeligate their vote if the person they originally delegated to hasn't voted or delegated in a way that results in a vote.
def change_delegation(w_addr: address):
    # Throws if sender has NOT delegated
    assert self.hasDelegated(msg.sender)        
    # Throws if sender tries to redelegate their vote to themselves    
    assert not msg.sender == w_addr
    u = self.voters[msg.sender]
    # Throws if sender u's delegation has already been counted
    assert not u.counted
    w = self.voters[w_addr]
    # Throws if new delegate w has not been given the right to vote
    assert w.can_vote
    v_addr = u.delegate
    v = self.voters[v_addr]

    # Throws if v doesn't meet the requirements mentioned in the function documentation
    assert (not self.hasActed(v_addr)) or (self.hasDelegated(v_addr) and (not v.counted))

    # Throws if w doesn't meet the requirements mentioned in the function documentation
    assert (self.hasDirectlyVoted(w_addr)) or (self.hasDelegated(w_addr) and w.counted)

    u.delegate = w_addr

# This function isn't necessary for breaking cycles, but it'd be weird to have change_delegation and not have this.
# The requirements on the sender u and the voter v that the sender currently delegates to are the same as in change_delegation.
def cancel_delegation_and_vote(proposal_ind: num):
    # Throws if sender has NOT delegated
    assert self.hasDelegated(msg.sender)            
    u = self.voters[msg.sender]
    # Throws if sender u's delegation has already been counted
    assert not u.counted
    v_addr = u.delegate
    v = self.voters[v_addr]

    # Throws if v doesn't meet the requirements mentioned in the function documentation
    assert not self.hasActed(v_addr) or (self.hasDelegated(v_addr) and not v.counted)

    # the remaining three operations are the same as in `vote`
    u.counted = True
    u.direct_vote = proposal_ind
    # If `proposal` is out of the range of the array, this will throw 
    # automatically and revert all changes.
    self.proposals[proposal_ind].vote_count += 1
    
# Directly vote on `proposals[proposal_ind].name`. _Eventually_ this 
# will result in votes delegated to you being counted also.
def vote(proposal_ind: num):
    # Throws if sender has already voted or delegated
    assert not self.hasActed(msg.sender)    

    self.voters[msg.sender].counted = True
    self.voters[msg.sender].direct_vote = proposal_ind
    # If `proposal` is out of the range of the array, this will throw 
    # automatically and revert all changes.
    self.proposals[proposal_ind].vote_count += 1

# If voter u has delegated to v, and v has 
# submitted their vote, and u has not had their vote counted, 
# then this will count's u's vote.
# Note that this can be called by anyone to move the contract forward.
def count_delegated_vote(u_addr:address):
    assert self.hasDelegated(u_addr)
    u = self.voters[u_addr]
    assert not u.counted
    v_addr = u.delegate
    assert self.hasDirectlyVoted(v_addr)
    v = self.voters[v_addr]
    # If `v.ballot_ind` is out of the range of the array,
    # this will throw automatically and revert all
    # changes.    
    proposal = self.proposals[v.direct_vote]    
    u.counted = True
    proposal.vote_count += 1    

# If voter u has an uncounted delegation to some voter v, and v has a counted
# delegation to a voter w who has submitted a ballot, then we move u's delegation 
# from v to w. 
# We then might as well call count_delegated_vote on u.
# Note that this can be called by anyone to move the contract forward.
def shorten_delegation_chain(u_addr:address):
    assert self.hasDelegated(u_addr)
    u = self.voters[u_addr]
    assert not u.counted
    v_addr = u.delegate
    assert self.hasDelegated(v_addr)    
    v = self.voters[v_addr]
    assert v.counted
    w_addr = v.delegate
    assert self.hasDirectlyVoted(w_addr)

    u.delegate = w_addr
    self.count_delegated_vote(u_addr)


# Computes the winning proposal taking only counted votes into account.
@constant
def winning_proposal_from_counted_votes() -> num:
    winning_vote_count = 0
    for i in range(5):
        if self.proposals[i].vote_count > winning_vote_count:
            winning_vote_count = self.proposals[i].vote_count
            winning_proposal = i
    return winning_proposal

# Calls winning_proposal() function to get the index
# of the winner contained in the proposals array and then
# returns the name of the winner
@constant
def current_winner_name() -> bytes32:
    return self.proposals[self.winning_proposal_from_counted_votes()].name