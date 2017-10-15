# Voting with delegation and delegation-cycle breaking
# This is based on @DavidKnott's viper/examples/voting/ballot.v.py and
# https://solidity.readthedocs.io/en/develop/solidity-by-example.html#voting
# but diverges significantly due to the constraint of bounded gas cost.

# NOT YET TESTED AND PROBALY HAS BUGS.

# Information about voters
voters: public({
    # `chairperson` called `give_right_to_vote` for this voter
    can_vote : bool,
    # if true, this voter's direct or delegated vote has been counted. initially False.
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
    for i in range(5):
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
    # Throws if sender is not chairperson
    assert msg.sender == self.chairperson
    # We use 0x0000000000000000000000000000000000000000 as a flag, so
    # throw if try to give voting right to it
    assert addr_of_new_voter != 0x0000000000000000000000000000000000000000
    # Throws if already voted or delegated
    assert not self.hasActed(addr_of_new_voter)
    self.voter_count += 1
    self.voters[addr_of_new_voter].can_vote = True
    # This means they haven't directly voted. The default inital value
    # of 0 (for num type) would mean they directly voted for proposal 0.
    self.voters[addr_of_new_voter].direct_vote = -1


# Delegate your vote to the voter at `to_addr`.
def delegate(to_addr: address):
    # Throws if delegate or sender has not been given the right to vote
    assert self.voters[to_addr].can_vote and self.voters[msg.sender].can_vote
    # Throws if sender has already directly voted or delegated
    assert not self.hasActed(msg.sender)
    # Throws if sender tries to delegate their vote to themselves
    assert not msg.sender == to_addr

    self.voters[msg.sender].delegate = to_addr

# Directly vote on `proposals[proposal_ind].name`. _Eventually_ this
# will result in votes delegated to `msg.sender` being counted also.
def vote(proposal_ind: num):
    # Throws if delegate or sender has not been given the right to vote
    assert self.voters[to_addr].can_vote and self.voters[msg.sender].can_vote
    # Throws if sender has already voted or delegated
    assert not self.hasActed(msg.sender)

    self.voters[msg.sender].counted = True
    self.voters[msg.sender].direct_vote = proposal_ind
    # If `proposal_ind` is out of the range of the array,
    # this will throw automatically and revert all changes.
    self.proposals[proposal_ind].vote_count += 1

# If voter u has delegated to v, and v has
# directly voted, and u has not had their vote counted,
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