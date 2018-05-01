





*****************************************************************************************************
WAIT TO HEAR FROM MENG. I DON'T KNOW ENOUGH ABOUT THE TYPES OF TRANSITIONS TO WORK ON THIS RIGHT NOW.
*****************************************************************************************************








from queue import Queue

# haven't yet taken into account form ordering constraints.
# ah, that can be done using PathSensitiveNeigbourExcluder

FORMS = [
	"shareholder approval",
	"pro rata rights",
	"shareholder approval", "director resolutions",
	"actual investment agreements",
	"ratify DORA"
]

# A company state (CS) is a just a json structure (in Python) that matches the datatype definition in Company.hs.
# We don't actually need a representation of the type in Python.

# these can be type parameters later:
CS = NewType('CS',Any)
FormId = NewType('FormId',str)
FilledForm = NewType('FilledForm',Any)
Form = NewType('Form',Any)

class Path(NamedTuple):
	nodes: Tuple[CS]
	edges: Tuple[FilledForm]

class PartialSoln(NamedTuple):
	path: Path,
	dest: CS

class Form(NamedTuple):
	titleid: FormId,
	gen: Callable[[PartialSoln], Iterable[CS]]

FormExcluder = Callable[[PartialSoln, FormId], bool]

CSExcluder = Callable[[PartialSoln, CS], bool]

# just BFS for now
# issue: this won't really consider multiple paths from src to dest
def findPath(src: CS, dest: CS,
			 forms: Iterable[Form],
			 cs_excluder: CSExcluder) -> Iterable[CS]:

	G = DirectedGraph()
	seen : Set[CS] = set([src])
	# explored_or_exploring : Set[CS] = set()
	to_explore_queue : Queue[CS] = Queue([src])
	addedby : Dict[CS,CS]

	found = False
	def addNeighbors(partsoln:PartialSoln) -> Iterable[CS]:
		interm = partsoln.path.nodes[-1]
		nonlocal found
		for form in forms:
			for u in form.gen(partsoln):
				if u not in seen and not cs_excluder(partsoln, u):
					seen.add(u)
					addedby[u] = interm
					if u == dest:
						print("found a shortest path")
						found = True
					to_explore_queue.push(u)

	def path_from_addedby() -> List[CS]:
		revpath = [dest]
		while True:
			cur = revpath[-1]
			if cur == src:
				break
			else:
				revpath.append(addedby[cur])
		return revpath.reverse()

	while len(to_explore_queue) > 0 and not found:
		interm = to_explore_queue.pop()
		addNeighbors(interm)

	return path_from_addedby()

