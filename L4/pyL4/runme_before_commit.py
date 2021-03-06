import os
import sys
import time
from typing import Dict, Optional, Set

from src.independent.util import print_all_todos

# it would be nice to have an sys.argv parser library generalize this to select tests_to_run
tests_to_run : Set[str] = { 'nlg' } if "nlg" in sys.argv else {
    'L4typechecker',
    # 'smtlegacy',
    'symbexec',
    'graphviz',
    'prettyprint',
    'nlg',
    'interpreter',
}


def runit(s, optional_s=""):
    line2 = f"Running {optional_s} `{s}`"
    print(f"\n{'='*len(line2)}\n{line2}")
    os.system(s)

# it would be nice to have a sys.argv parser library allow us to do --verbose or -v
# VERBOSE = True
VERBOSE = True

if not "onlytc" in sys.argv and not "tconly" in sys.argv:
    splits : Dict[str,float] = {}
    last = ""
    def timetask_start(label:str):
        global last
        splits[label] = time.process_time()
        last = label
    def timetask_stop(maybelabel:Optional[str] = None):
        label = maybelabel if maybelabel else last
        splits[label] = time.process_time() - splits[label]
    def show_splits():
        print()
        for label in splits:
            print(label, "time:", str(splits[label]*10)[0:4])

    timetask_start('total')

    timetask_start("parse")
    from test import test_parser
    # assert test_parser.EXAMPLES_FULL_SIZE == len(test_parser.EXAMPLES), f"Some entries of cli.EXAMPLES are commented out (count {len(test_parser.EXAMPLES)}), or you need to change cli.EXAMPLES_FULL_SIZE (count {test_parser.EXAMPLES_FULL_SIZE})"
    progs = test_parser.main(keep=True,verbose=False)
    timetask_stop()

    if 'nlg' in tests_to_run:
        timetask_start('nlg')
        from test import test_nlg
        test_nlg.main(progs)
        timetask_stop()

    if 'interpreter' in tests_to_run:
        timetask_start('interpreter')
        from test import test_interpreter
        # print(test_interpreter.EXAMPLES_TO_RUN)
        # assert test_interpreter.EXAMPLES_FULL_SIZE == len(test_interpreter.EXAMPLES_TO_RUN), f"Some entries of " \
        #         f"test_interpreter.EXAMPLES_TO_RUN are probably commented out... {test_interpreter.EXAMPLES_FULL_SIZE} {len(test_interpreter.EXAMPLES_TO_RUN)}"
        test_interpreter.main(progs, VERBOSE)
        timetask_stop()

    if 'L4typechecker' in tests_to_run:
        timetask_start('L4typechecker')
        from test import test_typechecker
        test_typechecker.main(progs, VERBOSE)
        timetask_stop()

    if 'graphviz' in tests_to_run:
        timetask_start('graphviz')
        from test import test_graphviz
        test_graphviz.main(progs, VERBOSE)
        timetask_stop()

    if 'prettyprint' in tests_to_run:
        timetask_start('prettyprint')
        from test import test_prettyprint
        test_prettyprint.main(progs)
        timetask_stop()

    if 'symbexec' in tests_to_run:
        timetask_start('symbexec')
        from test import test_symbolic_exec
        test_symbolic_exec.main(progs)
        timetask_stop()

    if 'smtlegacy' in tests_to_run:
        timetask_start('smt-legacy')
        from test import test_smt
        test_smt.main(progs)
        timetask_stop()

    timetask_stop('total')

    print_all_todos()

    show_splits()

# runit("export MYPYPATH=.; mypy --ignore-missing-imports src", "typechecker")
runit("export MYPYPATH=.; mypy runme_before_commit.py test src", "typechecker")



