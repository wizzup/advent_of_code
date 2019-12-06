#####################################################################
# https://adventofcode.com/2019/day/2
#####################################################################

from enum import Enum


def read_mem(inp):
    return [int(i) for i in inp.split(',')]

test1 = "1,0,0,3,99"
test2 = "1,9,10,3,2,3,11,0,99,30,40,50"

# mem = read_mem(input())
# mem = read_mem(test1)
# mem = read_mem(test2)
# print(mem)

class OpCode(Enum):
    OpAdd = 1
    OpMul = 2
    OpHalt = 99

    def __str__(self):
        ret = None
        if self.name == 'OpAdd':
            return "+"
        if self.name == 'OpMul':
            return "*"
        if self.name == 'OpHalt':
            return "H"

class Oper():
    def __init__(self, opcode, *args):
        self.opcode = opcode
        largs = len(args)
        self.b = args[0] if largs >= 1 else None
        self.c = args[1] if largs >= 2 else None
        self.d = args[2] if largs >= 3 else None

    def __repr__(self):
        if self.opcode in [OpCode.OpAdd, OpCode.OpMul]:
            return f'{self.b:>4} {self.opcode} {self.c:>4} -> {self.d:>4}'
        if self.opcode in [OpCode.OpHalt]:
            return f'{self.opcode}'

def parse(xs):
    op = None
    a = xs[0]
    if a == 1:
        [b,c,d,*_] = xs[1:]
        op = Oper(OpCode.OpAdd, b, c, d)
    if a == 2:
        [b,c,d,*_] = xs[1:]
        op =  Oper(OpCode.OpMul, b, c, d)
    if a == 99:
        op =  Oper(OpCode.OpHalt)

    if not op:
        print(f'error uknown op: {a}')
    return op

def run_step(pc):
    next = 0
    halt = False
    op = parse(mem[pc:])
    print(f'pc: {pc}, op: {op}, mem: {mem[pc:pc+5]}')
    if not op:
        print(f'error pc: {pc} mem {mem[pc:pc+10]}')
        return

    if op.opcode == OpCode.OpAdd:
        next = 4
        mem[op.d] = mem[op.b] + mem[op.c]
    if op.opcode == OpCode.OpMul:
        next = 4
        mem[op.d] = mem[op.b] * mem[op.c]
    if op.opcode == OpCode.OpHalt:
        halt = True

    return (pc + next, halt)

def run():
    n = 0
    while True:
        n,c = run_step(n)
        if c:
            break

    return mem[0]

def part_1():
    global mem
    mem = read_mem(input())
    mem[1] = 12
    mem[2] = 2
    print(f'ans: {run()}')

part_1()
