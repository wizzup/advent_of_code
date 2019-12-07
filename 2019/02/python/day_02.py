#####################################################################
# https://adventofcode.com/2019/day/2
#####################################################################

from enum import Enum


def read_mem(inp):
    return [int(i) for i in inp.split(',')]


class OpCode(Enum):
    OpAdd = 1
    OpMul = 2
    OpHalt = 99

    def __str__(self):
        if self.name() == 'OpAdd':
            return "+"
        if self.name() == 'OpMul':
            return "*"
        if self.name() == 'OpHalt':
            return "H"
        return None

class Oper():
    def __init__(self, opcode, *args):
        self.opcode = opcode
        largs = len(args)
        self.fst = args[0] if largs >= 1 else None
        self.snd = args[1] if largs >= 2 else None
        self.trd = args[2] if largs >= 3 else None

    def __repr__(self):
        if self.opcode in [OpCode.OpAdd, OpCode.OpMul]:
            return f'{self.fst:>4} {self.opcode} {self.snd:>4} -> {self.trd:>4}'
        if self.opcode in [OpCode.OpHalt]:
            return f'{self.opcode}'
        return None


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


def run_step(mem, pc):
    '''
    run single step from  memory at program counter
    '''
    nxt = 0
    halt = False
    opr = parse(mem[pc:])
    # print(f'pc: {pc}, op: {op}, mem: {mem[pc:pc+5]}')
    if not opr:
        print(f'error pc: {pc} mem {mem[pc:pc+10]}')
        return

    if opr.opcode == OpCode.OpAdd:
        nxt = 4
        mem[opr.trd] = mem[opr.fst] + mem[opr.snd]
    if opr.opcode == OpCode.OpMul:
        nxt = 4
        mem[opr.trd] = mem[opr.fst] * mem[opr.snd]
    if opr.opcode == OpCode.OpHalt:
        halt = True

    return (pc + nxt, halt)


def run(mem):
    '''
    run entire memory
    '''
    nxt = 0
    while True:
        nxt, hlt = run_step(mem, nxt)
        if hlt:
            break

    return mem


def tests():
    inp_1 = "1,0,0,3,99"
    inp_2 = "1,9,10,3,2,3,11,0,99,30,40,50"
    inp_3 = "1,0,0,0,99"
    inp_4 = "2,3,0,3,99"
    inp_5 = "2,4,4,5,99,0"
    inp_6 = "1,1,1,4,99,5,6,0,99"

    out_1 = "1,0,0,2,99"
    out_2 = "3500,9,10,70,2,3,11,0,99,30,40,50"
    out_3 = "2,0,0,0,99"
    out_4 = "2,3,0,6,99"
    out_5 = "2,4,4,5,99,9801"
    out_6 = "30,1,1,4,2,5,6,0,99"

    inps = [inp_1, inp_2, inp_3, inp_4, inp_5, inp_6]
    outs = [out_1, out_2, out_3, out_4, out_5, out_6]

    def test(inp, out):
        assert run(read_mem(inp)) == read_mem(out)

    for case in zip(inps, outs):
        test(*case)


def run_with(p, a, b):
    mem = read_mem(p)
    mem[1] = a
    mem[2] = b
    out = run(mem)[0]
    return out


def part_1(inp):
    return run_with(inp, 12, 2)


def part_2(inp):
    rng = range(100)
    cases = [(x, y) for x in rng for y in rng]
    for noun, verb in cases:
        out = run_with(inp, noun, verb)
        if out == 19690720:
            return 100 * noun + verb


def main():
    inp = input()
    print(f'part_1 : {part_1(inp)}')
    print(f'part_2 : {part_2(inp)}')


tests()
main()


