#####################################################################
# https://adventofcode.com/2019/day/5
#####################################################################

from enum import Enum


def read_mem(inp):
    return [int(i) for i in inp.split(',')]


class OpCode(Enum):
    OpAdd = 1
    OpMul = 2
    OpInp = 3
    OpOut = 4
    OpJnz = 5
    OpJz = 6
    OpLt = 7
    OpEq = 8
    OpHalt = 99

    def __str__(self):
        rs = {'OpAdd': "+",
              'OpMul': "*",
              'OpInp': "I",
              'OpOut': "O",
              'OpJz': "Jz",
              'OpJnz': "Jn",
              'OpLt': "<",
              'OpEq': "=",
              'OpHalt': "H"}
        return rs[self.name]

    def __format__(self, _):
        return str(self)


class Mode(Enum):
    Position = 0
    Immediate = 1

    def __str__(self):
        rs = {'Position': "@",
              'Immediate': "i"}
        return rs[self.name]

    def __repr__(self):
        return self.__str__()


class Param():
    def __init__(self, mode, value):
        self.mode = mode
        self.value = value

    def get(self, mem):
        # print(f'Param.get {self.mode} {self.value}')
        p, i = Mode.Position, Mode.Immediate

        def get_mem():
            return mem[self.value]

        def get_im():
            return self.value

        ret = {p: get_mem,
               i: get_im}
        return ret[self.mode]()

    def __str__(self):
        return f'{self.mode}{self.value}'

    def __format__(self, _):
        return str(self)


class Oper():
    def __init__(self, opcode, *args):
        self.opcode = opcode
        largs = len(args)
        self.fst = args[0] if largs >= 1 else None
        self.snd = args[1] if largs >= 2 else None
        self.trd = args[2] if largs >= 3 else None

    def dbg(self, mem):
        def get(x):
            p, i = Mode.Position, Mode.Immediate
            ret = {p: f'{x:>4}[{x.get(mem)}]',
                   i: f'{x:>4}[{x.get(mem)}]'}
            return ret[x.mode]

        if self.opcode in [OpCode.OpAdd, OpCode.OpMul,
                           OpCode.OpLt, OpCode.OpEq]:
            return (f'{get(self.fst)} {self.opcode} {get(self.snd)}'
                    f' -> {get(self.trd)}')
        if self.opcode in [OpCode.OpJnz, OpCode.OpJz]:
            return f'{self.opcode} {get(self.fst)} {get(self.snd)}'
        if self.opcode in [OpCode.OpInp, OpCode.OpOut]:
            return f'{self.opcode} {get(self.fst)}'
        if self.opcode in [OpCode.OpHalt]:
            return f'{self.opcode}'
        assert False

    def __str__(self):
        if self.opcode in [OpCode.OpAdd, OpCode.OpMul,
                           OpCode.OpLt, OpCode.OpEq]:
            return (f'{self.fst:>4} {self.opcode} {self.snd:>3}'
                    f' -> {self.trd:>3}')
        if self.opcode in [OpCode.OpJnz, OpCode.OpJz]:
            return f'{self.opcode} {self.fst} {self.fst}'
        if self.opcode in [OpCode.OpInp, OpCode.OpOut]:
            return f'{self.opcode} {self.fst:>2}'
        if self.opcode in [OpCode.OpHalt]:
            return f'{self.opcode}'
        assert False


def parse_mode(m):
    # print('parse_mode')
    p, i = Mode.Position, Mode.Immediate
    ms = str(m)
    mds, ops = ms[:-2], ms[-2:]  # mode, opcode
    mdk, mdv = ['0', '1'], [p, i]
    keys = [''.join(a+b+c).lstrip('0') for a in mdk for b in mdk for c in mdk]
    vals = [(c, b, a) for a in mdv for b in mdv for c in mdv]
    mdmap = dict(zip(keys, vals))
    return (int(ops), mdmap[mds])


def parse(xs):
    # print('parse')
    op = None
    a = xs[0]

    (op, modes) = parse_mode(a)
    # print(f'parse op:{op}, modes:{modes}, mem:{xs[:4]}')

    if op == 99:
        return Oper(OpCode.OpHalt)

    if op == 1:
        [b, c, d, *_] = xs[1:]
        b = Param(modes[0], b)
        c = Param(modes[1], c)
        d = Param(modes[2], d)
        return Oper(OpCode.OpAdd, b, c, d)
    if op == 2:
        [b, c, d, *_] = xs[1:]
        b = Param(modes[0], b)
        c = Param(modes[1], c)
        d = Param(modes[2], d)
        return Oper(OpCode.OpMul, b, c, d)
    if op == 3:
        [b, *_] = xs[1:]
        b = Param(modes[0], b)
        return Oper(OpCode.OpInp, b)
    if op == 4:
        [b, *_] = xs[1:]
        b = Param(modes[0], b)
        return Oper(OpCode.OpOut, b)
    if op == 5:
        [b, c, d, *_] = xs[1:]
        b = Param(modes[0], b)
        c = Param(modes[1], c)
        return Oper(OpCode.OpJnz, b, c)
    if op == 6:
        [b, c, d, *_] = xs[1:]
        b = Param(modes[0], b)
        c = Param(modes[1], c)
        return Oper(OpCode.OpJz, b, c)
    if op == 7:
        [b, c, d, *_] = xs[1:]
        b = Param(modes[0], b)
        c = Param(modes[1], c)
        d = Param(modes[2], d)
        return Oper(OpCode.OpLt, b, c, d)
    if op == 8:
        [b, c, d, *_] = xs[1:]
        b = Param(modes[0], b)
        c = Param(modes[1], c)
        d = Param(modes[2], d)
        return Oper(OpCode.OpEq, b, c, d)

    if not op:
        print(f'error uknown op: {a}')

    print(f'unknown operation {op}')
    assert False


def run_step(mem, out, inp, pc):
    # print('run_step')
    done = False
    halt = False

    opr = parse(mem[pc:])
    # print(f'pc: {pc}, op: {opr.dbg(mem):<18}, mem:{mem[pc:pc+4]}')
    if not opr:
        print(f'error pc: {pc} mem {mem[pc:pc+10]}')
        return

    if opr.opcode == OpCode.OpHalt:
        halt = True
        done = True
    if opr.opcode == OpCode.OpAdd:
        pc += 4
        mem[opr.trd.value] = opr.fst.get(mem) + opr.snd.get(mem)
        done = True
    if opr.opcode == OpCode.OpMul:
        pc += 4
        mem[opr.trd.value] = opr.fst.get(mem) * opr.snd.get(mem)
        done = True
    if opr.opcode == OpCode.OpLt:
        pc += 4
        mem[opr.trd.value] = 1 if opr.fst.get(mem) < opr.snd.get(mem) else 0
        done = True
    if opr.opcode == OpCode.OpEq:
        pc += 4
        mem[opr.trd.value] = 1 if opr.fst.get(mem) == opr.snd.get(mem) else 0
        done = True
    if opr.opcode == OpCode.OpJnz:
        pc = opr.snd.get(mem) if opr.fst.get(mem) != 0 else pc + 3
        done = True
    if opr.opcode == OpCode.OpJz:
        pc = opr.snd.get(mem) if opr.fst.get(mem) == 0 else pc + 3
        done = True
    if opr.opcode == OpCode.OpInp:
        if not inp:
            print('program need input')
            assert False
        pc += 2
        mem[opr.fst.value] = inp[0]
        done = True
    if opr.opcode == OpCode.OpOut:
        pc += 2
        out = mem[opr.fst.value]
        done = True

    if not done:
        print(f'error run_step: pc: {pc} opr {opr} mem {mem[pc:pc+10]}')
        assert False

    return (inp[:1], out, pc, halt)


def run_mem(mem, inp):
    # print('---------------------run_mem-------------------')
    '''
    run entire memory
    '''
    nxt = 0
    inx = inp
    out = 0
    while True:
        inx, out, nxt, hlt = run_step(mem, out, inx, nxt)
        if hlt:
            break

    return (out, mem)


def tests():
    inp1 = [
        (0, [], "1,0,0,3,99"),
        (1, [], "1,9,10,3,2,3,11,0,99,30,40,50"),
        (2, [], "1,0,0,0,99"),
        (3, [], "2,3,0,3,99"),
        (4, [], "2,4,4,5,99,0"),
        (5, [], "1,1,1,4,99,5,6,0,99"),
        (6, [0], "3,0,4,0,99"),
        (7, [1], "3,0,4,0,99"),
        (8, [2], "3,0,4,0,99"),
        (9, [], "1002,4,3,4,33"),
        (10, [], "1101,100,-1,4,0"),
        (11, [0], "3,9,8,9,10,9,4,9,99,-1,8"),
        (12, [8], "3,9,8,9,10,9,4,9,99,-1,8"),
        (13, [7], "3,9,7,9,10,9,4,9,99,-1,8"),
        (14, [8], "3,9,7,9,10,9,4,9,99,-1,8"),
        (15, [0], "3,3,1108,-1,8,3,4,3,99"),
        (16, [8], "3,3,1108,-1,8,3,4,3,99"),
        (17, [0], "3,3,1107,-1,8,3,4,3,99"),
        (18, [8], "3,3,1107,-1,8,3,4,3,99"),
        (19, [0], "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"),
        (20, [2], "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"),
        (21, [0], "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"),
        (22, [2], "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"),
    ]

    out1 = [
        (0, 0, "1,0,0,2,99"),
        (1, 0, "3500,9,10,70,2,3,11,0,99,30,40,50"),
        (2, 0, "2,0,0,0,99"),
        (3, 0, "2,3,0,6,99"),
        (4, 0, "2,4,4,5,99,9801"),
        (5, 0, "30,1,1,4,2,5,6,0,99"),
        (6, 0, "0,0,4,0,99"),
        (7, 1, "1,0,4,0,99"),
        (8, 2, "2,0,4,0,99"),
        (9, 0, "1002,4,3,4,99"),
        (10, 0, "1101,100,-1,4,99"),
        (11, 0, "3,9,8,9,10,9,4,9,99,0,8"),
        (12, 1, "3,9,8,9,10,9,4,9,99,1,8"),
        (13, 1, "3,9,7,9,10,9,4,9,99,1,8"),
        (14, 0, "3,9,7,9,10,9,4,9,99,0,8"),
        (15, 0, "3,3,1108,0,8,3,4,3,99"),
        (16, 1, "3,3,1108,1,8,3,4,3,99"),
        (17, 1, "3,3,1107,1,8,3,4,3,99"),
        (18, 0, "3,3,1107,0,8,3,4,3,99"),
        (19, 0, "3,12,6,12,15,1,13,14,13,4,13,99,0,0,1,9"),
        (20, 1, "3,12,6,12,15,1,13,14,13,4,13,99,2,1,1,9"),
        (21, 0, "3,3,1105,0,9,1101,0,0,12,4,12,99,0"),
        (22, 1, "3,3,1105,2,9,1101,0,0,12,4,12,99,1"),
    ]

    def test(inp, out):
        run_out, mem_out = run_mem(read_mem(inp[2]), inp[1])
        cout = (run_out == out[1])            # output
        cmem = (mem_out == read_mem(out[2]))  # memory
        if not (cout and cmem):
            print(f'------------- Test #{inp[0]} Failed ------------------')
            print(f'input:  {inp[1:]}')
            print(f'expect: {out[1],out[2]}')
            print(f'output: {run_out,",".join([str(i) for i in mem_out])}')
            print('-----------------------------------------------')
            assert False

    for case in zip(inp1, out1):
        _ = test(*case)


def run_program(p, inp):
    mem = read_mem(p)
    out = run_mem(mem, inp)[0]
    return out


def part_1(prog):
    out = run_program(prog, [1])
    print(out)


def part_2(prog):
    out = run_program(prog, [5])
    print(out)


def main():
    tests()

    prog = input()

    part_1(prog)

    part_2(prog)


if __name__ == '__main__':
    main()
