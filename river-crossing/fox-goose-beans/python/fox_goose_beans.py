"""
Fox, goose and bag of beans puzzle
https://en.wikipedia.org/wiki/Fox,_goose_and_bag_of_beans_puzzle

Code origin : https://github.com/wizzup/river-crossing-puzzle
MIT License
"""

from enum import Enum
from sys import stderr, stdin
from tty import setcbreak
from collections import namedtuple


class BL(Enum):
    """
    Boat location
    """
    LEFT = 0
    RIGHT = 1


GameState = namedtuple('GameState',
                       'left_side, on_boat, boat_loc, right_side')


def gamestate_str(g_s):
    """
    GameState string representation
    """
    def boatstr():
        l_b = len(g_s.on_boat)
        l_s = ''.join(g_s.on_boat)
        if l_b == 0:
            b_s = '....'
        elif l_b == 1:
            b_s = f'..{l_s}'
        else:
            b_s = l_s

        if g_s.boat_loc == BL.LEFT:
            ret = f'<{b_s}>======'
        else:
            ret = f'======<{b_s}>'
        return ret

    x_ls = [g_s.left_side, g_s.right_side]
    [l_ls, r_ls] = [''.join(i) for i in x_ls]
    pad = ' ' * (10 - 2 * len(l_ls))
    b_s = boatstr()

    return f'{pad}{l_ls} {b_s} {r_ls}'


def p_gs(g_s, txt=''):
    """
    print GameState
    """
    pads = (8 - 2 * len(g_s.right_side)) * ' '
    print(f'{gamestate_str(g_s)} {pads} {txt}')


def cross_river(g_s):
    """
    return new state with boat on the other side of the river
    """
    b_loc = BL.LEFT if g_s.boat_loc == BL.RIGHT else BL.RIGHT

    l_ls = g_s.left_side.copy()
    b_ls = g_s.on_boat.copy()
    r_ls = g_s.right_side.copy()

    return GameState(l_ls, b_ls,
                     b_loc, r_ls)


def load(g_s, entity):
    """
    return new state with entity move on a boat (with human)
    """
    b_ls = g_s.on_boat.copy()
    if len(b_ls) >= 2:
        print('boat overloaded', file=stderr)
        print(f" can't load {entity}\n on {gamestate_str(g_s)}",
              file=stderr)
        return g_s

    l_ls = g_s.left_side.copy()
    r_ls = g_s.right_side.copy()

    if entity in g_s.left_side and g_s.boat_loc == BL.LEFT:
        l_ls.remove(entity)
        b_ls.add(entity)
    elif entity in g_s.right_side and g_s.boat_loc == BL.RIGHT:
        r_ls.remove(entity)
        b_ls.add(entity)
    else:
        print(f'{entity} not near by the boat', file=stderr)
        print(f" can't load {entity}\n on {gamestate_str(g_s)}",
              file=stderr)
        return g_s

    return GameState(l_ls, b_ls, g_s.boat_loc, r_ls)


def unload(g_s, entity):
    """
    return new state with entity move on a boat (with human)
    on error return non-modify state
    """
    if entity not in g_s.on_boat:
        print(f'{entity} not on boat', file=stderr)
        print(f" can't unload {entity}\n on {gamestate_str(g_s)}",
              file=stderr)
        return g_s

    b_ls = g_s.on_boat.copy()
    b_ls.remove(entity)

    l_ls = g_s.left_side.copy()
    r_ls = g_s.right_side.copy()

    if g_s.boat_loc == BL.LEFT:
        l_ls.add(entity)
    else:
        r_ls.add(entity)

    return GameState(l_ls, b_ls, g_s.boat_loc, r_ls)


def is_bad(g_s):
    """
    Is it bad state?
    Bad state when
        - FOX with GOOSE without HUMAN
        - GOOSE with BEANS without HUMAN
    """
    f_g = {FOX, GOOSE}
    g_b = {GOOSE, BEANS}

    l_ls = g_s.left_side
    r_ls = g_s.right_side

    ret = False
    if g_s.boat_loc == BL.RIGHT:
        ret = f_g.issubset(l_ls) or g_b.issubset(l_ls)
    else:
        ret = f_g.issubset(r_ls) or g_b.issubset(r_ls)

    return ret


def is_good(g_s):
    """
    Is it good state?
    """
    return not is_bad(g_s)


FOX = '🐺'
GOOSE = '🐦'
BEANS = '👝'
HUMAN = '🚹'

EVERYONE = list({FOX, GOOSE, BEANS, HUMAN})
print(EVERYONE)

PREINIT_STATE = GameState(EVERYONE, set(), BL.LEFT, set())
p_gs(PREINIT_STATE, 'preinit')

INIT_STATE = GameState({FOX, GOOSE, BEANS}, {HUMAN}, BL.LEFT, set())
p_gs(INIT_STATE, 'init')

FINAL_STATE = GameState(set(), {HUMAN}, BL.RIGHT, {FOX, GOOSE, BEANS})
p_gs(FINAL_STATE, 'final')

POSTFINAL_STATE = GameState(set(), set(), BL.RIGHT, EVERYONE)
p_gs(POSTFINAL_STATE, 'postf')


def help_message():
    """
    Instruction
    """
    p_gs(PREINIT_STATE)
    _ = [print(' ' * 17 + '⌄') for _ in range(3)]
    p_gs(POSTFINAL_STATE)
    print()

    hlp = ["Instruction:",
           "",
           "<SPACE> for taking <..> (the boat) crossing the river",
           f"<j> for load/unload {FOX}(Fox)",
           f"<k> for load/unload {GOOSE}(Goose)",
           f"<l> for load/unload {BEANS}(Beans)",
           "<q> for quit the game (giving up)",
           "",
           "press anykey to close this help"]

    print('\n'.join(hlp))
    stdin.read(1)


def toggle(g_s, ent):
    """
    Toggle load/unload entity
    """
    inl = ent in g_s.left_side and g_s.boat_loc == BL.LEFT
    inr = ent in g_s.right_side and g_s.boat_loc == BL.RIGHT

    ret = g_s
    if inl or inr:
        ret = load(g_s, ent)
    elif ent in g_s.on_boat:
        ret = unload(g_s, ent)

    return ret


def esc(code):
    """
    vt100 escape code
    """
    return f'\u001b{code}'


def clear_screen():
    """
    clear terminal (vt100)
    """
    code = ''.join([esc(x) for x in ["[2J", "[2H"]])
    print(code, end='')


def ask_yn(prompt):
    """
    ask a yes/no prompt
    """
    print(f'{prompt} (y/n)')
    k = stdin.read(1).lower()
    return k == 'y'


def loop(g_s, num):
    """
    game loop
    """
    clear_screen()
    is_g = is_good(g_s)
    s_st = "OK" if is_g else "FAIL"
    p_gs(g_s, f'{s_st} {num}')

    if not is_g:
        print("Well, you have failed")
        _ = main() if ask_yn('Play again?') else exit()

    if g_s == FINAL_STATE:
        print('Well Done')
        print(f'You have crossed the river {num} times')
        _ = main() if ask_yn('Play again?') else exit()

    while True:
        k = stdin.read(1).lower()
        if k == 'q':
            exit()
        elif k == 'j':
            loop(toggle(g_s, FOX), num)
        elif k == 'k':
            loop(toggle(g_s, GOOSE), num)
        elif k == 'l':
            loop(toggle(g_s, BEANS), num)
        elif k == ' ':
            loop(cross_river(g_s), num+1)
        else:
            help_message()
            loop(g_s, num)


def main():
    """
    Program entry point
    """
    setcbreak(stdin)

    clear_screen()
    help_message()

    loop(INIT_STATE, 0)


if __name__ == '__main__':
    main()
