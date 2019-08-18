"""
snail climbing
"""

from enum import Enum
from time import sleep
from collections import namedtuple

# constants
DAY_LENGTH = 1  # how fast (half) a day in sec
SCREEN_HEIGHT = 21  # screen height (in num of lines)


def clear_screen():
    """
    clear terminal (vt100)
    """
    def esc(code):
        return f'\u001b[{code}'

    code = ''.join([esc(x) for x in ["2J", "2H"]])
    print(code, end='')


class Time(Enum):
    """
    Enum for day/night checking
    """
    DAY = 0
    NIGHT = 1


def draw_world(world, footnote=''):
    """
    render world to picture on terminal
    """
    clear_screen()
    wlh = world.well_height
    snp = world.snail_pos
    tme = world.time
    step = wlh / (SCREEN_HEIGHT - 1)
    s_ns = range(SCREEN_HEIGHT)[::-1]
    h_ns = [x * step for x in s_ns]

    ret = 20*' ' + ('🌞\n' if tme == Time.DAY else '🌛\n')
    ret += f"{9*'_'}{'_@_' if snp > wlh else '___'}\n"
    for (wht, lne) in zip(h_ns, s_ns):
        hls = f'{wht:>10.1f}' if not lne % 4 else 10 * ' '
        sns = '@ ' if tme == Time.DAY else '@ᙆ'
        shn = f'{sns} {snp:.1f}' if wht - step < snp <= wht else ' '
        ret += f'{hls} |{shn}\n'

    ret += f'{25*"_"}\n'
    print(ret)
    print(footnote)


def move_snail(world, amount):
    """
    return new world with snail_pos change
    negative amount means move down
    ignoring over/under the well amount
    """
    pos = world.snail_pos
    next_world = None

    if 0 <= (pos + amount) <= world.well_height:
        next_world = world._replace(snail_pos=world.snail_pos + amount)

    if (pos + amount) < 0:
        next_world = world._replace(snail_pos=0)

    if (pos + amount) > world.well_height:
        next_world = world._replace(snail_pos=world.well_height + 1)

    return next_world


def set_time(world, time):
    """
    set DAY/NIGHT
    """
    next_world = world._replace(time=time)
    return next_world


def is_in(world):
    """
    check if snail in the well
    """
    return world.snail_pos <= world.well_height


def move_animate(world, amount, footnote):
    """
    move and animate the moving
    """
    pos = world.snail_pos

    # no reason to move snail back in the well
    if pos >= world.well_height:
        return world

    for i in range(abs(round(amount)) + 1):
        dist = (pos + i) if amount > 0 else (pos - i)
        draw_world(world._replace(snail_pos=dist), footnote)
        sleep(DAY_LENGTH / abs(amount))

    next_world = move_snail(world, amount)
    return next_world


def sim(height, upp, down):
    """
    start simulation until snail is out of the well
    """
    World = namedtuple("World", "time,snail_pos,well_height")
    init_world = World(time=Time.DAY, snail_pos=0, well_height=height)
    draw_world(init_world)

    next_world = init_world
    day_count = 0
    dist_c = 0
    dist_f = 0
    while is_in(next_world):
        footnote = (f'day {day_count}\n'
                    + f'distances:\n'
                    + f'  climb {dist_c:.01f}\n'
                    + f'  fall  {dist_f:.01f}')
        dist_c += upp
        dist_f += down

        next_world = move_animate(next_world, upp, footnote)
        next_world = set_time(next_world, Time.NIGHT)

        next_world = move_animate(next_world, -down, footnote)
        next_world = set_time(next_world, Time.DAY)

        day_count += 1


def get_input():
    """
    get parameters from stdin
    """
    clear_screen()
    pmt_s = [f'enter {x} > ' for x in "HUD"]
    [h_in, u_in, d_in] = [float(input(p)) for p in pmt_s]
    print()
    print(f'H:{h_in}, U:{u_in}, D:{d_in}')
    return (h_in, u_in, d_in)


def main():
    """
    script entry point
    """
    height, up_val, down_val = get_input()
    sim(height, up_val, down_val)


if __name__ == '__main__':
    main()
