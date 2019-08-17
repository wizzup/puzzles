# problem origin:
# https://www.facebook.com/groups/admin.py.dev/permalink/1134011630117372/

# หอยทากตัวหนึ่ง ตกลงไปในบ่อที่มี ความสูง H เมตร มันพยายามจะไต่ให้
# ถึงปากบ่อ ในเวลากลางวันหอยทากไต่ขึ้น ไปได้ U เมตร เวลากลางคืนมันนอน
# หลับจึงไม่ได้ไต่แต่กลับไถลลงมาเป็น ระยะทาง D เมตร ให้เขียนโปรแกรม
# เพื่อหาว่าหอยทากจะใช้เวลากี่วันในการ ไต่ออกจากบ่อ (กำหนดให้หอยทากเริ่ม
# ไต่ในเวลากลางวัน) #ให้ผู้ใช้กรอกค่า H, U, D เอง

from enum import Enum
from time import sleep
from math import ceil, floor


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


class Time(Enum):
    DAY = 0,
    NIGHT = 1


class WorldState:
    def __init__(self, well_height):
        self.time = Time.DAY
        self.snail_pos = 0
        self.well_height = well_height
        self.screen_height = 21        # screen height (in num of lines)

    def __repr__(self):
        wlh = self.well_height
        sch = self.screen_height
        snp = self.snail_pos
        tme = self.time
        step = wlh / (sch - 1)
        s_n = range(sch)[::-1]
        h_n = [x * step for x in s_n]

        ret = '🌞\n' if tme == Time.DAY else '🌛\n'
        ret += f"{9*'_'}{'_@_' if snp > wlh else '___'}\n"
        for (h, l) in zip(h_n, s_n):
            h_s = f'{h:>10.1f}' if not l % 4 else 10 * ' '
            n_s = f'@ {snp:.1f}' if h - step < snp <= h else ' '
            ret += f'{h_s} |{n_s}\n'

        ret += f'{25*"_"}\n'
        return ret

    def get_snail_pos(self):
        return self.snail_pos

    def move_snail(self, p):
        self.snail_pos = p

    def set_time(self, t):
        self.time = t

    def get_time(self):
        return self.time

    def is_in(self):
        return self.snail_pos <= self.well_height


clear_screen()
H = float(input('H = '))
U = float(input('U = '))
D = float(input('D = '))
print()
print(f'H:{H}, U:{U}, D:{D}')
START = WorldState(H)
print(START)
print()
sleep(1)

count = 0
while START.is_in():
    clear_screen()

    tme = START.get_time()
    snp = START.get_snail_pos()

    if tme == Time.DAY:
        START.move_snail(snp + U)
        START.set_time(Time.NIGHT)

    if tme == Time.NIGHT:
        START.move_snail(snp - D)
        START.set_time(Time.DAY)

    print(START)
    count += 1
    print(f'{count / 2} days')
    sleep(0.5)
