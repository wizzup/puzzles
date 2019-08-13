from itertools import permutations, combinations


class Rule:
    def __init__(self,xs,ms,ys,os):
        self.xs = xs
        self.ms = ms
        self.ys = ys
        self.os = os

    def __repr__(self):
        return f'{self.xs} {self.ms} {self.ys} = {self.os}'

    def check(self,d):
        """
            A B C B  4 2 7 2
            B D C B  2 8 7 2
            C E A A  7 1 4 4

            >>> a = {'A': 4, 'B': 2, 'C': 7, 'D': 8, 'E': 1}
            >>> r = Rule("ABCB","+","BDCB","CEAA")
            >>> r.check(a)
            True
        """
        def l2s(d,l):
            return ''.join([str(d[i]) for i in l])

        x = int(l2s(d,self.xs))
        y = int(l2s(d,self.ys))
        o = int(l2s(d,self.os))

        if ms == "+":
            return x + y == o
        elif ms == "-":
            return x - y == o



digits = {1,2,3,4,5,6,7,8,9,0}

if __name__ == '__main__':
    while True:
        print('Enter rule set')
        xs = input('first  num         > ').strip()
        ms = input('operator           > ').strip()
        ys = input('second num         > ').strip()
        os = input('output             > ').strip()
        r = Rule(xs,ms,ys,os)
        print(r)

        symbols = {x for x in xs + ys + os}
        print(f"{len(symbols)} symbols : {''.join(sorted(symbols))}")

        possibles = [dict(zip(ss, ds)) for ss in permutations(symbols) for ds in combinations(digits, len(symbols))]
        print(f"{len(possibles)} possibles symbols x digits combinations")

        valids = [x for x in possibles if r.check(x)]
        print(f"{len(valids)} valid unconstrained solutions:")
        # for x in valids:
        #     print({i:x[i] for i in sorted(x)})

        def get_constrain(s):
            (s,n) = s.split('=')
            return (s,int(n))

        ns = int(input('num of constrains  > ').strip())
        cs = [get_constrain(input()) for _ in range(ns)]
        print(f'{len(cs)} constrains :  {cs}')

        stricts = []
        for i in valids:
            ps = []
            for j in cs:
                (s,n) = j
                if i[s] == n:
                    ps.append(True)
                else:
                    ps.append(False)

            if all(ps):
                stricts.append(i)

        def d2s(d):
            return ''.join([str(x[i]) for i in d])

        def print_node(d):
            [x,y,o] = [d2s(i) for i in [xs,ys,os]]
            print(f"""
            {x:>5} {ms}
            {y:>5}
            {o:>5}
            """)

        print(f"{len(stricts)} valid constrained solutions:")
        for x in stricts:
            print({i:x[i] for i in sorted(x)})
            print_node(x)
