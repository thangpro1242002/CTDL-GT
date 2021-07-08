a = int(input())
b = int(input())

def gcd(a,b):
# hien thi list nhung so chia het cua a
    answera = []
    for i in range(1, a + 1):
        if a % i == 0:
            answera.append(i)
# hien thi list nhung so chia het cua b
    answerb = []
    for v in range(1, b + 1):
        if b % v == 0:
            answerb.append(v)
# hien thi list nhung so chung chia het cua a va b
    answert = []
    for e in answera:
        if e in answerb:
            answert.append(e)
# tim so lon nhat trong list
    max_value = answert[0]
    for f in answert:
        if f > max_value:
            max_value = f
    return max_value

print(gcd(a,b))