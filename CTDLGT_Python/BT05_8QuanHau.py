from itertools import permutations

n=8
GPT=0 #Để chỉ giải pháp lần thứ bao nhiêu
cols = range(n)
for combo in permutations(cols):
    if n==len(set(combo[i]+i for i in cols))==len(set(combo[i]-i for i in cols)):
        GPT += 1
        print('Giải pháp '+str(GPT)+': '+str(combo)+'\n')
        print("\n".join(' o ' * i + ' X ' + ' o ' * (n-i-1) for i in combo) + "\n\n\n\n")