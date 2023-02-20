a = 0
b = 0

A = {
    'a1': 'a_tmp_x, a_tmp_y = a, b',
    'a2': 'a_tmp_x = 1',
    'a3': 'a, b = a_tmp_x, a_tmp_y'
}

B = {
    'b1': 'b_tmp_x, b_tmp_y = a, b',
    'b2': 'b_tmp_y = 1',
    'b3': 'a, b = b_tmp_x, b_tmp_y'
}

final = "print('a =', a ,' b =', b)"

instructions = list(input().split())

for pc in instructions:
    print(pc)
    if pc[0] == 'a':
        exec(A[pc])
        exec("print('a_tmp_x =',a_tmp_x, ' a_tmp_y =', a_tmp_y)")
    else:
        exec(B[pc])
        exec("print('b_tmp_x =',b_tmp_x, ' b_tmp_y =', b_tmp_y)")
    exec("print('a =', a, ' b =', b)")
    print()

exec(final)