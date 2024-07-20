with open('mips32.txt') as f:
    lines = list(f.readlines())
    header, body = lines[0], lines[1:]
    titles = list(header.split())
    table = dict()
    for line in body:
        items = line.split()
        table[items[0]] = {k: v for k, v in zip(titles[1:], items[1:])}

    print('val decodingMap = Map(')
    for inst, ctrls in table.items():
        print(f'  M"{ctrls["pattern"]}" -> InstType.{inst},')
    print(')')

    for inst, ctrls in table.items():
        print(f"""InstType.{inst} -> InstDecoderInfo1(
        AluOp.{ctrls["AluOp"]},
        AluBSrc.{ctrls["AluBSrc"]},
        ShiftOp.{ctrls["ShiftOp"]},
        ShiftSrc.{ctrls["ShiftSrc"]},
        WriteBackEn.{ctrls["WriteBackEn"]},
        WriteBackRegSrc.{ctrls["WriteBackRegSrc"]},
        WriteBackDataSrc.{ctrls["WriteBackDataSrc"]},
        ImmExtType.{ctrls["ImmExtType"]},
        PcSrc.{ctrls["PcSrc"]}),""")
