# ZCORE-003

I started writing processor cores since I entered my graduate school. My first
core is a simple Nand2Tetris core, following the famous course with the same
name, using a specific (gate-level) hdl. The design of the core is extremely
simple, but with typical components. My second core is a RISC-V core, following
the famous course CS61C from UCB, using logisim to describe the circuit. A
single cycle version and a two-stage ((IF) + (ID, EX, MEM, WR)) piplined version
is implemented. In the piplined version, only a control hazard is need to be
handled. Well, although these two cores are naive, they are interesting for me.

I decided to write more complex and realistic processor cores. So this
repository is my third core, a MIPS32 core from labs in a CA course at UCAS,
China. I reused their spec and test suite, but write with SpinalHDL. The course
have a progressive list of milestones, from single cycle to 5-stage pipline to
decoupled interfaces and perfs and caches and SoC and so on. This might be a
good project to prepare me for more complex out-of-order or multi-issue cores.

