class Ivals():

  __slots__ = ("lams", "kappas", "rs", "cps", "tflash", "nlayer")

  def __init__(self):
    self.lams = {}
    self.kappas = {}
    self.rs = {}
    self.cps = {}


ivals = Ivals()

fname="common.in"

with open(fname) as f1:
  src = f1.readlines()
  readPtr = 0
  ivals.nlayer = int(src[readPtr])
  readPtr = readPtr + 1
  ivals.tflash = 1e-3 * float(src[readPtr])
  readPtr = readPtr + 1
  for i in range(0, 3):
      ivals.cps[i] = float(src[readPtr + i])
      ivals.rs[i + 3] = 1e3 * float(src[readPtr + i + 3])
  readPtr = readPtr + 6
  ivals.kappas[0] = 1e-2 * float(src[readPtr + 0])
  for i in range(1, 3):
      ivals.kappas[i] = 1e-3 * float(src[readPtr + i])
  readPtr = readPtr + 3
  for i in range(0, ivals.nlayer):
      ivals.lams[i] = 1e3 * float(src[readPtr + i])

