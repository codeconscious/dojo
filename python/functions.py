class ExponentCalculator:
    def __init__(self, base): # ctor
        self.base = base

    def exponents(self, exponent):
        result = self.base
        for n in range(1, exponent):
            result *= self.base
        print("%s to the %sth power is %s" % (self.base, exponent, result))

    @staticmethod
    def exponentsWithBase(customBase, exponent):
        result = customBase
        for n in range(1, exponent):
            result *= customBase
        print("%s to the %sth power is %s" % (customBase, exponent, result))

# calcFive = ExponentCalculator(5)
# calcFive.exponents(3)
# calcFive.exponentsWithBase(2, 20)
