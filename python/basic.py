# Syntax cheatsheet

greeting = 'Hello 世界'
print(greeting)
print(greeting[0:5]) # The colon can be omitted
print(greeting[0::2]) # Third number is the step
print(greeting[::-1]) # Reverse the order
print((greeting + " ") * 3) # In triplicate

words = greeting.split(" ")
print("There are %d words in '%s'" % (len(words), greeting))

if words[0] == 'Hello' and words[1] == '世界': # Use "and"
    print("Expected string found")
elif words[0] == "Goodbye":
    print("Error: Unexpected string")
else:
    print("Failure!")

if "世界" in words: # Contains
    print("Found!")
else:
    print("Sorry, not found...")

myList = [10 ** 3, 20, 30] # Two asterisks represents powers, so that's 10^3
myList.append(1)
myList.append(2)
myList.append(3)
myOtherList = [-3, 4] * 10 # You can repeat list contents too.
finalList = myList + myOtherList # Combining lists

sum = 0
for x in finalList:
    sum += x
avg = sum / len(finalList)
print("The total is %d, and the average is %f." % (sum, avg))

for n in range(5, 10):
    print(n) # Prints 5 to 9

# While loops can have an else statement.
count = 0
while (count < 4): # Prints 0 to 3
    print(count)
    count += 1
else: # Note: This is not hit if "break" is used
    print("Counting complete!")


# Object equality check
if myList is finalList:
    print("One and them same instance")
else:
    print("Not the same instance")


# Import style 1
import functions
calcTen = functions.ExponentCalculator(10)
calcTen.exponents(3)
calcTen.exponentsWithBase(2, 20)

# Import style 2
from functions import ExponentCalculator # Or: *
calc20 = ExponentCalculator(20)
calc20.exponents(4)

# Fun fact: Imports can be set via conditionals too
# if visual_mode:
#     import draw_visual as draw
# else:
#     import draw_textual as draw


# Lambdas
sum = lambda x,y : x * y
print(sum(3, 4))


# Params (as in C#)
def paramsTest(first, second, third, *therest):
    print("First: %s" %(first))
    print("Second: %s" %(second))
    print("Third: %s" %(third))
    print("And all the rest... %s" %(list(therest)))

paramsTest(1, 2, 3, 4, 5, 10, 100)


# Send args by keyword
def math_operation(first, second, third, **options):
    if options.get("action") == "sum":
        print("The sum is: %d" %(first + second + third))
    if options.get("number") == "first":
        return first
result = math_operation(1, 2, 3, action = "sum", number = "first")
print("Result: %d" %(result))
