import random

# Return 10 random numbers
def lottery():
    for i in range(9):
        yield random.randint(1, 40)
    yield random.randint(-20, -1)

lottery_output = ""
for random_number in lottery():
    lottery_output += ("%d  " % (random_number))
print(lottery_output)


# Print 75 letters, separating them into groups of 5 for legibility.
def letter_generator(count):
    letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    for i in range(count):
        yield letters[random.randint(0, 25)]

randomLetters = ""
for index, letter in enumerate(letter_generator(75)): # Use enumerate() to get the index
    randomLetters += letter
    if (index % 5 == 4):
        randomLetters += " "
print(randomLetters)
