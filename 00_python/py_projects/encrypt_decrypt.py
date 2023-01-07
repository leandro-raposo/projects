from tkinter import messagebox, simpledialog, Tk

'''
Now let’s see how to create a GUI application to encrypt and decrypt using Python.
Here we need to write some code that uses an infinite loop that will keep asking the
user if they want to encrypt or decrypt a message.
According to user input, we need to write an event program because the operation of 
the program depends on user input. Here we can use the dialogue box to get user input
and the info box to show the encrypted and decrypted message to the user.
As stated before, I will be using an infinite loop, so the program will keep running
until the user wants to encrypt and decrypt using Python. The program will end at the
point where the user gives input other than “encrypt” and “decrypt”.
'''

# the encrypt algorithm
def is_even(number):
    return number % 2 == 0

def get_even_letters(message):
    even_letters = []
    for counter in range(0, len(message)):
        if is_even(counter):
            even_letters.append(message[counter])
    return even_letters

def get_odd_letters(message):
    odd_letters = []
    for counter in range(0, len(message)):
        if not is_even(counter):
            odd_letters.append(message[counter])
    return odd_letters

def swap_letters(message):
    letter_list = []
    if not is_even(len(message)):
        message = message + 'x'
    even_letters = get_even_letters(message)
    odd_letters = get_odd_letters(message)
    for counter in range(0, int(len(message)/2)):
        letter_list.append(odd_letters[counter])
        letter_list.append(even_letters[counter])
    new_message = ''.join(letter_list)
    return new_message

def get_task():
    task = simpledialog.askstring('Task', 'Do you want to (1) encrypt or (2) decrypt?')
    return task

# function to get the msg from user
def get_message():
    message = simpledialog.askstring('Message', 'Enter the secret message: ')
    return message

root = Tk()
while True:
    task = get_task()
    if task == '1':
        message = get_message()
        encrypted = swap_letters(message)
        messagebox.showinfo('Secret message is:', encrypted)
        
    elif task == '2':
        message = get_message()
        decrypted = swap_letters(message)
        messagebox.showinfo('The original secret message is:', decrypted)
    else:
        break


root.mainloop()
