# RSA-cryptosystem

There is a first version of an implementation of RSA-cryptosystem in haskell. 

# Instructions
There are 3 files for this program. To run each of them, firstly run :l name_of_file.hs. 
# key-gen.hs 
When you call key-gen method from it you will see the message that key is generated, and after it is generated, you will see a corresponding message. For the creation of keys, I take prime numbers in a range (2^100, 2^112), which are pretty big. I could take a bigger interval, but then key generation would take a lot of time.
# rsa-encrypt.hs
Call encrypt method. Then you can print the message which you want to be encrypted as one line. In the stdout you will see a list of integers, i.e. [1234] - a corresponding encryption of the text.
# rsa-decrypt.hs 
To decrypt you call a method 'decrypt'. In stdin you print a list of integers which you got after encryption. The result is the original message in stdout.
