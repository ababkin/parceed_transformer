# The \s are just to multiline the command and make it more readable
ssh \
-L 4321:localhost:27017 \
-i ~/parceed-ssh.pem \
ubuntu@ec2-54-86-9-206.compute-1.amazonaws.com
